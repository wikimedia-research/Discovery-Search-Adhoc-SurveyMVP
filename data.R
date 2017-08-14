if (!grepl("^stat1", Sys.info()["nodename"])) {
  message("Creating an auto-closing SSH tunnel in the background...")
  # See https://gist.github.com/scy/6781836 for more info.
  system("ssh -f -o ExitOnForwardFailure=yes stat1003.eqiad.wmnet -L 3307:analytics-store.eqiad.wmnet:3306 sleep 10")
  library(RMySQL)
  con <- dbConnect(MySQL(), host = "127.0.0.1", group = "client", dbname = "log", port = 3307)
} else {
  con <- wmf::mysql_connect("log")
}

library(magrittr)
library(glue)

# First table (17069968) has the 0 time delay before asking.
# Second table (17073843) has the 60s delay before asking
#   and the addition of recording the 'unsure' responses.
revision <- "17069968"
start_date <- as.Date("2017-08-03") + 1
end_date <- as.Date("2017-08-11") - 1

query <- "SELECT
  session_id,
  ts,
  pages.page_title AS article,
  choice,
  query,
  question
FROM (
  SELECT
    event_mwSessionId AS session_id,
    timestamp AS ts,
    event_articleId AS article_id,
    event_choice AS choice,
    event_query AS query,
    CASE event_question
      WHEN 'wikimediaevents-humanrel-question-a' THEN \"Would you click on this page when searching for '...'?\"
      WHEN 'wikimediaevents-humanrel-question-b' THEN \"If you searched for '...', would this article be a good result?\"
      WHEN 'wikimediaevents-humanrel-question-c' THEN \"If you searched for '...', would this article be relevant?\"
      WHEN 'wikimediaevents-humanrel-question-d' THEN \"If someone searched for '...', would they want to read this article?\"
    END AS question
  FROM log.HumanSearchRelevance_{revision}
  WHERE LEFT(timestamp, 8) = '{yyyymmdd}'
    AND wiki = 'enwiki'
) AS events
LEFT JOIN enwiki.page AS pages
  ON events.article_id = pages.page_id;"

results <- do.call(rbind, lapply(
  seq(start_date, end_date, by = "day"),
  function(date) {
    message("Fetching data from ", format(date, "%d %B %Y"))
    yyyymmdd <- format(date, "%Y%m%d")
    query <- glue(query)
    result <- wmf::mysql_read(query, "log", con)
    result$ts <- lubridate::ymd_hms(result$ts)
    return(result)
  }
))

wmf::mysql_close(con)

if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

results$article %<>% gsub("_", " ", ., fixed = TRUE)
readr::write_tsv(results, file.path("data", glue("{revision}.tsv")))
