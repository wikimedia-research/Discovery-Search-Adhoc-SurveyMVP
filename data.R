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
# revision <- "17069968"
# start_date <- as.Date("2017-08-03") + 1
# end_date <- as.Date("2017-08-11") - 1
revision <- "17073843"
start_date <- as.Date("2017-08-10") + 1
end_date <- as.Date("2017-08-19") - 1

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

trey <- readr::read_delim("query | opinion | article
who is v for vendetta?| ok | V for Vendetta (film)
who is v for vendetta?| ok | V for Vendetta
who is v for vendetta?| good | List of V for Vendetta characters
who is v for vendetta?| best | V (comics)
who is v for vendetta?| bad | Vendetta Pro Wrestling
star and stripes|ok | Stars and Stripes Forever (disambiguation)
star and stripes|bad | The White Stripes
star and stripes|bad | Tars and Stripes
star and stripes|ok | The Stars and Stripes Forever
star and stripes|bad | Stripes (film)
block buster|best | Blockbuster
block buster|good | Block Buster!
block buster|bad | The Sweet (album)
block buster|ok | Block Busters
block buster|bad | Buster Keaton
10 items or fewer|ok | Fewer vs. less
10 items or fewer|very bad | 10-foot user interface
10 items or fewer|very bad | Magic item (Dungeons & Dragons)
10 items or fewer|very bad | Item-item collaborative filtering
10 items or fewer|very bad | Item 47
sailor soldier tinker spy|best | Tinker Tailor Soldier Spy
sailor soldier tinker spy|good | Tinker, Tailor
sailor soldier tinker spy|ok | Blanket of Secrecy
sailor soldier tinker spy|ok | List of fictional double agents
sailor soldier tinker spy|ok | Ian Bannen
how do flowers bloom?|bad | Britain in Bloom
how do flowers bloom?|very bad | Flowers in the Attic (1987 film)
how do flowers bloom?|best | Flower
how do flowers bloom?|very bad | Thymaridas
how do flowers bloom?|very bad | Flowers in the Attic
yesterday beetles|very bad | Private language argument
yesterday beetles|very bad | Diss (music)
yesterday beetles|very bad | How Do You Sleep? (John Lennon song)
yesterday beetles|very bad | Maria Mitchell Association
yesterday beetles|very bad | The Collected Stories of Philip K. Dick
search engine|best | Web search engine
search engine|good | List of search engines
search engine|ok | Search engine optimization
search engine|ok | Search engine marketing
search engine|ok | Audio search engine
what is a genius iq?|good | Genius
what is a genius iq?|best | IQ classification
what is a genius iq?|bad | Genius (website)
what is a genius iq?|ok | High IQ society
what is a genius iq?|bad | Social IQ score of bacteria
why is a baby goat a kid?|best | Goat
why is a baby goat a kid?|very bad | Super Why!
why is a baby goat a kid?|very bad | Barney & Friends
why is a baby goat a kid?|very bad | The Kids from Room 402
why is a baby goat a kid?|very bad | Oliver Hardy filmography
", delim = "|",  trim_ws = TRUE)
trey$opinion <- factor(trey$opinion, levels = c("very bad", "bad", "ok", "good", "best"))
trey$rating <- dplyr::case_when(
  trey$opinion == "very bad" ~ -2,
  trey$opinion == "bad" ~ -1,
  trey$opinion == "ok" ~ 0,
  trey$opinion == "good" ~ 1,
  trey$opinion == "best" ~ 2
)
readr::write_tsv(trey, file.path("data", "trey_opinion.tsv"))
