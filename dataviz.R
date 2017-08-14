responses <- readr::read_tsv("data/17069968.tsv")

library(magrittr)
library(ggplot2)
library(glue)

if (!dir.exists("figures")) {
  dir.create("figures", recursive = TRUE)
}

aggregates <- responses %>%
  dplyr::group_by(query, article, question, choice) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  tidyr::spread(choice, n, fill = 0) %>%
  tidyr::gather(choice, n, -c(query, article, question)) %>%
  dplyr::mutate(choice = factor(choice, levels = c("yes", "no", "dismiss", "timeout")))

plots <- list()
for (query in unique(aggregates$query)) {
  injector <- function(string) {
    return(sub("...", query, string, fixed = TRUE))
  }
  plots[[query]] <- ggplot(
    aggregates[aggregates$query == query, ],
    aes(x = article, y = n, fill = choice)
  ) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +
    facet_wrap( ~ question, labeller = as_labeller(injector)) +
    scale_fill_manual("User's response", values = c(
      yes = "#377EB8",
      no = "#E41A1C",
      dismiss = "#984EA3",
      timeout = "#A65628"
    )) +
    coord_flip() +
    labs(
      title = glue("Query: '{query}'"),
      x = "Article", y = "Number of responses"
    ) +
    wmf::theme_facet(12, "Helvetica")
  ggsave(
    filename = paste0(gsub(".", "_", make.names(query), fixed = TRUE), ".png"),
    plot = plots[[query]], path = "figures",
    width = 16, height = 8, dpi = 300, units = "in"
  )
}
# cowplot::plot_grid(plotlist = plots, ncol = 1)
