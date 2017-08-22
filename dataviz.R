# responses <- readr::read_tsv("data/17069968.tsv")
responses <- readr::read_tsv("data/17073843.tsv")
trey <- readr::read_tsv("data/trey_opinion.tsv")

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
  dplyr::mutate(
    total = yes + no + unsure,
    yes = yes / total,
    no = no / total,
    unsure = unsure / total,
    dismiss = dismiss / (total + dismiss),
    engaged = (total + dismiss) / (total + dismiss + timeout)
  ) %>%
  dplyr::select(-c(total, timeout)) %>%
  tidyr::gather(choice, prop, -c(query, article, question)) %>%
  dplyr::mutate(choice = factor(choice, levels = c("yes", "no", "unsure", "dismiss", "engaged")))

plots <- list()
for (query in unique(aggregates$query)) {
  injector <- function(string) {
    return(sub("...", query, string, fixed = TRUE))
  }
  plots[[query]] <- ggplot(
    aggregates[aggregates$query == query, ],
    aes(x = article, y = prop, fill = choice)
  ) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "white") +
    facet_wrap( ~ question, labeller = as_labeller(injector)) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual("Response", values = c(
      yes = "#377EB8",
      no = "#E41A1C",
      unsure = "#4DAF4A",
      dismiss = "#984EA3",
      engaged = "black"
    )) +
    coord_flip() +
    labs(
      title = glue("Query: '{query}'"),
      subtitle = "Yes/No/Unsure is out of non-dismissive responses; dismiss % is out of all engagements; engagement is inverse of time-outs",
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

responses_2 <- responses %>%
  dplyr::group_by(query, article, question, choice) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  tidyr::spread(choice, n, fill = 0) %>%
  dplyr::mutate(
    total = yes + no + unsure + 1,
    engaged = yes + no + unsure + dismiss
  ) %>%
  dplyr::left_join(trey, by = c("query", "article"))

p <- ggplot(responses_2, aes(x = rating, y = ((unsure / 2) + yes - no) / (unsure + yes + no + 1))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "black") +
  geom_jitter(height = 0, width = 0.1, aes(color = opinion), size = 2) +
  scale_color_brewer(palette = "Set1") +
  # geom_violin(aes(fill = opinion), alpha = 0.5, draw_quantiles = c(0.5)) +
  facet_wrap(~ question) +
  labs(
    x = "Trey's opinion of article's relevance",
    y = "Crowd score: (#unsure/2 + #yes - #no) / (#yes + #no + 1)",
    title = "Distribution of the crowd's responses compared to expert opinion",
    subtitle = "With simple linear regression fit overlaid"
  ) +
  wmf::theme_facet(12, "Helvetica")
ggsave(
  filename = "score_compare_v2.png", plot = p, path = "figures",
  width = 10, height = 8, dpi = 300, units = "in"
)

aggregates_2 <- responses_2 %>%
  dplyr::group_by(rating, question, opinion) %>%
  dplyr::summarize(engaged = sum(engaged), total = sum(total), timeout = sum(timeout)) %>%
  dplyr::ungroup() %>%
  cbind(., binom::binom.bayes(.$engaged, .$total + .$timeout, conf.level = 0.95)[, c("mean", "lower", "upper")])
p <- ggplot(responses_2, aes(x = rating, y = engaged / (total + timeout))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Set1") +
  geom_pointrange(data = aggregates_2, aes(y = mean, ymin = lower, ymax = upper, color = opinion)) +
  facet_wrap(~ question) +
  labs(
    x = "Trey's opinion of article's relevance",
    y = "Proportion of users",
    title = "Users' engagement with survey (not letting it time-out)",
    subtitle = "With simple linear regression fit and 95% credible intervals overlaid"
  ) +
  wmf::theme_facet(12, "Helvetica")
ggsave(
  filename = "engagement_opinion.png", plot = p, path = "figures",
  width = 10, height = 8, dpi = 300, units = "in"
)

# ggplot(responses_2, aes(x = (yes - no) / total, y = engaged / total)) +
#   geom_point() +
#   facet_wrap(~ question)
