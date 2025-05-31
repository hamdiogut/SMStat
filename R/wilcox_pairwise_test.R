wilcox_pairwise_test <- function(response, group, p_adjust = "bonferroni") {
  if (!is.numeric(response)) stop("response sayısal olmalı.")
  if (!is.factor(group)) group <- as.factor(group)
  if (length(response) != length(group)) stop("Uzunluklar eşit olmalı.")
  if (length(unique(group)) < 2) stop("En az iki grup olmalı.")

  df <- data.frame(response = response, group = group)

  suppressMessages({
    library(rstatix)
    results <- df |>
      wilcox_test(response ~ group, p.adjust.method = p_adjust) |>
      mutate(across(where(is.numeric), ~formatC(.x, digits = 3, format = "f")))
  })

  results <- results[, c("group1", "group2", "statistic", "p", "p.adj")]
  names(results) <- c("Grup1", "Grup2", "W", "p", paste0("p-adj (", p_adjust, ")"))
  return(results)
}
