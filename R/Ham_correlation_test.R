Ham_correlation_test <- function(x, y, conf.level = 0.95, alpha = 0.05, desired_power = 0.8) {
  library(dplyr)
  library(dplyr)
  library(shiny)
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y))

  if (!requireNamespace("pwr", quietly = TRUE)) {
    stop("Lütfen önce 'pwr' paketini yükleyin: install.packages('pwr')")
  }

  n <- length(x)

  # Pearson korelasyonu
  pearson_res <- cor.test(x, y, method = "pearson", conf.level = conf.level)
  r_val <- pearson_res$estimate
  r_comment <- get_correlation_strength(r_val)
  pearson_color <- if (pearson_res$p.value < alpha) "red" else "black"

  # Power hesaplama
  power_res <- pwr::pwr.r.test(
    n = n,
    r = r_val,
    sig.level = alpha,
    alternative = "two.sided"
  )
  current_power <- power_res$power

  if (current_power < desired_power) {
    needed_n <- ceiling(pwr::pwr.r.test(
      power = desired_power,
      r = r_val,
      sig.level = alpha,
      alternative = "two.sided"
    )$n)

    power_comment <- paste0(
      "Power is not sufficient (", round(current_power, 3),
      "). ", needed_n - n,
      " more samples are needed to reach ", desired_power, " power."
    )
  } else {
    power_comment <- paste0(
      "Power is sufficient (", round(current_power, 3),
      "). No need for extra samples."
    )
  }

  # Spearman
  spearman_res <- cor.test(x, y, method = "spearman", conf.level = conf.level)
  spear_comment <- get_correlation_strength(spearman_res$estimate)
  spearman_color <- if (spearman_res$p.value < alpha) "red" else "black"

  # Kendall
  kendall_res <- cor.test(x, y, method = "kendall", conf.level = conf.level)
  kendall_comment <- get_correlation_strength(kendall_res$estimate)
  kendall_color <- if (kendall_res$p.value < alpha) "red" else "black"

  result <- list(
    pearson = list(
      estimate = HTML(
        paste0("<span style='color:", pearson_color, "; font-size: 16px;'>",
               round(r_val, 2), " (", r_comment, ")</span>")
      ),
      p.value = round(pearson_res$p.value, 3),
      conf.int = round(pearson_res$conf.int, 2),
      power = round(current_power, 3),
      comment = power_comment
    ),
    spearman = list(
      estimate = HTML(
        paste0("<span style='color:", spearman_color, "; font-size: 16px;'>",
               round(spearman_res$estimate, 2), " (", spear_comment, ")</span>")
      ),
      p.value = round(spearman_res$p.value, 3)
    ),
    kendall = list(
      estimate = HTML(
        paste0("<span style='color:", kendall_color, "; font-size: 16px;'>",
               round(kendall_res$estimate, 2), " (", kendall_comment, ")</span>")
      ),
      p.value = round(kendall_res$p.value, 3)
    )
  )

  return(result)
}
