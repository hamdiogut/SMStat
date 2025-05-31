hesapla_power_wilcox <- function(grup1, grup2, alpha = 0.05) {
  library(rstatix)
  library(pwr)

  df <- data.frame(
    Deger = c(grup1, grup2),
    Grup = factor(c(rep("A", length(grup1)), rep("B", length(grup2)))
    ))

  # Etki büyüklüğü (r)
  eff <- tryCatch({
    wilcox_effsize(df, Deger ~ Grup)
  }, error = function(e) return(NULL))

  if (is.null(eff)) return(list(error = "Wilcoxon etki büyüklüğü hesaplanamadı."))

  r <- eff$effsize[1]
  d <- r * sqrt(2)  # Yaklaşık Cohen's d

  # Büyüklük yorumu
  magnitude <- if (r < 0.1) {
    "Very small"
  } else if (r < 0.3) {
    "Small"
  } else if (r < 0.5) {
    "Medium"
  } else {
    "Large"
  }

  # Power tahmini
  n <- min(length(grup1), length(grup2))
  power_result <- tryCatch({
    pwr.t.test(n = n, d = d, sig.level = alpha, type = "two.sample", alternative = "two.sided")
  }, error = function(e) NULL)

  power_text <- if (!is.null(power_result$power)) round(power_result$power, 3) else NA
  needed_n <- if (!is.null(power_result$power) && power_result$power < 0.8) {
    ceiling(pwr.t.test(d = d, power = 0.8, sig.level = alpha, type = "two.sample")$n)
  } else {
    NA
  }

  return(list(
    r_effect_size = round(r, 3),
    cohen_d_approx = round(d, 3),
    magnitude = magnitude,
    power = power_text,
    gerekli_n = needed_n
  ))
}
