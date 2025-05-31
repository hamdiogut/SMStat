hesapla_power_ttest <- function(grup1, grup2, alpha = 0.05) {
  library(pwr)
  # Örneklem büyüklükleri
  n1 <- length(grup1)
  n2 <- length(grup2)

  # Etki büyüklüğü (Cohen's d)
  mean_diff <- abs(mean(grup1) - mean(grup2))
  pooled_sd <- sqrt(((sd(grup1)^2 + sd(grup2)^2) / 2))
  effect_size <- mean_diff / pooled_sd

  # Etki büyüklüğü yorumu
  magnitude <- if (effect_size < 0.2) {
    "Very small"
  } else if (effect_size < 0.5) {
    "Small"
  } else if (effect_size < 0.8) {
    "Medium"
  } else {
    "Large"
  }

  # Power hesaplama (eşit grup varsayımı ile)
  n <- min(n1, n2)  # Denge için küçük grup kullanılır
  sonuc <- tryCatch({
    pwr.t.test(n = n, d = effect_size, sig.level = alpha, type = "two.sample", alternative = "two.sided")
  }, error = function(e) {
    return(list(error = e$message))
  })

  return(list(
    effect_size = round(effect_size, 3),
    magnitude = magnitude,
    power = if (!is.null(sonuc$power)) round(sonuc$power, 3) else NA,
    gerekli_n = if (!is.null(sonuc$power) && sonuc$power < 0.8) {
      ceiling(pwr.t.test(d = effect_size, power = 0.8, sig.level = alpha, type = "two.sample")$n)
    } else {
      NA
    }
  ))
}
