wilcoxon_power_check <- function(x, y, alpha = 0.05, target_power = 0.8) {
  library(pwr)
  if (length(x) != length(y)) stop("Vektör uzunlukları eşit olmalı.")

  # Fark vektörü
  diff_vals <- x - y
  n <- length(diff_vals)

  # Etki büyüklüğü (nonparametrik tahmin)
  sd_diff <- sd(diff_vals)
  if (sd_diff == 0) stop("Farkların standart sapması sıfır. Etki büyüklüğü hesaplanamaz.")

  d_est <- abs(median(diff_vals)) / sd_diff

  # Power tahmini
  power_est <- tryCatch({
    pwr.t.test(n = n, d = d_est, sig.level = alpha, type = "paired")$power
  }, error = function(e) NA)

  # Gerekli örnek sayısını hesapla (yaklaşık)
  required_n <- tryCatch({
    if (!is.na(power_est) && power_est < target_power) {
      ceiling(pwr.t.test(power = target_power, d = d_est, sig.level = alpha, type = "paired")$n)
    } else {
      NA
    }
  }, error = function(e) NA)

  # Liste döndür
  result <- list(
    method = "Wilcoxon Signed-Rank Test (paired)",
    n = n,
    alpha = alpha,
    d_est = round(d_est, 4),
    power_est = round(power_est, 4),
    power_sufficient = !is.na(power_est) && power_est >= target_power,
    required_n_if_power_low = required_n
  )

  return(result)
}
