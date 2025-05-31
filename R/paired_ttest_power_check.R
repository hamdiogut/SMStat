paired_ttest_power_check <- function(x, y, alpha = 0.05, target_power = 0.8) {
  library(pwr)
  mplist<-list()
  if (length(x) != length(y)) stop("Vektör uzunlukları eşit olmalı.")

  # Farkları al
  diff_vals <- x - y

  # Cohen's d
  d <- mean(diff_vals) / sd(diff_vals)

  # Mevcut örnek sayısı
  n <- length(diff_vals)

  # Power hesapla
  power_result <- pwr.t.test(n = n, d = d, sig.level = alpha, type = "paired")
  mplist[["Power"]] <- round(power_result$power, 3)
  mplist[["Cohen_s_d"]] <- round(d, 3)

  # Power yeterli değilse gerekli örnek sayısını hesapla
  if (power_result$power < target_power) {
    required_n <- pwr.t.test(power = target_power, d = d, sig.level = alpha, type = "paired")$n
    mplist[["ek_ornek"]]<-ceiling(required_n)
  } else {
    mplist[["Power_yorum"]]<-"Örnek Yerelidir"
  }
  return(mplist)
}
