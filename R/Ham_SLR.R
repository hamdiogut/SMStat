Ham_SLR <- function(y, x, alpha = 0.05) {
  if (length(y) != length(x)) stop("x ve y aynı uzunlukta olmalı.")

  model <- lm(y ~ x)
  summary_model <- summary(model)

  intercept <- coef(summary_model)[1, "Estimate"]
  slope <- coef(summary_model)[2, "Estimate"]
  intercept_p <- coef(summary_model)[1, "Pr(>|t|)"]
  slope_p <- coef(summary_model)[2, "Pr(>|t|)"]
  r_squared <- summary_model$r.squared

  # Etki büyüklüğü (Cohen's f²)
  f2 <- r_squared / (1 - r_squared)

  # Etki büyüklüğü yorumu
  effect_size_label <- if (f2 < 0.02) {
    "very small"
  } else if (f2 < 0.15) {
    "small"
  } else if (f2 < 0.35) {
    "medium"
  } else {
    "large"
  }

  # Power hesaplama
  n <- length(y)
  u <- 1  # bağımsız değişken sayısı
  v <- n - u - 1  # serbestlik derecesi

  power_result <- pwr::pwr.f2.test(u = u, v = v, f2 = f2, sig.level = alpha)
  power <- power_result$power

  # Power mesajı
  if (power < 0.8) {
    n_needed <- ceiling(pwr::pwr.f2.test(u = u, f2 = f2, sig.level = alpha, power = 0.8)$v + u + 1)
    power_message <- paste0("Power is insufficient (", round(power, 3), "). ",
                            "To achieve 0.8 power, at least ", n_needed, " samples are needed.")
  } else {
    power_message <- paste0("Power is sufficient (", round(power, 3), ").")
  }

  return(list(
    intercept = intercept,
    slope = slope,
    intercept_p_value = intercept_p,
    slope_p_value = slope_p,
    r_squared = r_squared,
    cohen_f2 = round(f2, 3),
    effect_size = paste0(round(f2, 3), " (", effect_size_label, ")"),
    power = round(power, 3),
    power_message = power_message
  ))
}
