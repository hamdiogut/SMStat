transform_by_worst_group_Tukey <- function(response, group) {
  ## -------------------------------------------------- 0) Kontroller
  if (length(response) != length(group))
    stop("response ve group aynı uzunlukta olmalı.")

  group <- as.factor(group)
  lvls  <- levels(group)

  ## -------------------------------- 1) Shapiro p-değerleri (her grup)
  shapiro_p <- sapply(lvls, function(g) {
    x <- na.omit(response[group == g])
    if (length(x) < 3) return(NA_real_)
    tryCatch(shapiro.test(x)$p.value, error = function(e) NA_real_)
  })
  names(shapiro_p) <- lvls
  if (all(is.na(shapiro_p)))
    stop("Gruplarda normallik testi için yeterli gözlem yok.")

  ## ----------------------------- 2) En küçük p-değerli (bozuk) grup
  worst_group <- names(which.min(shapiro_p))
  x_worst     <- na.omit(response[group == worst_group])

  ## -------------------------------- 3) λ’yı doğrudan al (returnLambda = TRUE)
  library(rcompanion)
  lambda <- transformTukey(x_worst, returnLambda = TRUE)
  if (is.null(lambda) || length(lambda) != 1 || is.na(lambda))
    stop("Lambda alınamadı – transformTukey lambda döndürmedi.")

  ## -------------------------------- 4) Aynı λ ile tüm veriyi dönüştür
  boxcox_apply <- function(x, λ) if (λ == 0) log(x) else (x^λ - 1) / λ
  response_tf  <- boxcox_apply(response, lambda)

  ## ------------------------------ 5) Dönüşüm sonrası Shapiro p-değerleri
  shapiro_p_tf <- sapply(lvls, function(g) {
    x <- na.omit(response_tf[group == g])
    if (length(x) < 3) return(NA_real_)
    tryCatch(shapiro.test(x)$p.value, error = function(e) NA_real_)
  })
  names(shapiro_p_tf) <- lvls

  ## -------------------------------- 6) Çıktı listesi
  list(
    lambda_used         = round(lambda, 4),
    target_group        = worst_group,
    original_pvalues    = round(shapiro_p,    4),
    transformed_pvalues = round(shapiro_p_tf, 4),
    transformed_data    = data.frame(
      response_tf = response_tf,
      group       = group,
      stringsAsFactors = FALSE)
  )
}
