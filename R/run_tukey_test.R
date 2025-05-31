run_tukey_test <- function(response, group) {
  if (!is.numeric(response)) stop("response sayısal olmalı.")
  if (!is.factor(group)) group <- as.factor(group)
  if (length(response) != length(group)) stop("Uzunluklar eşit değil.")

  model <- aov(response ~ group)
  tukey_result <- TukeyHSD(model)[[1]]

  df <- as.data.frame(tukey_result)
  df$Comparison <- rownames(df)
  df <- df[, c("Comparison", "diff", "lwr", "upr", "p adj")]
  names(df) <- c("Karşılaştırma", "Fark", "Alt CI", "Üst CI", "p-adj")
  df$`p-adj` <- formatC(df$`p-adj`, digits = 3, format = "f")
  return(df)
}
