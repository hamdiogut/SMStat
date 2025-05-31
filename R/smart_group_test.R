smart_group_test <- function(response, group, alpha = 0.05, target_power = 0.8) {
  library(pwr)
  library(car)
  library(stats)
  library(rstatix)
  library(MASS)

  # --- 0. Box-Cox dönüşümü ------------------------------------------
  trans_res <- transform_by_worst_group_Tukey(response, group)
  df_tf     <- trans_res$transformed_data
  opvals    <- round(trans_res$original_pvalues,    3)
  tpvals    <- round(trans_res$transformed_pvalues, 3)
  lambda_val<- round(trans_res$lambda_used,         3)

  # 🔎 response sütunu adını dinamik bul (grup dışındaki ilk değişken)
  response_col <- setdiff(names(df_tf), "group")[1]
  if (is.na(response_col)) stop("Transformed dataframe response sütununu bulamıyor.")
  y <- df_tf[[response_col]]
  g <- df_tf$group

  # --- Yardımcı fonksiyon: power analizi ----------------------------
  cohen_f_power <- function(y, g, alpha) {
    fit  <- aov(y ~ g)
    tbl  <- summary(fit)[[1]]
    SSb  <- tbl[1, "Sum Sq"]
    SSw  <- tbl[2, "Sum Sq"]
    eta2 <- SSb / (SSb + SSw)
    f    <- sqrt(eta2 / (1 - eta2))
    k    <- length(unique(g))
    n_bar <- length(y) / k
    pow  <- pwr.anova.test(k = k, n = n_bar, f = f, sig.level = alpha)$power
    req_n <- pwr.anova.test(k = k, f = f, sig.level = alpha, power = target_power)$n
    add_per_grp <- pmax(0, ceiling(req_n) - ceiling(n_bar))
    tukey_df <- run_tukey_test(y, g)
    list(f = round(f, 3), power = round(pow, 3), needed_per_group = add_per_grp,tukresult=tukey_df)
  }

  # --- 1. Normallik testine göre Kruskal ----------------------------
  if (any(tpvals < 0.05, na.rm = TRUE)) {
    kw <- kruskal.test(y ~ g)
    tukey_df<-wilcox_pairwise_test(y,g)
    pow_lst <- cohen_f_power(rank(y), g, alpha)
    return(c(
      list(step = "Kruskal-Wallis", test_name = "kruskal.test", p_value = round(kw$p.value, 3)),
      pow_lst,
      list(lambda_used = lambda_val,
           original_pvalues = opvals,
           transformed_pvalues = tpvals,
           result = kw,
           tukresult = tukey_df,
           transformed_data = df_tf)
    ))
  }

  # --- 2. Varyans homojenliği ---------------------------------------
  lev_p <- round(car::leveneTest(y ~ g)[1, "Pr(>F)"], 3)
  if (lev_p < 0.05) {
    welch <- oneway.test(y ~ g, var.equal = FALSE)
    tukey_df <- run_tukey_test(y, g)
    pow_lst <- cohen_f_power(y, g, alpha)


    return(c(
      list(step = "Welch ANOVA", test_name = "oneway.test", levene_p_value = lev_p, p_value = round(welch$p.value, 3), tukresult = tukey_df,),
      pow_lst,
      list(lambda_used = lambda_val,
           original_pvalues = opvals,
           transformed_pvalues = tpvals,
           result = welch,
           transformed_data = df_tf)
    ))
  }

  # --- 3. Klasik ANOVA ve Tukey -------------------------------------
  fit <- aov(y ~ g)
  aov_p <- round(summary(fit)[[1]][["Pr(>F)"]][1], 3)
  pow_lst <- cohen_f_power(y, g, alpha)
  tukey_df <- run_tukey_test(y, g)

  return(c(
    list(step = "Klasik ANOVA", test_name = "aov", levene_p_value = lev_p, p_value = aov_p),
    pow_lst,
    list(lambda_used = lambda_val,
         original_pvalues = opvals,
         transformed_pvalues = tpvals,
         result = summary(fit),
         tukresult = tukey_df,
         transformed_data = df_tf)
  ))
}
