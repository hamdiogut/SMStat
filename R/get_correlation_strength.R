get_correlation_strength <- function(r) {
  abs_r <- abs(r)
  case_when(
    abs_r < 0.1 ~ "no correlation",
    abs_r < 0.3 ~ "little correlation",
    abs_r < 0.5 ~ "medium correlation",
    abs_r < 0.7 ~ "high correlation",
    abs_r >= 0.7 ~ "very high correlation",
    TRUE ~ "undefined"
  )
}
