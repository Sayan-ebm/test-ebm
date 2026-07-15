psa_summary_stats <- function(x) {
  x <- x[!is.na(x)]
  
  n <- length(x)
  m <- mean(x)
  med <- median(x)
  sdv <- sd(x)
  
  se <- if (n > 1) sdv / sqrt(n) else NA_real_
  
  ci_lower <- if (!is.na(se)) m - 1.96 * se else NA_real_
  ci_upper <- if (!is.na(se)) m + 1.96 * se else NA_real_
  
  data.frame(
    mean = m,
    median = med,
    sd = sdv,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}