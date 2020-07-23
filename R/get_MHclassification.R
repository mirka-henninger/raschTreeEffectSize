#' Performs classification of the Mantel-Haenszel effect size measured based on ETS guidelines
#' see Dorans & Holland, 1993, p. 41f or Paek & Holland, 2015, Psychometrika
#'
#' @param resMH A vector or dataframe with the Mantel-Haenszel Odds Ratio effect size measure in the Delta scale (-2.35 * log (Mantel-Haenszel estimate of the common odds ratios)) for each item
#' @param resMHsd A vector or dataframe with the standard deviation of Mantel-Haenszel Odds Ratio effect size measure in the Delta scale for each item (-2.35 * sqrt (the variance of the lambda_MH statistic))
#'
#' @return A vector or dataframe with the corresponding A/B/C classification for each item
get_MHclassification <- function(resMH, resMHsd, ...){
  pVals_A <- get_MHpVal(resMH, resMHsd, tau = 0)
  pVals_C <- get_MHpVal(resMH, resMHsd, tau = 1)
  MHclassi <- ifelse(abs(resMH) < 1 | pVals_A >= .05, "A",
                     ifelse(abs(resMH) >= 1.5 & pVals_C < .05, "C",
                            "B"))
  return(MHclassi)
}

