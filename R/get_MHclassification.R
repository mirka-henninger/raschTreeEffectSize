#' Performs classification of the Mantel-Haenszel effect size measured based on ETS guidelines
#' see Dorans & Holland, 1993, p. 41f or Paek & Holland, 2015, Psychometrika
#'
#' @param resMH A vector or dataframe with the Mantel-Haenszel Odds Ratio effect size measure in the Delta scale (-2.35 * log (Mantel-Haenszel estimate of the common odds ratios)) for each item
#' @param resMHsd A vector or dataframe with the standard deviation of Mantel-Haenszel Odds Ratio effect size measure in the Delta scale for each item (-2.35 * sqrt (the variance of the lambda_MH statistic))
#'
#' @return A vector or dataframe with the corresponding A/B/C classification for each item
get_MHclassification <- function(resMH, resMHsd){
  #' Computes p-values for the statistical hypothesis testing based on log transformation of the Mantel-Haenszel common odds ratio in the Delta-scale for DIF
  #' p values of the hypothesis test are need for the classification of DIF items based on the ETS guidelines
  #' based on Paek & Holland, 2015, Psychometrika
  get_MHpVal <- function(resMH, resMHsd, tau){
    sigTest_MantelHaenszel <- function(MH = 1, sdMH = .5, tau){
      # schervish <- pnorm((-tau - abs(MH)) / sdMH) + pnorm((tau - abs(MH)) / sdMH)
      # return(schervish)
      df <- 1
      lambda <- (tau/sdMH)^2
      psi <- (MH/sdMH)^2
      chisqu <- 1 - pchisq(psi, df, lambda)
      return(chisqu)
    }
    pVals <- data.frame()
    for (i in 1:nrow(resMH)){
      pVals <- rbind(pVals, sigTest_MantelHaenszel(MH = unlist(resMH[i,]), sdMH = unlist(resMHsd[i,]), tau = tau))
    }
    rownames(pVals) <- rownames(resMH); colnames(pVals) <- colnames(resMH)
    return(pVals)
  }

  pVals_A <- get_MHpVal(resMH, resMHsd, tau = 0)
  pVals_C <- get_MHpVal(resMH, resMHsd, tau = 1)
  MHclassi <- ifelse(abs(resMH) < 1 | pVals_A >= .05, "A",
                     ifelse(abs(resMH) >= 1.5 & pVals_C < .05, "C",
                            "B"))
  return(MHclassi)
}
