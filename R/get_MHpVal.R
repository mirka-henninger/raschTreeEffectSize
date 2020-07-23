#' Computes p-values for the statistical hypothesis testing based on log transformation of the Mantel-Haenszel common odds ratio in the Delta-scale for DIF
#' p values of the hypothesis test are need for the classification of DIF items based on the ETS guidelines
#' based on Paek & Holland, 2015, Psychometrika
get_MHpVal <- function(resMH, resMHsd, tau = 1){
  sigTest_MantelHaenszel <- function(MH = 1, sdMH = .5, tau = 1){
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
