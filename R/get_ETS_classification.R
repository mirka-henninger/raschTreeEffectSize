#' Performs classification of the Mantel-Haenszel effect size measured based on ETS guidelines
#' see Dorans & Holland, 1993, p. 41f or Paek & Holland, 2015, Psychometrika
#'
#' @param res_MH A vector or dataframe with the Mantel-Haenszel Odds Ratio effect size measure in the Delta scale (-2.35 * log (Mantel-Haenszel estimate of the common odds ratios)) for each item
#' @param res_MH_sd A vector or dataframe with the standard deviation of Mantel-Haenszel Odds Ratio effect size measure in the Delta scale for each item (-2.35 * sqrt (the variance of the lambda_MH statistic))
#'
#' @return A vector or dataframe with the corresponding A/B/C classification for each item
get_ETS_classification <- function(res_MH, res_MH_sd){
  #' Computes p-values for the statistical hypothesis testing based on log transformation of the Mantel-Haenszel common odds ratio in the Delta-scale for DIF
  #' p values of the hypothesis test are need for the classification of DIF items based on the ETS guidelines
  #' based on Paek & Holland, 2015, Psychometrika
  get_MH_pval <- function(res_MH, res_MH_sd, tau){
    sigtest_mantelhaenszel <- function(MH = 1, sd_MH = .5, tau){
      # schervish <- pnorm((-tau - abs(MH)) / sd_MH) + pnorm((tau - abs(MH)) / sd_MH)
      # return(schervish)
      df <- 1
      lambda <- (tau/sd_MH)^2
      psi <- (MH/sd_MH)^2
      chisqu <- 1 - pchisq(psi, df, lambda)
      return(chisqu)
    }
    p_vals <- data.frame()
    for (i in 1:nrow(res_MH)){
      p_vals <- rbind(p_vals, sigtest_mantelhaenszel(MH = unlist(res_MH[i,]), sd_MH = unlist(res_MH_sd[i,]), tau = tau))
    }
    rownames(p_vals) <- rownames(res_MH); colnames(p_vals) <- colnames(res_MH)
    return(p_vals)
  }

  p_vals_A <- get_MH_pval(res_MH, res_MH_sd, tau = 0)
  p_vals_C <- get_MH_pval(res_MH, res_MH_sd, tau = 1)
  MH_classi <- ifelse(abs(res_MH) < 1 | p_vals_A >= .05, "A",
                     ifelse(abs(res_MH) >= 1.5 & p_vals_C < .05, "C",
                            "B"))
  return(MH_classi)
}
