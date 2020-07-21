#' classification of DIF based on ETS scale
#' based on Dorans & Holland, 1993, p. 41f
#' based on Paek & Holland, 2015, Psychometrika
get_MHclassification <- function(resMH, resMHsd, ...){
  pVals_A <- get_MHpVal(resMH, resMHsd, tau = 0)
  pVals_C <- get_MHpVal(resMH, resMHsd, tau = 1)
  MHclassi <- ifelse(abs(resMH) < 1 | pVals_A >= .05, "A",
                     ifelse(abs(resMH) >= 1.5 & pVals_C < .05, "C",
                            "B"))
  return(MHclassi)
}

