#' Performs purification of the Mantel-Haenszel odds ratio in the Delta-scale
#'
#' @param dat A dataframe with dichotomous item responses
#' @param group An indicator of the two groups for which DIF analyses should be performed
#' @param sums A sum score used as a matching criterion for computing the Mantel Haenszel statistic
#' @param MHstat A vector or dataframe including the Mantel-Haenszel odds ratio in the Delta-scale
#' @param whichMH a vector containing the item numbers of items with Delta-MH > 1
#'
#' @return A list with purified the Mantel-Haenszel odds ratio in the Delta-scale, standard deviation of purified the Mantel-Haenszel odds ratio in the Delta-scale, a vector     indicating with items were purified
purify_MH <- function(dat, group, sums, MHstat, whichMH = whichMH){
  # purified MH for no DIF items
  tempDat <- dat[, ! (colnames(dat) %in% c("sums","group"))]
  tempDat <- data.frame(tempDat[,abs(MHstat) < 1])
  sums <- rowSums(tempDat, na.rm = TRUE)
  temp <- difR::mantelHaenszel(data = dat, member = group, match = sums)
  tempMH <- temp$resAlpha
  tempMHvar <- temp$varLambda
  resultsMH <- (2.35) * log(tempMH)
  resultsSD <- (2.35) * sqrt(tempMHvar)
  # purified MH for DIF items (including the DIF item in question)
  resultsMH[whichMH] <- NA
  for (i in whichMH){
    sumscore <- dat[,i] + sums
    temp_i <- difR::mantelHaenszel(data = data.frame(dat[,i]), member = group, match = sumscore)
    tempMH_i <- temp_i$resAlpha
    tempMHsd_i <- temp_i$varLambda
    resultsMH[i] <- (2.35) * log(tempMH_i)
    resultsSD[i] <- (2.35) * sqrt(tempMHsd_i)
  }
  whichMHpurified <- which(abs(resultsMH) >= 1)
  return(list(resultsMH = resultsMH,
              resultsSD = resultsSD,
              whichMHpurified = whichMHpurified))
}
