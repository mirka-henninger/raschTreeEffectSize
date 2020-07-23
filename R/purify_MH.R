purify_MH <- function(dat, group, sums, MHstat, whichMH = whichMH){
  # purified MH for no DIF items
  tempDat <- dat[, ! (colnames(dat) %in% c("sums","group"))]
  tempDat <- tempDat[,abs(MHstat) < 1]
  sums <- rowSums(tempDat)
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
