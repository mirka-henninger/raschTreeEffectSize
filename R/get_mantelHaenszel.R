get_MantelHaenszel <- function(raschTree){
  datFitted <- partykit::data_party(raschTree) # dataframe with data, split variables and fitted groups (end nodes)
  nodes <- partykit::node_party(raschTree) # where the splits are
  # prepare data for MH
  dat <- return_splitGroups(nodes, datFitted)
  groupCols <- names(dat)[grep("node", names(dat))]
  dat$sums <- rowSums(dat[[1]])
  resMH <- data.frame(); resMHsd <- data.frame()

  for (node in groupCols){
    # remove NAs in end nodes before calculating MH
    dat_woNA <- dat[!is.na(dat[[node]]),]
    # calculate MH
    temp <- difR::mantelHaenszel(data = dat_woNA[[1]], member = dat_woNA[[node]], match = dat_woNA$sums)
    tempMH <- temp$resAlpha
    tempMHvar <- temp$varLambda
    resultsMH <- (2.35) * log(tempMH)
    resultsSD <- (2.35) * sqrt(tempMHvar)
    whichMH <- which(abs(resultsMH) >= 1)

    # purified MH for no DIF items
    tempDat <- dat_woNA[[1]]
    tempDat <- tempDat[,abs(resultsMH) < 1]
    dat_woNA$sums <- rowSums(tempDat)
    temp <- difR::mantelHaenszel(data = dat_woNA[[1]], member = dat_woNA[[node]], match = dat_woNA$sums)
    tempMH <- temp$resAlpha
    tempMHvar <- temp$varLambda
    resultsMH <- (2.35) * log(tempMH)
    resultsSD <- (2.35) * sqrt(tempMHvar)

    # purified MH for DIF items (including the DIF item in question)
    resultsMH[whichMH] <- NA
    for (i in whichMH){
      sums <- dat_woNA[[1]][,i] + dat_woNA$sums
      temp <- difR::mantelHaenszel(data = dat_woNA[[1]], member = dat_woNA[[node]], match = sums)
      tempMH <- temp$resAlpha
      tempMHvar <- temp$varLambda
      resultsMH[i] <- (2.35) * log(tempMH[i])
      resultsSD[i] <- (2.35) * sqrt(tempMHvar[i])
    }
    resMH <- rbind(resMH, resultsMH)
    resMHsd <- rbind(resMHsd, resultsSD)
  }

  # format output
  colnames(resMH) <- paste("item", 1:ncol(dat[[1]]), sep = "")
  rownames(resMH) <- groupCols
  colnames(resMHsd) <- paste("item", 1:ncol(dat[[1]]), sep = "")
  rownames(resMHsd) <- groupCols

  # ABC classification
  MHclassi <- get_MHclassification(resMH, resMHsd)

  # return output
  return(list(MH = resMH, SD = resMHsd, classification = MHclassi))
}
