calculate_mantelHaenszel <- function(dat, splitGroup, sums, purification = "none", ...){
  counter <- 0
  removeNA <- !is.na(as.vector(splitGroup))

  # remove missings in nodes from MH calculations
  dat <- dat[removeNA,]
  sums <- sums[removeNA]
  group <- splitGroup[removeNA]

  # calculate MH
  temp <- difR::mantelHaenszel(data = dat, member = group, match = sums)
  tempMH <- temp$resAlpha
  tempMHvar <- temp$varLambda
  resultsMH <- (2.35) * log(tempMH)
  resultsSD <- (2.35) * sqrt(tempMHvar)
  whichMH <- which(abs(resultsMH) >= 1)
  if (purification == "2step" | purification == "iterative" & length(whichMH) > 0){
    purified <- purify_MH(dat = dat, group = group, sums = sums, MHstat = resultsMH, whichMH = whichMH)
    resultsMH <- purified$resultsMH
    resultsSD <- purified$resultsSD
    counter <- counter + 1
  }
  if (purification == "iterative" & length(whichMH) > 0){
    repeat {
      if(all(whichMH %in% purified$whichMHpurified)){break}
      whichMH <- purified$whichMHpurified
      purified <-  purify_MH(dat = dat, group = group, sums = sums, MHstat = resultsMH, whichMH = whichMH)
      resultsMH <- purified$resultsMH
      resultsSD <- purified$resultsSD
      counter <- counter + 1
      if(counter > 1000){break}
    }
  }
  MHclassi <- get_MHclassification(resMH = data.frame(resultsMH), resMHsd = data.frame(resultsSD))

  MH <- rbind(MH = resultsMH,
              SD = resultsSD)
  colnames(MH) <- paste("item", 1:ncol(dat), sep = "")
  names(MHclassi) <- paste("item", 1:ncol(dat), sep = "")
  result <- list(mantelHaenszel = MH,
                 classification = c(MHclassi),
                 purification = purification,
                 purificationCounter = counter
  )
  return(result)
}

