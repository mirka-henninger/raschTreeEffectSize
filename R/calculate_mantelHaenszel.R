#' Calculates the Mantel-Haenszel statistic for Differential Item Functioning (DIF)
#'
#' @param dat A dataframe with dichotomous item responses
#' @param splitGroup An indicator of the two groups for which DIF analyses should be performed
#' @param sums A sum score used as a matching criterion for computing the Mantel Haenszel statistic
#' @param purification A character indicating the type of purification that is used on the Mantel Haenszel statistic. Options are "none", "2step", or "iterative"
#' @param ... Further arguments
#'
#' @return A list with the Mantel-Haenszel effect size measure in the Delta scale, the classification based on the ETS guidelines and information on purification and convergence of purification if iterative purification was done.
#' @export
calculate_mantelHaenszel <- function(dat, splitGroup, sums, purification, ...){
  if (! purification %in% c("none", "2step", "iterative")){
    print("Argument 'purification' is neither 'none', '2step', nor 'iterative'. No purification is done")
    purification <- "none"
  }
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
  if ((purification == "2step" | purification == "iterative") & length(whichMH) > 0){
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
      if(counter > ncol(dat)){break}
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

