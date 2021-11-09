#' Calculates the Mantel-Haenszel statistic for Differential Item Functioning (DIF)
#'
#' @param dat A dataframe with dichotomous item responses
#' @param split_group An indicator of the two groups for which DIF analyses should be performed
#' @param sums A sum score used as a matching criterion for computing the Mantel Haenszel statistic
#' @param purification A character indicating the type of purification that is used on the Mantel Haenszel statistic. Options are "none", "2step", or "iterative"
#' @param ... Further arguments
#'
#' @return A list with the Mantel-Haenszel effect size measure in the Delta scale, the classification based on the ETS guidelines and information on purification and convergence of purification if iterative purification was done.
calculate_mantelhaenszel <- function(dat, split_group, sums, purification, ...){
  if (! purification %in% c("none", "2step", "iterative")){
    print("Argument 'purification' is neither 'none', '2step', nor 'iterative'. No purification is done")
    purification <- "none"
  }
  counter <- 0
  remove_NA <- !is.na(as.vector(split_group))

  # remove missings in nodes from MH calculations
  dat <- dat[remove_NA,]
  sums <- sums[remove_NA]
  group <- split_group[remove_NA]

  # calculate MH
  temp <- difR::mantelHaenszel(data = dat, member = group, match = sums)
  temp_MH <- temp$resAlpha
  temp_MH_var <- temp$varLambda
  results_MH <- (2.35) * log(temp_MH)
  results_SD <- (2.35) * sqrt(temp_MH_var)
  which_MH <- which(abs(results_MH) >= 1)
  if ((purification == "2step" | purification == "iterative") & length(which_MH) > 0){
    purified <- purify_MH(dat = dat, group = group, sums = sums, MH_stat = results_MH, which_MH = which_MH)
    results_MH <- purified$results_MH
    results_SD <- purified$results_SD
    counter <- counter + 1
  }
  if (purification == "iterative" & length(which_MH) > 0){
    repeat {
      if(all(which_MH %in% purified$which_MH_purified)){break}
      which_MH <- purified$which_MH_purified
      purified <-  purify_MH(dat = dat, group = group, sums = sums, MH_stat = results_MH, which_MH = which_MH)
      results_MH <- purified$results_MH
      results_SD <- purified$results_SD
      counter <- counter + 1
      if(counter >= ncol(dat)){break}
    }
  }
  MHclassi <- get_ETS_classification(res_MH = data.frame(results_MH), res_MH_sd = data.frame(results_SD))

  MH <- rbind(MH = results_MH,
              SD = results_SD)
  colnames(MH) <- paste("item", 1:ncol(dat), sep = "")
  names(MHclassi) <- paste("item", 1:ncol(dat), sep = "")
  result <- list(mantelhaenszel = MH,
                 classification = c(MHclassi),
                 purification = purification,
                 purification_counter = counter
  )
  return(result)
}
#' Performs purification of the Mantel-Haenszel odds ratio in the Delta-scale
#'
#' @param dat A dataframe with dichotomous item responses
#' @param group An indicator of the two groups for which DIF analyses should be performed
#' @param sums A sum score used as a matching criterion for computing the Mantel Haenszel statistic
#' @param MH_stat A vector or dataframe including the Mantel-Haenszel odds ratio in the Delta-scale
#' @param which_MH a vector containing the item numbers of items with Delta-MH > 1
#'
#' @return A list with purified the Mantel-Haenszel odds ratio in the Delta-scale, standard deviation of purified the Mantel-Haenszel odds ratio in the Delta-scale, a vector     indicating with items were purified
purify_MH <- function(dat, group, sums, MH_stat, which_MH = which_MH){
  # purified MH for no DIF items
  temp_dat <- data.frame(dat[,abs(MH_stat) < 1])
  sums <- rowSums(temp_dat, na.rm = TRUE)
  temp <- difR::mantelHaenszel(data = dat, member = group, match = sums)
  temp_MH <- temp$resAlpha
  temp_MH_var <- temp$varLambda
  results_MH <- (2.35) * log(temp_MH)
  results_SD <- (2.35) * sqrt(temp_MH_var)
  # purified MH for DIF items (including the DIF item in question)
  results_MH[which_MH] <- NA
  for (i in which_MH){
    sumscore <- dat[,i] + sums
    temp_i <- difR::mantelHaenszel(data = data.frame(dat[,i]), member = group, match = sumscore)
    temp_MH_i <- temp_i$resAlpha
    temp_MH_sd_i <- temp_i$varLambda
    results_MH[i] <- (2.35) * log(temp_MH_i)
    results_SD[i] <- (2.35) * sqrt(temp_MH_sd_i)
  }
  which_MH_purified <- which(abs(results_MH) >= 1)
  return(list(results_MH = results_MH,
              results_SD = results_SD,
              which_MH_purified = which_MH_purified))
}

