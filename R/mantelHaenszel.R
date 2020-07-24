#' Returns a logical indicating whether all items are classified as the stopping criterion in the tree growing process
#' uses the calculate_mantelHaenszel function for the Mantel-Haenszel effect size measure and classification
mantelHaenszel <- function(purification, stopkrit = c("A")){
  stopfun <- function(y, kidids){
    dat <- y
    group <- kidids - 1
    sums <- rowSums(dat)
    MH <- calculate_mantelHaenszel(dat = dat, splitGroup = group, sums = sums, purification = purification)$classification
    return(all(MH %in% stopkrit))
  }
  return(stopfun)
}
