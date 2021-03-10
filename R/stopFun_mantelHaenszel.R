#' Returns a function that can be used for the stopFun argument in the Raschtree.
#' The stopping function returns a logical indicating whether all items are classified as the stopping criterion in the tree growing process
#' It uses the calculate_mantelHaenszel function for the Mantel-Haenszel effect size measure and classification
#' @param purification A character indicating the type of purification should be used? Options "none", "2step", "iterative"
#' @param stopCrit A character indicating when the Raschtree should be stopped. Default is "A", hence the Raschtree is stopped when all items fall into category "A" of the ETS classification scheme. When c("A", "B"), the Raschtree is stopped when all items fall in either category "A" or "B".
#'
#' @return A function that can be used for the stopFun argument in the Raschtree
#' @export
stopFun_mantelHaenszel <- function(purification, stopCrit = c("A")){
  stopfun <- function(y, kidids){
    dat <- y
    group <- kidids - 1
    sums <- rowSums(dat)
    MH <- calculate_mantelHaenszel(dat = dat, splitGroup = group, sums = sums, purification = purification)$classification
    return(all(MH %in% stopCrit))
  }
  return(stopfun)
}
