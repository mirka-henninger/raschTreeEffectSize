#' Function that saves Mantel-Haenszel effect size measures of a Raschtree into the Raschtree object
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#'
#' @return The Raschtree object with 'info' extended by a list named mantelHaenszel with one entry for each node containing entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#' 
#' @examples
#' data("DIFSim", package = "psychotree")
#' rt <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' rt_MH <- add_mantelHaenszel(rt, purification = "iterative")
#' rt_MH$info$mantelHaenszel
#' 
#' @export
add_mantelHaenszel <- function(object, purification){
  object$info$mantelHaenszel <- get_mantelHaenszel(rt, purification = purification)
  return(object)
}