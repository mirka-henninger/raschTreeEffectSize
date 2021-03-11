#' Function that saves Mantel-Haenszel effect size measures of a Raschtree into the Raschtree object
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#'
#' @return The Raschtree object with 'info' extended by a list named mantelHaenszel containing entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' RT_MH <- add_mantelHaenszel(RT, purification = "iterative")
#' RT_MH$info$mantelHaenszel
#' }
#' @export
add_mantelHaenszel <- function(object, purification){
  object$info$mantelHaenszel <- get_mantelHaenszel(object, purification = purification, by = "type")
  return(object)
}
