#' Function that returns the Mantel-Haenszel effect size measure of a Raschtree object
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#' @param ... Further arguments
#'
#' @return A list with Mantel-Haenszel effect size, classification, purification type, and convergence if purification was iterative
#' @export
get_mantelHaenszel <- function(object, purification, ...){
  ids <- which(!partykit::nodeids(object, terminal = FALSE) %in% partykit::nodeids(object, terminal = TRUE))
  datFitted <- partykit::data_party(object) # dataframe with data, split variables and fitted groups (end nodes)
  splits <- partykit::node_party(object) # where the splits are

  # prepare data for MH
  splitGroups <- return_splitGroups(node = splits, datFitted)
  dat <- datFitted[[1]]
  sums <- rowSums(dat)
  nodeNames <- paste("node", ids, sep = "")
  MH <- lapply(splitGroups, function(grp)(calculate_mantelHaenszel(dat = dat, splitGroup = grp, sums = sums, purification = purification)))
  return(MH)
}

