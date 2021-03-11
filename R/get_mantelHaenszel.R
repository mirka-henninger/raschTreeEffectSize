#' Function that returns the Mantel-Haenszel effect size measure of a Raschtree object
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#' @param by A character either "node" or "type" defining the structure of the function output
#'
#' @return If by = "node": a list with one entry for each node; If by = "type": a list with entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' MH <- get_mantelHaenszel(RT, purification = "iterative", by = "type")
#' MH
#' }
#'
#' @export
get_mantelHaenszel <- function(object, purification, by = "node"){
  ids <- which(!(partykit::nodeids(object, terminal = FALSE) %in% partykit::nodeids(object, terminal = TRUE)))
  datFitted <- partykit::data_party(object) # dataframe with data, split variables and fitted groups (end nodes)
  splits <- partykit::node_party(object) # where the splits are

  # prepare data for MH
  splitGroups <- return_splitGroups(node = splits, datFitted)
  dat <- datFitted[[1]]
  sums <- rowSums(dat)
  nodeNames <- paste("node", ids, sep = "")
  MH <- lapply(splitGroups, function(grp)(calculate_mantelHaenszel(dat = dat, splitGroup = grp, sums = sums, purification = purification)))
  if(by == "type"){
    summary_mantelHaenszel <- function(x){
      list(classification = sapply(x, function(x) x$classification),
           mantelHaenszel = sapply(x, function(x) x$mantelHaenszel[1,]),
           purification =  sapply(x, function(x) x$purification),
           purificationCounter = sapply(x, function(x) x$purificationCounter)
      )
    }
    MH <- summary_mantelHaenszel(MH)
  }
  return(MH)
}

