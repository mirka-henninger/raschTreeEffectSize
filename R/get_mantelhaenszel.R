#' Function that returns the Mantel-Haenszel effect size measure of a Raschtree object
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#' @param by A character either "node" or "type" defining the structure of the function output
#'
#' @return If by = "node": a list with one entry for each node; If by = "type": a list with entries Mantel-Haenszel effect size, classification, purification type, and purification_counter
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim)
#' MH <- get_mantelhaenszel(RT, purification = "iterative", by = "type")
#' MH
#' }
#'
#' @export
get_mantelhaenszel <- function(object, purification, by = "node"){
  # check whether object is of type Raschtree, modelparty, and party
  if(!(all(class(object) %in% c("raschtree", "modelparty", "party"))))
    stop("Object must be an Raschtree object (as returned from the raschtree function")

  # extract information from Raschtree
  ids <- which(!(partykit::nodeids(object, terminal = FALSE) %in% partykit::nodeids(object, terminal = TRUE)))
  dat_fitted <- partykit::data_party(object) # dataframe with data, split variables and fitted groups (end nodes)
  splits <- partykit::node_party(object) # where the splits are

  # prepare data for MH
  split_groups <- return_split_groups(node = splits, dat_fitted)
  dat <- dat_fitted[[1]]
  sums <- rowSums(dat)
  node_names <- paste("node", ids, sep = "")
  MH <- lapply(split_groups, function(grp)(calculate_mantelhaenszel(dat = dat, split_group = grp, sums = sums, purification = purification)))
  if(by == "type"){
    summary_mantelhaenszel <- function(x){
      list(classification = sapply(x, function(x) x$classification),
           mantelhaenszel = sapply(x, function(x) x$mantelhaenszel[1,]),
           purification =  sapply(x, function(x) x$purification),
           purification_counter = sapply(x, function(x) x$purification_counter)
      )
    }
    MH <- summary_mantelhaenszel(MH)
  }
  return(MH)
}

