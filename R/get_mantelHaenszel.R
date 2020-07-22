get_mantelHaenszel <- function(object, ...){
  ids <- which(!partykit::nodeids(object, terminal = FALSE) %in% partykit::nodeids(object, terminal = TRUE))
  datFitted <- partykit::data_party(object) # dataframe with data, split variables and fitted groups (end nodes)
  splits <- partykit::node_party(object) # where the splits are

  # prepare data for MH
  splitGroups <- return_splitGroups(node = splits, datFitted)
  dat <- datFitted[[1]]
  sums <- rowSums(dat)
  nodeNames <- paste("node", ids, sep = "")
  MH <- lapply(splitGroups, function(grp)(calculate_mantelHaenszel(dat = dat, splitGroup = grp, sums = sums, purification = "none")))
  return(MH)
}

