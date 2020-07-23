#' Returns the splitGroups of a raschtree based on the node structure and the allocation of respondents in terminal nodes
return_splitGroups <- function(node, datFitted){
  innerNodes <- get_innerNodes(node)
  get_id <- function(x){
    ids <- x$id
    res <- paste("node", ids, sep = "")
  }

  splitGroups <- lapply(innerNodes, get_splitGroup, datFitted)
  names(splitGroups) <- lapply(innerNodes, get_id)
  return(splitGroups)
}
