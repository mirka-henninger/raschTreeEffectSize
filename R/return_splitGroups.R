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
