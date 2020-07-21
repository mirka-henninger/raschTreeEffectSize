return_splitGroups <- function(node, datFitted){
  innerNodes <- get_innerNodes(node)
  get_id <- function(x){
    ids <- x$id
    res <- paste("node", ids, sep = "")
  }

  temp <- lapply(innerNodes, get_splitGroup, datFitted)
  names(temp) <- lapply(innerNodes, get_id)
  res <- cbind(datFitted, temp)
  return(res)
}
