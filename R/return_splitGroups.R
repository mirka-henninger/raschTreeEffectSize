#' Returns the splitGroups of a raschtree based on the node structure and the allocation of respondents in terminal nodes
return_splitGroups <- function(node, datFitted){
  innerNodes <- get_innerNodes(node)
  get_id <- function(x){
    ids <- x$id
    res <- paste("node", ids, sep = "")
  }
  
  #' Get the split groups for a node in a tree object
  get_splitGroup <- function(node, datFitted){
    if(is_terminalNode(node) == TRUE){
      errorCondition("not possible")
    }
    children <- kids_node(node)
    left <- children[[1]]
    right <- children[[2]]
    terminalLeft <- get_terminalNodes(left)
    terminalRight <- get_terminalNodes(right)

    map_it <- function(x){
      if(x %in% terminalLeft){return(0)}
      if(x %in% terminalRight){return(1)}
      return(NA)
    }
    res <- unlist(lapply(datFitted$`(fitted)`, map_it))

    return(res)
  }

  splitGroups <- lapply(innerNodes, get_splitGroup, datFitted)
  names(splitGroups) <- lapply(innerNodes, get_id)
  return(splitGroups)
}
