#' Returns the splitGroups of a Raschtree based on the node structure and the allocation of respondents in terminal nodes
#' @param node A node in a Raschtree
#' @param datFitted A dataframe with data, split variables and fitted groups (end nodes) as returned from partykit::data_party(object)
#'
#' @return Split groups of a node in a Raschtree
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
