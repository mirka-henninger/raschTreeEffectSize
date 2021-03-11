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
    return(res)
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
#' Returns the terminal nodes of a tree object
#' @param node A node in a Raschtree
#'
#' @return terminal nodes of a tree object
get_terminalNodes <- function(node){
  if(is_terminalNode(node) == TRUE){
    id <- node$id
    res <- list(id = id)
  }
  if(is_terminalNode(node) == FALSE){
    children <- kids_node(node)
    res <- list()
    for (i in 1:(length(children))){
      res[[i]] <- get_terminalNodes(children[[i]])
    }
    res <- unlist(res)
  }
  return(res)
}
#' Function that returns the inner nodes of a tree object
#' @param node A node in a Raschtree
#'
#' @return inner nodes of a tree object
get_innerNodes <- function(node){
  if(is_terminalNode(node)){
    res <- list()
  }
  if(is_terminalNode(node) == FALSE){
    children <- kids_node(node)
    res <- list()
    for (i in 1:(length(children))){
      res[[i]] <- get_innerNodes(children[[i]])
    }
    res[[length(children)+1]] <- list(node = node)
    res <- unlist(res, recursive=FALSE)
  }
  return(res)
}
#' checks whether a node is a terminal node
#' @param node A node in a Raschtree
is_terminalNode <- function(node){
  res <- is.null(kids_node(node))
  return(res)
}
