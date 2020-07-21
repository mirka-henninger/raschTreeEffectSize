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

