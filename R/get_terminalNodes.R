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
