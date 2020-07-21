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
