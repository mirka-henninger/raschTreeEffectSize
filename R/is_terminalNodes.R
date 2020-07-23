#' checks whether a node is a terminal node
is_terminalNode <- function(node){
  res <- is.null(kids_node(node))
  return(res)
}
