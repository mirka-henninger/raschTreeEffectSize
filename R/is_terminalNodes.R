#' checks whether a node is a terminal node
#' @param node A node in a Raschtree
is_terminalNode <- function(node){
  res <- is.null(kids_node(node))
  return(res)
}
