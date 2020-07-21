is_terminalNode <- function(node){
  res <- is.null(kids_node(node))
  return(res)
}
