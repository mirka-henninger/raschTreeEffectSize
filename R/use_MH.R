use_MH <- function (tree, type = "mantelhaenszel", stop = "A", ...)
{
  if (type != "mantelhaenszel") {
    warning("Unknown specification of 'type'")
    return(tree)
  }
  if(type == "mantelhaenszel" & length(tree) > 1){
    pok <- apply(get_MantelHaenszel(tree)$classification, 1, function(x){all(x == stop)})
    pok <- pok[pok == TRUE]
    pnode <- sub(pattern = "node", x = names(pok), replacement = "")
  }
  if (length(pnode) < 1L)
    break
  tree <- nodeprune.party(tree, ids = pnode)
  node <- tree$node
  nd <- as.list(node)
  kids <- lapply(nd, "[[", "kids")
  tmnl <- sapply(kids, is.null)
  id <- seq_along(nd)
  check <- sapply(id, function(i) !tmnl[i] && all(tmnl[kids[[i]]]))
  return(tree)
}
