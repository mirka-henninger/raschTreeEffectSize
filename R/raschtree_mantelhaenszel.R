raschtree_mantelhaenszel <- function (formula, data, stop = "A", purification = "none", ...)
{
  object <- raschtree(formula, data = data)
  if(length(object) > 1){
    MH <- get_mantelHaenszel(object, stop = stop, purification = purification)
    pok <- sapply(MH, function(x){all(x$classification == stop)})
    pok <- pok[pok == TRUE]
    pnode <- sub(pattern = "node", x = names(pok), replacement = "")
  }
  if (length(pnode) < 1L)
    break
  object <- nodeprune.party(object, ids = pnode)
  node <- object$node
  nd <- as.list(node)
  kids <- lapply(nd, "[[", "kids")
  tmnl <- sapply(kids, is.null)
  id <- seq_along(nd)
  check <- sapply(id, function(i) !tmnl[i] && all(tmnl[kids[[i]]]))
  return(list(raschtree = object, mantelHaenszel = MH))
}
