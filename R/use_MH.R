use_MH <- function (object, type = "mantelhaenszel", stop = "A", ...)
{
  if (type != "mantelhaenszel") {
    warning("Unknown specification of 'type'")
    return(object)
  }
  if(type == "mantelhaenszel" & length(object) > 1){
    MH <- get_mantelHaenszel(object)
    pok <- lapply(MH, function(x){all(x$classification == stop)})
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
  return(object)
}
