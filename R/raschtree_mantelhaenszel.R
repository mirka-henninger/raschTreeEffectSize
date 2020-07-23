raschtree_mantelhaenszel <- function (formula, data, stopping = TRUE, stopkrit = "A", purification, ...)
{
  object <- raschtree(formula, data = data)
  pnode <- NULL
  if(length(object) > 1){
    MH <- get_mantelHaenszel(object, purification = purification)
    pok <- sapply(MH, function(x){all(x$classification %in% stopkrit)})
    pok <- pok[pok == TRUE]
    pnode <- sub(pattern = "node", x = names(pok), replacement = "")
  }
  if (length(pnode) < 1L) {
    print("Stopping criterion was not met")
    return(list(raschtree = object,
                mantelHaenszel = "Stopping criterion was not met, no stopping based on the Mantel-Haenszel effect size measure"))
    break
  }
  if (stopping == FALSE){
    print("No stopping based on the Mantel-Haenszel effect size measure")
    return(list(raschtree = object, mantelHaenszel = summary_mantelhaenszel(MH)))
    break
  } else
  object <- nodeprune.party(object, ids = pnode)
  MH <- get_mantelHaenszel(object, purification = purification)
  node <- object$node
  nd <- as.list(node)
  kids <- lapply(nd, "[[", "kids")
  tmnl <- sapply(kids, is.null)
  id <- seq_along(nd)
  check <- sapply(id, function(i) !tmnl[i] && all(tmnl[kids[[i]]]))
  splitKrit <- paste(c("A", "B", "C")[!c("A", "B", "C") %in% stopkrit], collapse = " or ")
  print(paste("Stopping based on the Mantel-Haenszel effect size measure. Splitting was only performed, when follow-up nodes showed at least one item classified in category ",
              splitKrit, ".", sep = ""))
  return(list(raschtree = object, mantelHaenszel = summary_mantelhaenszel(MH)))
}
