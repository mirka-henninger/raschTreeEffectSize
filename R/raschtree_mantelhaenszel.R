#' Fits raschtree based on the psychotree package with additional options for stopping based on the Mantel-Haenszel effect size measure
#'
#'@param formula
#'@param data
#'@param stopping A logical indicator whether stopping based on the Mantel-Haenszel effect size measure should be performed. Default is TRUE
#'@param stopkrit A character indicating the stopping criterion. Options are "A", "B", "C" or combinations of these. Stopping is done when all items in follow-up nodes fall into the stopping criterion.
#'@param purification A character indicating the type of purification that should be done. Options are "none", "2step", or "iterative".
#'
#'@return A list with two entries: raschtree object and information on the Mantel-Haenszel statistic
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
