#' Improves the output of the Mantel Haenszel statistic for the raschtree
#' @param x An object (list) returned from the get_mantelHaenszel.R function
#'
#' @return A list with classification, Mantel-Haenszel odds ratio, purification, and purification counter for each split in the Raschtree
#' @export
summary_mantelhaenszel <- function(x){
  list(classification = sapply(x, function(x) x$classification),
       mantelHaenszel = sapply(x, function(x) x$mantelHaenszel[1,]),
       purification =  sapply(x, function(x) x$purification),
       purificationCounter = sapply(x, function(x) x$purificationCounter)
  )
}
