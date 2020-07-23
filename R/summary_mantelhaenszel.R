#' Improves the output of the Mantel Haenszel statistic for the raschtree
summary_mantelhaenszel <- function(x){
  list(classification = sapply(x, function(x) x$classification),
       mantelHaenszel = sapply(x, function(x) x$mantelHaenszel[1,]),
       purification =  sapply(x, function(x) x$purification),
       convergence = sapply(x, function(x) x$purificationCounter) < 1000
  )
}
