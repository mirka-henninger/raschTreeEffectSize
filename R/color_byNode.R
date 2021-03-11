#' Helper function to to be used for the 'col' argument in the plot method for Raschtrees
#' Helps to color item parameter in profileplots by inner nodes and ETS Mantel-Haenszel classification
#'
#' @param nodeName An integer indicating the inner node after which the item paramter should be colored
#' @param cols A named character vector indicating the
#' @return A list named after the terminal nodes containing the colors of the item parameter based on ETS Mantel-Haenszel classification
#'
#' @examples
#' data("DIFSim", package = "psychotree")
#' rt <- raschtree(resp ~ age + gender + motivation, data = DIFSim, verbose = TRUE)
#' rt_MH <- add_mantelHaenszel(rt, purification = "iterative")
#' plot(rt_MH, col = color_byNode(rt_MH, 2))
#'
#' @export
color_byNode <- function(object, nodeName, cols = c("green", "orange", "red")){
  # get info on end nodes
  nodes <- partykit::node_party(object) # where the splits are
  innerNodes <- raschTreeEffectSize:::get_innerNodes(nodes)
  whichID <- which(sapply(innerNodes, function(x) x$id) == nodeName)
  terminalNodes <- raschTreeEffectSize:::get_terminalNodes(innerNodes[[whichID]])
  allTerminalNodes <- raschTreeEffectSize:::get_terminalNodes(nodes)
  otherTerminalNodes <- allTerminalNodes[!allTerminalNodes %in% terminalNodes]

  # color
  MHclassi <- object$info$mantelHaenszel$classification
  colorInfo <- data.frame(MHclassi)[,which(colnames(MHclassi) == paste("node", nodeName, sep = ""))]
  colFun <- function(x){ifelse(x == "A", cols[1], ifelse(x == "B", cols[2], ifelse(x == "C", cols[3], x)))}
  colorBy <- colFun(colorInfo)

  colorList <- rep(list(list()), length(allTerminalNodes))
  colorList <- setNames(colorList, as.character(allTerminalNodes))
  for(entry in terminalNodes){
    colorList[[which(names(colorList) == entry)]] <- colorBy
  }
  for(entry in otherTerminalNodes){
    colorList[[which(names(colorList) == entry)]] <- "black"
  }
  print(paste("Colored by Node", nodeName))
  return(colorList)
}
