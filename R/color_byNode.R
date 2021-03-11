#' Helper function to to be used for the 'col' argument in the plot method for Raschtrees
#' Helps to color item parameter in profileplots by inner nodes and ETS Mantel-Haenszel classification
#'
#' @param nodeID An integer indicating the inner node after which the item paramter should be colored
#' @param color A character vector indicating the colors for items classified as A, B, or C
#'
#' @return A function that takes the argument 'purification'nodeID' and 'color' and can be used as a value for the argument 'terminal_panel' in plot.raschtree()
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' rt <- raschtree(resp ~ age + gender + motivation, data = DIFSim, verbose = TRUE)
#' rt_MH <- add_mantelHaenszel(rt, purification = "iterative")
#' plot(rt_MH, terminal_panel = color_byNode(nodeID = 1, color = c("blue", "orange", "darkgreen")))
#' }
#'
#' @export
color_byNode <- function(nodeID, color = c("green", "orange", "red")) {
  return_colorFun <- function(object, ...){
    psychotree::node_profileplot(object, col = create_colorList(object, nodeID = nodeID, cols = color, ...))
  }
  class(return_colorFun) <- "grapcon_generator"
  return(return_colorFun)
}
#' Helper function to create a list with colors for each terminal node
#'
#' @param object An object of class raschtree that has the mantelHaenszel statistic added to it.
#' @param nodeID An integer indicating the inner node after which the item paramter should be colored
#' @param cols A named character vector indicating the
#'
#' @return A list named after the terminal nodes containing the colors of the item parameter based on ETS Mantel-Haenszel classification
create_colorList <- function(object, nodeID, cols){
  # get info on end nodes
  nodes <- partykit::node_party(object) # where the splits are
  innerNodes <- get_innerNodes(nodes)
  whichID <- which(sapply(innerNodes, function(x) x$id) == nodeID)
  terminalNodes <- get_terminalNodes(innerNodes[[whichID]])
  allTerminalNodes <- get_terminalNodes(nodes)
  otherTerminalNodes <- allTerminalNodes[!allTerminalNodes %in% terminalNodes]

  # color
  MHclassi <- object$info$mantelHaenszel$classification
  colorInfo <- data.frame(MHclassi)[,which(colnames(MHclassi) == paste("node", nodeID, sep = ""))]
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
  print(paste("Colored by Node", nodeID))
  return(colorList)
}

