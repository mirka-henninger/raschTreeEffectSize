#' Helper function to to be used for the 'col' argument in the plot method for Raschtrees
#' Helps to color item parameter in profileplots by inner nodes and ETS Mantel-Haenszel classification
#'
#' @param nodeID An integer indicating the inner node after which the item paramter should be colored
#' @param classColor A character vector indicating the colors for items classified as A, B, or C
#' @param classSize A character vector indicating the size for items classified as A, B, or C
#' @param panelColor A character vector indicating the colors of the background panels for left and right terminal nodes of the inner node by which items are colored
#'
#' @return A function that takes the argument 'purification'nodeID' and 'color' and can be used as a value for the argument 'terminal_panel' in plot.raschtree()
color_byNode <- function(nodeID, classColor, classSize, panelColor) {
  return_colorFun <- function(object, ...){
    returnFun <-
      node_profileplot(object,
                       col = create_colorList(object, nodeID = nodeID, pars = classColor, default = "black"),
                       cex = create_colorList(object, nodeID = nodeID, pars = classSize, default = .4),
                       border = "black",
                       bg = create_bgList(object, nodeID = nodeID, backgroundCols = panelColor),
                       cf_fun = anchor_pars(nodeID))
    return(returnFun)
  }
  message(paste("Colored by Node", nodeID))
  class(return_colorFun) <- "grapcon_generator"
  return(return_colorFun)
}
#' Helper function to create a list with colors and point sizes for each terminal node
#'
#' @param object An object of class raschtree that has the mantelHaenszel statistic added to it.
#' @param nodeID An integer indicating the inner node after which the item paramter should be colored
#' @param pars A character vector of length three indicating the colors in which items classified as A/B/C should be displayed
#' @param default The default color for end nodes that are not colored
#'
#' @return A list named after the terminal nodes containing the colors of the item parameter based on ETS Mantel-Haenszel classification
create_colorList <- function(object, nodeID, pars, default){
  # check whether Delta-MH is saved in the Raschtree object
  if(is.null(object$info$mantelHaenszel))
    stop("No Mantel-Haenszel classification found. Please use the add_mantelHaenszel function to add Mantel-Haenszel effect size measures to the Raschtree object")

  # get IDs of inner nodes
  nodes <- partykit::node_party(object) # where the splits are
  innerNodes <- get_innerNodes(nodes)
  innerNodesIDs <- sapply(innerNodes, function(x) x$id)

  # check whether nodeID describes an inner node
  if(!(nodeID %in% innerNodesIDs)) stop(c("Coloring can only be executed based on ETS-MH of inner nodes (inner nodes: ", paste(as.character(innerNodesIDs), collapse = ", "), ")"))

  # get terminal nodes of nodeID and the remaining node IDs
  whichID <- which(innerNodesIDs == nodeID)
  terminalNodes <- get_terminalNodes(innerNodes[[whichID]])
  allTerminalNodes <- get_terminalNodes(nodes)
  otherTerminalNodes <- allTerminalNodes[!allTerminalNodes %in% terminalNodes]

  # create the color list
  MHclassi <- object$info$mantelHaenszel$classification
  colorInfo <- data.frame(MHclassi)[,which(colnames(MHclassi) == paste("node", nodeID, sep = ""))]
  colFun <- function(x){ifelse(x == "A", pars[1], ifelse(x == "B", pars[2], ifelse(x == "C", pars[3], x)))}
  colorBy <- colFun(colorInfo)

  colorList <- rep(list(list()), length(allTerminalNodes))
  colorList <- stats::setNames(colorList, as.character(allTerminalNodes))
  for(entry in terminalNodes){
    colorList[[which(names(colorList) == entry)]] <- colorBy
  }
  for(entry in otherTerminalNodes){
    colorList[[which(names(colorList) == entry)]] <- default
  }
  return(colorList)
}
#' Helper function to create a list with colors for left and right child node of the node by which A/B/C coloring is performed
#'
#' @param object An object of class raschtree that has the mantelHaenszel statistic added to it.
#' @param nodeID An integer indicating the inner node after which the item paramter should be colored
#' @param backgroundCols A character vector indicating the colors of the background panels for left and right terminal nodes of the inner node by which items are colored
#'
#' @return A list named after the terminal nodes containing the colors of the item parameter based on ETS Mantel-Haenszel classification
create_bgList <- function(object, nodeID, backgroundCols){
  # check whether Delta-MH is saved in the Raschtree object
  if(is.null(object$info$mantelHaenszel))
    stop("No Mantel-Haenszel classification found. Please use the add_mantelHaenszel function to add Mantel-Haenszel effect size measures to the Raschtree object")

  # get IDs of inner nodes
  nodes <- partykit::node_party(object) # where the splits are
  innerNodes <- get_innerNodes(nodes)
  innerNodesIDs <- sapply(innerNodes, function(x) x$id)

  # check whether nodeID describes an inner node
  if(!(nodeID %in% innerNodesIDs)) stop(c("Coloring can only be executed based on ETS-MH of inner nodes (inner nodes: ", paste(as.character(innerNodesIDs), collapse = ", "), ")"))

  # get terminal nodes of nodeID and the remaining node IDs
  whichID <- which(innerNodesIDs == nodeID)
  childrenLeft <- kids_node(innerNodes[[whichID]])[[1]]
  childrenRight <- kids_node(innerNodes[[whichID]])[[2]]
  terminalLeft <- get_terminalNodes(childrenLeft)
  terminalRight <- get_terminalNodes(childrenRight)

  bgList <- rep(list(list()), length(c(terminalLeft, terminalRight)))
  bgList <- stats::setNames(bgList, as.character(c(terminalLeft, terminalRight)))
  for(entry in terminalLeft){
    bgList[[which(names(bgList) == entry)]] <- backgroundCols[1]
  }
  for(entry in terminalRight){
    bgList[[which(names(bgList) == entry)]] <- backgroundCols[2]
  }
  return(bgList)
}
#' Helper function for anchoring items in the Rasch tree profile plot
#'
#' @param nodeID An integer indicating the inner node after which the item paramter should be colored
#'
#' @return function cf that will be used to extract anchored item parameter from the mob object
anchor_pars <- function(nodeID){
  return(function(object, nodes){
    # get IDs of inner nodes
    innerNodes <- get_innerNodes(partykit::node_party(object))
    innerNodesIDs <- sapply(innerNodes, function(x) x$id)

    # get terminal nodes of nodeID and the remaining node IDs
    whichID <- which(innerNodesIDs == nodeID)
    anchoringNodes <- get_terminalNodes(innerNodes[[whichID]])
    otherNodes <- nodes[!nodes %in% anchoringNodes]

    # use only no-DIF items in a specific node for anchoring
    MHclassi <- object$info$mantelHaenszel$classification
    anchorInfo <- data.frame(MHclassi)[,which(colnames(MHclassi) == paste("node", nodeID, sep = ""))]
    anchorItems <- which(anchorInfo == "A")
    message("Anchor-Items:"); message(paste(anchorItems, "- "))
    # only terminal nodes of the inner node are anchored, other nodes use sum-to-zero
    unchanged <- apply_to_models(object, otherNodes, FUN = function(x) coef(itempar(x, alias = TRUE, vcov = FALSE)))
    anchored <- apply_to_models(object, anchoringNodes, FUN = function(x) coef(itempar(x, ref = anchorItems, alias = TRUE, vcov = FALSE)))
    cf <- as.list(c(unchanged, anchored))
    cf <- cf[order(names(cf))]
    return(cf)
  })
}
