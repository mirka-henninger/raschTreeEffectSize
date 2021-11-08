#' Creates a raschtreeMH object based on the original Raschtree with additional Mantel-Haenszel effect size information
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#'
#' @return An object of type raschtreeMH with 'info' extended by a list named mantelHaenszel containing entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim,
#'                 stopfun = stopFun_mantelHaenszel(purification = "iterative"))
#' RT_MH <- add_mantelHaenszel(RT, purification = "iterative")
#' RT_MH$info$mantelHaenszel
#' plot(RT_MH, colorbyNode = 1)
#' }
#' @export
add_mantelHaenszel <- function(object, purification){
  # check whether object is of type Raschtree, modelparty, and party
  if(!(all(class(object) %in% c("raschtree", "modelparty", "party"))))
    stop("Object must be an Raschtree object (as returned from the raschtree function")
  object$info$mantelHaenszel <- get_mantelHaenszel(object, purification = purification, by = "type")
  class(object) <- c("raschtreeMH", class(object))
  return(object)
}
#' Plots the Mantel-Haenszel Raschtree
#'
#' @description Additional options to show the ETS-Mantel-Haenszel effect size classification in the inner nodes,
#' option to color the items in terminal nodes by A/B/C DIF classification based on inner nodes,
#' and option to turn on a background color so you can see which are the left and right terminal nodes for the inner nodes which items are colored
#'
#' @param x An object of type raschtreeMH with 'info' extended by a list named mantelHaenszel containing entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#' @param showETSMH A logical with default is TRUE: Should the ETS-Mantel-Haenszel classification be shown in each inner node?
#' @param colorbyNode An integer indicating the inner node after which the item paramter should be colored
#' @param ABCcolors A character vector of length three indicating the colors in which items classified as A/B/C should be displayed
#' @param ABCsize A vector of length three indicating the size in which items classified as A/B/C should be displayed
#' @param nodeBackground A character vector of length two indicating the background colors of the panels for which the items have been compared
#' @param ... arguments passed to the underlying functions, i.e., to mob_control for raschtree, and to the underlying predict and plot methods, respectively.
#' @return An object of S3 class "raschtree" inheriting from class "modelparty".
#'
#' @export
plot.raschtreeMH <- function(x,
                             showETSMH = TRUE,
                             colorbyNode = NULL,
                             ABCcolors = c("#99e1e3", "#ba6100", "#6c0200"),
                             ABCsize = c(.4,.75,.9),
                             nodeBackground = c("#eee3af", "#aeaeae"), ...){

  # define inner and terminal panels
  inner_panel <- partykit::node_inner
  if(showETSMH == TRUE) inner_panel <- show_ETSMH

  terminal_panel <- node_profileplot
  if(!is.null(colorbyNode)) terminal_panel <- color_byNode(nodeID = colorbyNode,
                                                           classColor = ABCcolors,
                                                           classSize = ABCsize,
                                                           panelColor = nodeBackground)

  # plot raschtreeMH based on the original raschtree
  partykit::plot.modelparty(x,
                            terminal_panel = terminal_panel,
                            inner_panel = inner_panel, ...)
}
