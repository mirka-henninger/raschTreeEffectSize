#' Creates a raschtreeMH object based on the original Raschtree with additional Mantel-Haenszel effect size information
#'
#' @param object An object of type Raschtree
#' @param purification A character indicating the type of purification ("none", "2step", "iterative")
#'
#' @return An object of type raschtreeMH with 'info' extended by a list named mantelhaenszel containing entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#'
#' @examples
#' \dontrun{
#' data("DIFSim", package = "psychotree")
#' RT <- raschtree(resp ~ age + gender + motivation, data = DIFSim,
#'                 stopfun = stopfun_mantelhaenszel(purification = "iterative"))
#' RT_MH <- add_mantelhaenszel(RT, purification = "iterative")
#' RT_MH$info$mantelhaenszel
#' plot(RT_MH, color_by_node = 1)
#' }
#' @export
add_mantelhaenszel <- function(object, purification){
  # check whether object is of type Raschtree, modelparty, and party
  if(!(all(class(object) %in% c("raschtree", "modelparty", "party"))))
    stop("Object must be an Raschtree object (as returned from the raschtree function")
  object$info$mantelhaenszel <- get_mantelhaenszel(object, purification = purification, by = "type")
  class(object) <- c("raschtreeMH", class(object))
  return(object)
}
#' Plots the Mantel-Haenszel Raschtree
#'
#' @description Additional options to show the ETS-Mantel-Haenszel effect size classification in the inner nodes,
#' option to color the items in terminal nodes by A/B/C DIF classification based on inner nodes,
#' and option to turn on a background color so you can see which are the left and right terminal nodes for the inner nodes which items are colored
#'
#' @param x An object of type raschtreeMH with 'info' extended by a list named mantelhaenszel containing entries Mantel-Haenszel effect size, classification, purification type, and purificationCounter
#' @param show_classification A logical with default is TRUE: Should the ETS-Mantel-Haenszel classification be shown in each inner node?
#' @param color_by_node An integer indicating the inner node after which the item paramter should be colored
#' @param ABC_colors A character vector of length three indicating the colors in which items classified as A/B/C should be displayed
#' @param ABC_size A vector of length three indicating the size in which items classified as A/B/C should be displayed
#' @param node_background A character vector of length two indicating the background colors of the panels for which the items have been compared
#' @param ... arguments passed to the underlying functions, i.e., to mob_control for raschtree, and to the underlying predict and plot methods, respectively.
#' @return An object of S3 class "raschtree" inheriting from class "modelparty".
#'
#' @export
plot.raschtreeMH <- function(x,
                             show_classification = TRUE,
                             color_by_node = NULL,
                             ABC_colors = c("#99e1e3", "#ba6100", "#6c0200"),
                             ABC_size = c(.4,.75,.9),
                             node_background = c("#eee3af", "#aeaeae"), ...){

  # define inner and terminal panels
  inner_panel <- partykit::node_inner
  if(show_classification == TRUE) inner_panel <- show_mantelhaenszel

  terminal_panel <- node_profileplot
  if(!is.null(color_by_node)) terminal_panel <- color_by_node(node_ID = color_by_node,
                                                              class_color = ABC_colors,
                                                              class_size = ABC_size,
                                                              panel_color = node_background)

  # plot raschtreeMH based on the original raschtree
  partykit::plot.modelparty(x,
                            terminal_panel = terminal_panel,
                            inner_panel = inner_panel, ...)
}
