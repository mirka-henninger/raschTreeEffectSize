#' Helper function to display the ETS Mantel-Haenszel classification summary in each inner node of the Raschtree
#'
#' @param purification A character indicating the type of purification ("none", "2step", "iterative"
#' @return A function that takes the argument purification and can be used as a value for the argument 'inner_panel' in plot.raschtree()
#'
#' @examples
#' data("DIFSim", package = "psychotree") # create artificial data
#' rt <- raschtree(resp ~ age + gender + motivation, data = DIFSim, verbose = TRUE) # fit Raschtree model
#' plot(rt, inner_panel = show_ETSMH(purification = "iterative")) # plot Raschtree with summary of ETS classification for each inner node
#'
#' @export
show_ETSMH <- function(purification){
  print(paste("Purification:", purification, sep = " "))
  inner_show_ETSMH <- function(obj, id = TRUE, pval = TRUE, abbreviate = FALSE, fill = "white", gp = gpar())
  {
    # calculate ETS-MH
    MH <- get_mantelHaenszel(obj, purification = purification, by = "type")
    tabMH <- data.frame(apply(MH$classification, 2, table))
    tabMH <- apply(tabMH, 2, function(x) paste(rownames(tabMH), ":", x, "; ", sep = ""))
    tabMH <- apply(tabMH, 2, function(x) paste(c(rep("_", 14), "\nETS-MH: ", x), collapse = ""))

    # original node_inner function
    meta <- obj$data
    nam <- names(obj)

    extract_label <- function(node) {
      if(is.terminal(node)) return(rep.int("", 2L))

      varlab <- character_split(split_node(node), meta)$name
      # ETSMHlab <- character_split(split_node(node), tabMH)$name
      if(abbreviate > 0L) varlab <- abbreviate(varlab, as.integer(abbreviate))

      ## FIXME: make more flexible rather than special-casing p-value
      if(pval) {
        nullna <- function(x) is.null(x) || is.na(x)
        pval <- suppressWarnings(try(!nullna(info_node(node)$p.value), silent = TRUE))
        pval <- if(inherits(pval, "try-error")) FALSE else pval
      }
      if(pval) {
        pvalue <- node$info$p.value
        plab <- ifelse(pvalue < 10^(-3L),
                       paste("p <", 10^(-3L)),
                       paste("p =", round(pvalue, digits = 3L)))
      } else {
        plab <- ""
      }
      tabMH <- tabMH[[paste("node", nam[id_node(node)], sep = "")]]
      return(c(varlab, plab, tabMH))
    }

    maxstr <- function(node) {
      lab <- extract_label(node)
      klab <- if(is.terminal(node)) "" else unlist(lapply(kids_node(node), maxstr))
      lab <- c(lab, klab)
      lab <- unlist(lapply(lab, function(x) strsplit(x, "\n")))
      lab <- lab[which.max(nchar(lab))]
      if(length(lab) < 1L) lab <- ""
      return(lab)
    }

    nstr <- maxstr(node_party(obj))
    if(nchar(nstr) < 6) nstr <- "aAAAAa"

    ### panel function for the inner nodes
    rval <- function(node) {
      pushViewport(viewport(gp = gp, name = paste("node_inner", id_node(node), "_gpar", sep = "")))
      node_vp <- viewport(
        x = unit(0.5, "npc"),
        y = unit(0.5, "npc"),
        width = unit(1, "strwidth", nstr) * 1.3,
        height = unit(5, "lines"),
        name = paste("node_inner", id_node(node), sep = ""),
        gp = gp
      )
      pushViewport(node_vp)

      xell <- c(seq(0, 0.2, by = 0.01),
                seq(0.2, 0.8, by = 0.05),
                seq(0.8, 1, by = 0.01))
      yell <- sqrt(xell * (1-xell))

      lab <- extract_label(node)
      fill <- rep(fill, length.out = 2L)

      grid.polygon(x = unit(c(xell, rev(xell)), "npc"),
                   y = unit(c(yell, -yell)+0.5, "npc"),
                   gp = gpar(fill = fill[1]))

      ## FIXME: something more general instead of pval ?
      grid.text(lab[1L], y = unit(3 + 0.5 * (lab[2L] != "") + 0.5 * (lab[3L] != ""), "lines"))
      if(lab[2L] != "") grid.text(lab[2L], y = unit(2.5 + 0.5 * (lab[3L] != ""), "lines"))
      if(lab[3L] != "") grid.text(lab[3L], y = unit(2, "lines"))

      if(id) {
        nodeIDvp <- viewport(x = unit(0.5, "npc"), y = unit(1, "npc"),
                             width = max(unit(1, "lines"), unit(1.3, "strwidth", nam[id_node(node)])),
                             height = max(unit(1, "lines"), unit(1.3, "strheight", nam[id_node(node)])))
        pushViewport(nodeIDvp)
        grid.rect(gp = gpar(fill = fill[2]))
        grid.text(nam[id_node(node)])
        popViewport()
      }
      upViewport(2)
    }
    return(rval)
  }
  class(inner_show_ETSMH) <- "grapcon_generator"
  return(inner_show_ETSMH)
}


