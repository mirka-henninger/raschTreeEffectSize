#' Recursive partitioning (also known as trees) based on Rasch models.
#' for more details see psychotree::raschtree
#' @param formula A symbolic description of the model to be fit. This should be of type y ~ x1 + x2 where y should be a binary 0/1 item response matrix and x1 and x2 are used as partitioning variables.
#' @param data a data frame containing the variables in the model.
#' @param na.action a function which indicates what should happen when the data contain missing values (NAs).
#' @param reltol arguments passed via raschmodel to optim.
#' @param deriv character. Which type of derivatives should be used for computing gradient and Hessian matrix? Analytical with sum algorithm ("sum"), analytical with difference algorithm ("diff", faster but numerically unstable), or numerical. Passed to raschmodel.
#' @param maxit arguments passed via raschmodel to optim.
#' @param ... arguments passed to the underlying functions, i.e., to mob_control for raschtree, and to the underlying predict and plot methods, respectively.
## high-level convenience interface to mob()
raschtree <- function(formula, data, na.action,
  reltol = 1e-10, deriv = c("sum", "diff", "numeric"), maxit = 100L, ...)
{
  ## keep call
  cl <- match.call(expand.dots = TRUE)

  ## use dots for setting up mob_control
  control <- mob_control(...)
  control$ytype <- "matrix"

  ## control options for raschfit
  raschcontrol <- list(reltol = reltol, deriv = deriv, maxit = maxit)

  ## call mob
  m <- match.call(expand.dots = FALSE)
  m$fit <- raschfit
  m$control <- control
  for(n in names(raschcontrol)) if(!is.null(raschcontrol[[n]])) m[[n]] <- raschcontrol[[n]]
  if("..." %in% names(m)) m[["..."]] <- NULL
  m[[1L]] <- as.name("mob")
  rval <- eval(m, parent.frame())

  ## extend class and keep original call
  rval$info$call <- cl
  class(rval) <- c("raschtree", class(rval))
  return(rval)
}

## glue code for calling raschmodel()
raschfit <- function(y, x = NULL, start = NULL, weights = NULL, offset = NULL, ...,
  estfun = FALSE, object = FALSE)
{
  if(!(is.null(x) || NCOL(x) == 0L)) warning("x not used")
  if(!is.null(offset)) warning("offset not used")
  rval <- raschmodel(y, weights = weights, start = start, ..., hessian = object | estfun)
  rval <- list(
    coefficients = rval$coefficients,
    objfun = -rval$loglik,
    estfun = if(estfun) estfun.raschmodel(rval) else NULL,
    object = if(object) rval else NULL
  )
  return(rval)
}

## methods
print.raschtree <- function(x,
  title = "Rasch tree", objfun = "negative log-likelihood", ...)
{
  partykit::print.modelparty(x, title = title, objfun = objfun, ...)
}

predict.raschtree <- function(object, newdata = NULL,
  type = c("probability", "cumprobability", "mode", "median", "mean",
    "category-information", "item-information", "test-information", "node"),
  personpar = 0, ...)
{
  ## type of prediction
  type <- match.arg(type)

  ## nodes can be handled directly
  if(type == "node") return(partykit::predict.modelparty(object, newdata = newdata, type = "node", ...))

  ## get default newdata otherwise
  if(is.null(newdata)) newdata <- model.frame(object)

  ## predictions inherited from the basic *model object, evaluated at one person parameter
  partykit::predict.modelparty(object, newdata = newdata,
    type = function(obj) predict(obj, newdata = personpar[1L], type = type, ...))
}

apply_to_models <- function(object, node = NULL, FUN = NULL, drop = FALSE, ...) {
  if(is.null(node)) node <- nodeids(object, terminal = FALSE)
  if(is.null(FUN)) FUN <- function(object, ...) object
  rval <- if("object" %in% object$info$control$terminal) {
    nodeapply(object, node, function(n) FUN(info_node(n)$object))
  } else {
    lapply(refit.modelparty(object, node, drop = FALSE), FUN)
  }
  names(rval) <- node
  if(drop & length(node) == 1L) rval <- rval[[1L]]
  return(rval)
}

itempar.raschtree <- function(object, node = NULL, ...)
{
  ids <- if(is.null(node)) nodeids(object, terminal = TRUE) else node
  myitempar <- function(obj) coef(itempar(obj, ...))
  if(length(ids) == 1L) {
    apply_to_models(object, node = ids, FUN = myitempar, drop = TRUE)
  } else {
    do.call("rbind", apply_to_models(object, node = ids, FUN = myitempar, drop = FALSE))
  }
}

plot.raschtree <- function(x, type = c("profile", "regions"), terminal_panel = NULL,
  tp_args = list(...), tnex = 2L, drop_terminal = TRUE, ...)
{
  if(!is.null(terminal_panel)) {
    if(!missing(type)) warning("only one of 'type' and 'terminal_panel' should be specified")
  } else {
    terminal_panel <- switch(match.arg(type),
      "regions" = node_regionplot,
      "profile" = node_profileplot)
  }
  partykit::plot.modelparty(x, terminal_panel = terminal_panel,
    tp_args = tp_args, tnex = tnex, drop_terminal = drop_terminal, ...)
}
