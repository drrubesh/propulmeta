#' Influence Plot for Meta-Analyses
#'
#' Draws a leave-one-out influence forest plot.
#'
#' @param object A `meta_ratio`, `meta_mean`, or `meta_prop` object.
#' @param layout Layout style for forest plot (e.g., "RevMan5", "default").
#' @param save_as "viewer", "pdf", or "png".
#' @param filename Optional file name if saving.
#' @param width,height Dimensions for export (inches).
#' @param ... Additional arguments to pass to meta::forest.metainf().
#'
#' @return A forest plot.
#' @export
plot_influence <- function(object,
                           layout = "RevMan5",
                           save_as = c("viewer", "pdf", "png"),
                           filename = NULL,
                           width = NULL,
                           height = NULL,
                           ...) {
  save_as <- match.arg(save_as)

  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("Object must be of class meta_prop, meta_ratio, or meta_mean.")
  }

  # Properly select influence object
  infl_obj <- if ("meta_prop" %in% class(object)) {
    object$influence.meta
  } else {
    object$influence.analysis
  }

  if (is.null(infl_obj) || !inherits(infl_obj, "metainf")) {
    stop("Influence analysis failed: No valid leave-one-out estimates.")
  }

  k <- length(infl_obj$studlab)

  sizing <- .auto_plot_sizing(k, height = height, width = width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  if (is.null(filename) && save_as != "viewer") {
    ext <- switch(save_as, pdf = "pdf", png = "png")
    filename <- paste0("influence_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
  }

  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  }

  # Correct forest call
  meta::forest.metainf(
    x = infl_obj,
    layout = tolower(layout),
    smlab = "Leave-One-Out Meta-Analysis",
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.pval.Q = TRUE,
    just.addcols = "right",
    digits = 2,
    squaresize = 0.5,
    col.bg = "red",
    col.border = "black",
    col.diamond = "black",
    col.diamond.lines = "black",
    fs.study = fontsize,
    fs.ci = fontsize,
    fs.hetstat = fontsize,
    fs.axis = fontsize,
    xlab = if ("meta_prop" %in% class(object)) "Proportion (%)" else NULL,
    pscale = if ("meta_prop" %in% class(object)) 100 else 1,
    ...
  )

  if (save_as != "viewer") {
    grDevices::dev.off()
    message(paste("Influence plot saved as", filename))
  } else {
    message("Influence plot displayed in Viewer. Use `save_as = 'pdf'` or 'png' to export.")
  }

  invisible(TRUE)
}
