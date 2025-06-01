#' Influence Plot for Meta-Analyses
#'
#' Draws a leave-one-out influence forest plot.
#'
#' @param object A `meta_ratio`, `meta_mean`, or `meta_prop` object.
#' @param layout Layout style for forest plot (e.g., "RevMan5", "default").
#' @param save_as "viewer", "pdf", or "png".
#' @param filename Optional file name if saving.
#' @param width,height Dimensions for export (inches).
#' @param ... Additional arguments to pass to meta::forest
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
    stop("Object must be of class meta_prop, meta_ratio, or meta_mean.", call. = FALSE)
  }

  infl_obj <- if (inherits(object, "meta_prop")) {
    object$influence.meta
  } else {
    object$influence.analysis
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
  } else if (interactive()) {
    try(grDevices::dev.off(), silent = TRUE)  # Try closing broken device
    grDevices::dev.new(width = width, height = height)  # Open fresh one
  }


  # 📊 Plot -- wrap in print() to force plot
  meta::forest(
    x = infl_obj,
    smlab = "Leave-One-Out Meta-Analysis",
    just.addcols = "right",
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
    message(sprintf("Influence plot saved as '%s' in the working directory.", filename))
  } else {
    message("Influence plot displayed in Plots pane Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
    if (k > 40) message("Plot may exceed viewer margins. Export for full view.")
  }

  invisible(TRUE)
}

