#' Influence Plot for Meta-Analyses using meta::forest.metainf
#'
#' Draws a leave-one-out forest plot using meta::forest(). Only supports objects with a metainf object.
#'
#' @param object A `meta_ratio`, `meta_mean`, or `meta_prop` object.
#' @param layout Character. Layout style for forest plot (e.g., "RevMan5", "default").
#' @param save_as One of "viewer", "pdf", or "png". Controls whether to print or export the plot.
#' @param filename Optional file name if saving.
#' @param width,height Dimensions for the exported plot (in inches).
#' @param ... Additional arguments passed to `meta::forest()`.
#'
#' @return A forest plot rendered or saved to file.
#' @export
plot_influence <- function(object,
                           layout = "RevMan5",
                           save_as = c("viewer", "pdf", "png"),
                           filename = NULL,
                           width = NULL,
                           height = NULL,
                           ...) {
  save_as <- match.arg(save_as)

  if (!inherits(object, c("meta_ratio", "meta_mean", "meta_prop"))) {
    stop("❌ Object must be of class meta_ratio, meta_mean, or meta_prop.")
  }

  # Find metainf object
  infl_obj <- if ("meta_prop" %in% class(object)) {
    object$influence.meta
  } else {
    object$influence.analysis
  }

  if (!inherits(infl_obj, "metainf")) {
    stop("❌ No valid 'metainf' object found in influence.meta or influence.analysis.")
  }

  # Autosize based on number of studies
  k <- infl_obj$k
  sizing <- .auto_plot_sizing(k, height = height, width = width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  # File name
  if (is.null(filename) && save_as != "viewer") {
    ext <- switch(save_as, pdf = "pdf", png = "png")
    filename <- paste0("influence_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
  }

  # Open export device if needed
  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  } else {

    message("📊 Influence plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
  }

  # Forest plot
  meta::forest(
    x = infl_obj,
    layout = tolower(layout),
    smlab = "Leave-One-Out Meta-Analysis",
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.pval.Q = TRUE,
    just.addcols = "right",
    digits = 2,
    squaresize = 0.5,
    col.square = "red",
    col.square.lines = "black",
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

  # Close export device
  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Influence plot saved to '{filename}'"))
  }

  invisible(TRUE)
}
