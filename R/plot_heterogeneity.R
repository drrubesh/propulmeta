#' Heterogeneity Plot (I² or Tau²) from Leave-One-Out Meta-Analysis
#'
#' @param object A `meta_prop`, `meta_ratio`, or `meta_mean` object
#' @param stat Statistic to plot: either `"I2"` (default) or `"tau2"`
#' @param save_as "viewer", "pdf", or "png" (default = "viewer")
#' @param filename Optional filename for saving
#' @param width,height Plot dimensions (in inches)
#' @param ... Additional arguments passed to meta::plot()
#'
#' @return A forest-style heterogeneity plot rendered or saved
#' @export
plot_heterogeneity <- function(object,
                               stat = c("I2", "tau2"),
                               save_as = c("viewer", "pdf", "png"),
                               filename = NULL,
                               width = 10,
                               height = NULL,
                               ...) {
  stat <- match.arg(stat)
  save_as <- match.arg(save_as)

  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("❌ Only supports meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  # Get metainf object
  infl_obj <- if ("meta_prop" %in% class(object)) {
    object$influence.meta
  } else {
    object$influence.analysis
  }

  if (!inherits(infl_obj, "metainf")) {
    stop("❌ No metainf object found in influence.meta or influence.analysis.")
  }

  k <- infl_obj$k
  if (is.null(height)) {
    height <- min(45, max(12, 0.35 * k))
  }

  # Save if needed
  if (save_as != "viewer") {
    if (is.null(filename)) {
      ext <- switch(save_as, pdf = "pdf", png = "png")
      filename <- paste0("heterogeneity_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
    }

    if (save_as == "pdf") {
      grDevices::pdf(filename, width = width, height = height)
    } else if (save_as == "png") {
      grDevices::png(filename, width = width, height = height, units = "in", res = 300)
    }
  } else {
    while (dev.cur() > 1) grDevices::dev.off()
    message("📊 Heterogeneity plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` to export.")
  }

  # Plot using meta's internal function
  plot(infl_obj, stat = stat, ...)

  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Heterogeneity plot saved as '{filename}'"))
  }

  invisible(TRUE)
}
