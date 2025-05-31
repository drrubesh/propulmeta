#' Heterogeneity Plot (I2 or Tau2) from Leave-One-Out Meta-Analysis
#'
#' @param object A `meta_prop`, `meta_ratio`, or `meta_mean` object.
#' @param stat "I2" or "tau2" (default "I2").
#' @param save_as "viewer", "pdf", or "png".
#' @param filename Optional file name if saving.
#' @param width,height Dimensions for export (inches).
#' @param ... Additional arguments for plot().
#'
#' @return A heterogeneity plot.
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
    stop("Only supports meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  # Select influence object
  infl_obj <- if ("meta_prop" %in% class(object)) {
    object$influence.meta
  } else {
    object$influence.analysis
  }

  if (is.null(infl_obj) || !inherits(infl_obj, "metainf")) {
    stop("No valid leave-one-out meta-analysis object found for plotting heterogeneity.")
  }

  study_labels <- infl_obj$studlab
  heterogeneity_values <- if (stat == "I2") infl_obj$I2.leave1out else infl_obj$tau2.leave1out

  if (all(is.na(heterogeneity_values)) || length(heterogeneity_values) == 0) {
    stop("No valid heterogeneity estimates to plot.")
  }

  k <- length(heterogeneity_values)
  if (is.null(height)) {
    height <- min(45, max(12, 0.35 * k))
  }

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
  }

  plot(
    seq_along(heterogeneity_values),
    heterogeneity_values,
    type = "b",
    pch = 19,
    xlab = "Study Removed",
    ylab = ifelse(stat == "I2", "I² (%)", "Tau²"),
    axes = FALSE,
    ...
  )
  axis(1, at = seq_along(study_labels), labels = study_labels, las = 2, cex.axis = 0.7)
  axis(2)
  box()

  if (save_as != "viewer") {
    grDevices::dev.off()
    message(paste("Heterogeneity plot saved as", filename))
  } else {
    message("Heterogeneity plot displayed in Viewer. Use `save_as = 'pdf'` or 'png' to export.")
  }

  invisible(TRUE)
}
