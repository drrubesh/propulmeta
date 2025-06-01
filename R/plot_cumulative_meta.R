#' Cumulative Meta-Analysis Plot (Chronological Support)
#'
#' @param object A `meta_prop`, `meta_ratio`, or `meta_mean` object.
#' @param arrange_by Either "study_order" (default) or "year".
#' @param save_as One of "viewer", "pdf", or "png".
#' @param filename Optional file name for export.
#' @param height Height of the plot in inches.
#' @param width, height Plot dimensions in inches.
#' @param ... Additional arguments passed to plot().
#'
#' @return A cumulative meta-analysis plot rendered or saved.
#' @export
plot_cumulative_meta <- function(object,
                                 arrange_by = c("study_order", "year"),
                                 save_as = c("viewer", "pdf", "png"),
                                 filename = NULL,
                                 width = NULL,
                                 height = NULL,
                                 ...) {
  arrange_by <- match.arg(arrange_by)
  save_as <- match.arg(save_as)

  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("Only supports meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  meta_obj <- object$meta
  if (!inherits(meta_obj, "meta")) {
    stop("Meta-analysis object not found in 'meta' field.", call. = FALSE)
  }

  # Reorder by year if specified
  if (arrange_by == "year" && !is.null(meta_obj$year)) {
    ord <- order(meta_obj$year)
    meta_obj <- meta::update.meta(
      meta_obj,
      studlab = meta_obj$studlab[ord],
      TE = meta_obj$TE[ord],
      seTE = meta_obj$seTE[ord],
      year = meta_obj$year[ord]
    )
  }

  # Compute cumulative meta-analysis
  cum_obj <- tryCatch(
    meta::metacum(meta_obj),
    error = function(e) {
      stop("Cumulative meta-analysis failed: ", e$message, call. = FALSE)
    }
  )
  # Dynamic sizing
  k <- length(cum_obj$studlab)
  sizing <- .auto_plot_sizing(k, height = height, width = width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  # Identify valid studies: skip early k=1, k=2 studies
  valid_idx <- which(cum_obj$k >= 3)

  if (length(valid_idx) == 0) {
    stop("No valid cumulative meta-analysis estimates to plot (need at least 3 studies).")
  }

  if (is.null(filename) && save_as != "viewer") {
    ext <- switch(save_as, pdf = "pdf", png = "png")
    filename <- paste0("cumulative_meta_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
  }

  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  } else if (interactive()) {
    try(grDevices::dev.off(), silent = TRUE)  # Try closing broken device
    grDevices::dev.new(width = width, height = height)  # Open fresh one
  }

  # Plot the cumulative object (only valid studies)
  meta::forest(cum_obj,
               subset = valid_idx,
               fs.study = fontsize,
               fs.ci = fontsize,
               xlab = if ("meta_prop" %in% class(object)) "Proportion (%)" else NULL,
               pscale = if ("meta_prop" %in% class(object)) 100 else 1,
               ...
               )

  if (save_as != "viewer") {
    grDevices::dev.off()
    message(sprintf("Cumulative meta plot saved as '%s' in the working directory.", filename))
  } else {
    message("Cumulative meta plot displayed in Plots pane Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
    if (k > 40) message("Plot may exceed viewer margins. Export for full view.")
  }

  invisible(TRUE)
}
