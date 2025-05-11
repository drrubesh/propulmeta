#' Cumulative Meta-Analysis Plot (Chronological Support)
#'
#' @param object A `meta_prop`, `meta_ratio`, or `meta_mean` object.
#' @param arrange_by Either "study_order" (default) or "year".
#' @param save_as One of "viewer", "pdf", or "png".
#' @param filename Optional file name for export.
#' @param width, height Plot dimensions in inches.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return A cumulative meta-analysis plot rendered or saved.
#' @export
plot_cumulative_meta <- function(object,
                                 arrange_by = c("study_order", "year"),
                                 save_as = c("viewer", "pdf", "png"),
                                 filename = NULL,
                                 width = 10,
                                 height = 8,
                                 ...) {
  arrange_by <- match.arg(arrange_by)
  save_as <- match.arg(save_as)

  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("❌ Only supports meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  meta_obj <- object$meta
  if (!inherits(meta_obj, "meta")) {
    stop("❌ meta field not found in object.", call. = FALSE)
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
  cum_obj <- tryCatch(meta::metacum(meta_obj), error = function(e) {
    stop("❌ Cumulative meta-analysis failed: ", e$message)
  })

  # Device handling
  original_device <- grDevices::dev.cur()

  # Export logic
  if (save_as != "viewer") {
    if (is.null(filename)) {
      ext <- switch(save_as, pdf = "pdf", png = "png")
      filename <- paste0("cumulative_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
    }

    if (save_as == "pdf") {
      grDevices::pdf(filename, width = width, height = height)
    } else if (save_as == "png") {
      grDevices::png(filename, width = width, height = height, units = "in", res = 300)
    }
  }

  # Plot the cumulative object
  plot(cum_obj, ...)

  # Close file device if needed
  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Cumulative plot saved as '{filename}'"))
  } else {
    message("📊 Cumulative meta-analysis plot displayed in Viewer.")
  }

  invisible(TRUE)
}
