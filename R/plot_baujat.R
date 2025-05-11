#' Baujat Plot for Meta-Analysis
#'
#' Displays the contribution of each study to heterogeneity (Q) vs. influence on pooled estimate.
#'
#' @param object A `meta_prop`, `meta_ratio`, or `meta_mean` object.
#' @param save_as One of "viewer", "pdf", or "png". Default is "viewer".
#' @param filename Optional file name (used when save_as is "pdf" or "png").
#' @param width Width of plot in inches (default = 10).
#' @param height Height of plot in inches (default = 8).
#' @param ... Additional arguments passed to `meta::baujat()`.
#'
#' @return A Baujat plot rendered or saved.
#' @export
plot_baujat <- function(object,
                        save_as = c("viewer", "pdf", "png"),
                        filename = NULL,
                        width = 10,
                        height = 8,
                        ...) {
  save_as <- match.arg(save_as)

  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("❌ Only supports meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  m <- object$meta
  if (!inherits(m, "meta")) {
    stop("❌ object$meta must be of class 'meta'", call. = FALSE)
  }

  original_device <- grDevices::dev.cur()

  if (save_as != "viewer") {
    if (is.null(filename)) {
      ext <- switch(save_as, pdf = "pdf", png = "png")
      filename <- paste0("baujat_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
    }

    if (save_as == "pdf") {
      grDevices::pdf(filename, width = width, height = height)
    } else if (save_as == "png") {
      grDevices::png(filename, width = width, height = height, units = "in", res = 300)
    }
  }

  meta::baujat(m, ...)

  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Baujat plot saved as '{filename}'"))
  } else {
    message("📊 Baujat plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` to export.")
  }

  invisible(TRUE)
}
