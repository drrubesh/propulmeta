#' DOI Plot for Meta-Analyses with Fewer Than 10 Studies
#'
#' Displays a DOI plot and LFK index using `metasens::doiplot()`. Recommended when fewer than 10 studies are included.
#'
#' @param object A meta-analysis object of class `meta_ratio`, `meta_mean`, or `meta_prop`.
#' @param save_as "viewer", "pdf", or "png". Default is "viewer".
#' @param filename Optional export filename.
#' @param width,height Plot size in inches. Defaults: 7x7.
#' @param ... Additional arguments passed to `metasens::doiplot()`.
#'
#' @return Invisible NULL. Displays or saves the DOI plot.
#' @export
doi_plot <- function(object,
                     save_as = c("viewer", "pdf", "png"),
                     filename = NULL,
                     width = 7,
                     height = 7,
                     ...) {
  save_as <- match.arg(save_as)

  if (!requireNamespace("metasens", quietly = TRUE)) {
    stop("The 'metasens' package is required. Please install it using install.packages('metasens').", call. = FALSE)
  }

  meta_obj <- object$meta
  k <- meta_obj$k

  message("\n📈 DOI Plot for Publication Bias")
  message("--------------------------------")
  message("Total studies included: ", k)

  if (k >= 10) {
    message("⚠️  DOI plots are primarily used for meta-analyses with fewer than 10 studies.")
    message("➡️  Use `publication_bias()` for larger analyses.\n")
    return(invisible(NULL))
  }

  TE <- meta_obj$TE
  seTE <- meta_obj$seTE

  # Handle export
  if (save_as != "viewer") {
    if (is.null(filename)) {
      ext <- switch(save_as, pdf = "pdf", png = "png")
      filename <- paste0("doi_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
    }
    if (save_as == "pdf") {
      grDevices::pdf(filename, width = width, height = height)
    } else {
      grDevices::png(filename, width = width, height = height, units = "in", res = 300)
    }
  }

  # Plot
  tryCatch({
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mar = c(5, 5, 4, 2))
    metasens::doiplot(TE, seTE, xlab = "Effect Size", lfkindex = TRUE, ...)
  }, error = function(e) {
    message("❌ Failed to generate DOI plot: ", e$message)
  })

  # Close export device if used
  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ DOI plot saved as '{filename}'"))
  } else {
    message("📊 DOI plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` to export.")
  }

  invisible(NULL)
}
