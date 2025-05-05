#' DOI Plot for Meta-Analyses with Fewer Than 10 Studies
#'
#' Displays a DOI plot and LFK index using metasens::doiplot(). Recommended for meta-analyses with <10 studies.
#'
#' @param object A meta-analysis object of class `meta_ratio`, `meta_mean`, or `meta_prop`.
#' @param ... Additional arguments passed to `metasens::doiplot()`.
#'
#' @return Invisible NULL. Displays a plot to the active graphics device.
#' @export
doi_plot <- function(object, ...) {
  # Install required packages quietly if not already installed
  if (!requireNamespace("metasens", quietly = TRUE)) {
    install.packages("metasens", quiet = TRUE)
  }
  if (!requireNamespace("grDevices", quietly = TRUE)) {
    install.packages("grDevices", quiet = TRUE)  # grDevices is base, but safeguard
  }

  # Load meta-analysis object
  meta_obj <- object$meta
  k <- meta_obj$k

  cat("\nDOI Plot for Publication Bias\n-----------------------------\n")
  cat(paste("Total studies included:", k, "\n"))

  if (k >= 10) {
    cat("⚠️  DOI plots are mainly recommended for <10 studies.\n")
    cat("➡️  Use `publication_bias()` for Egger/Begg tests and funnel plots instead.\n\n")
    return(invisible(NULL))
  }

  TE <- meta_obj$TE
  seTE <- meta_obj$seTE

  cat("→ Generating DOI plot using `metasens::doiplot()`...\n")

  tryCatch({
    if (interactive()) grDevices::dev.new(width = 7, height = 7)
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mar = c(5, 5, 4, 2))  # Avoid "figure margins too large" error

    metasens::doiplot(TE, seTE, xlab = "Effect Size", lfkindex = TRUE, ...)
  }, error = function(e) {
    message("❌ Failed to generate DOI plot: ", e$message)
  })

  invisible(NULL)
}
