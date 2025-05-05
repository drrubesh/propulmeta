#' Generic forest plot wrapper for all meta-analysis types
#'
#' Dispatches to the appropriate plotting function based on class
#'
#' @param x A `meta_ratio`, `meta_mean`, or `meta_prop` object
#' @param ... Additional arguments passed to the relevant plot function
#'
#' @return A forest plot (displayed or saved to file)
#' @export
plot_meta <- function(x, ...) {
  if (inherits(x, "meta_ratio")) {
    plot_meta_ratio(x, ...)
  } else if (inherits(x, "meta_mean")) {
    plot_meta_mean(x, ...)
  } else if (inherits(x, "meta_prop")) {
    plot_meta_prop(x, ...)
  } else {
    stop("❌ 'x' must be a meta_ratio, meta_mean, or meta_prop object.", call. = FALSE)
  }
}
