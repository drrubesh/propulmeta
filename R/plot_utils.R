#' Internal helper: Auto-adjust plot size for meta-analysis
#'
#' @param k Number of studies.
#' @param height Optional fixed height (in inches).
#' @param width Optional fixed width (in inches).
#'
#' @return A list with `height`, `width`, and `fontsize`.
#' @keywords internal
.auto_plot_sizing <- function(k, height = NULL, width = NULL) {
  fontsize <- if (k > 40) 9 else 11
  height <- if (is.null(height)) min(45, max(12, 0.35 * k)) else height
  width  <- if (is.null(width)) 10 else width
  list(height = height, width = width, fontsize = fontsize)
}
