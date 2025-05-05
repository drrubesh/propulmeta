#' Internal helper: Auto-adjust plot size for meta-analysis
#'
#' @param k Number of studies
#' @param height Optional fixed height (in inches)
#' @param width Optional fixed width (in inches)
#'
#' @return A list with `height`, `width`, and `fontsize`
#' @keywords internal
.auto_plot_sizing <- function(k, height = NULL, width = NULL) {
  fontsize <- ifelse(k > 40, 9, 11)
  height <- height %||% min(45, max(12, 0.35 * k))
  width  <- width  %||% 10
  list(height = height, width = width, fontsize = fontsize)
}
