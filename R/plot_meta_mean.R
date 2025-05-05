#' Forest plot for meta-analysis of mean differences
#'
#' @param x A `meta_mean` object from `propulmeta::meta_mean()`
#' @param filename Output file name (used only if `save_as` is "pdf" or "png")
#' @param save_as Either "pdf", "png", or "viewer" (default = "viewer")
#' @param layout Forest plot layout: "revman5", "default", "JAMA", etc.
#' @param ... Additional arguments passed to `meta::forest()`
#'
#' @return A forest plot either displayed in viewer or saved to file
#' @keywords internal
plot_meta_mean <- function(x,
                           filename = NULL,
                           save_as = c("viewer", "pdf", "png"),
                           layout = "revman5",
                           height= NULL,
                           width= NULL,
                           ...) {
  checkmate::assert_class(x, "meta_mean")
  save_as <- match.arg(save_as)

  m <- x$meta
  table <- x$table
  k <- length(m$studlab)


  # Auto-adjust plot size
  sizing <- .auto_plot_sizing(k, height = height, width = width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize


  # Labels
  labels <- data.frame(Study = m$studlab,
                       N = paste(m$n.e, "/", m$n.c))

  # Subgroup support
  byvar <- if (isTRUE(x$subgroup) && !all(is.na(table$subgroup))) {
    tmp <- table$subgroup
    attr(tmp, "label") <- "Subgroup"
    tmp
  } else {
    NULL
  }

  # Default filename
  if (is.null(filename) && save_as != "viewer") {
    suffix <- if (isTRUE(x$subgroup)) "_subgroup" else ""
    filename <- switch(save_as,
                       pdf = paste0("forest_plot_mean", suffix, ".pdf"),
                       png = paste0("forest_plot_mean", suffix, ".png"))
  }

  # Open device if exporting
  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  }

  # Viewer fallback
  if (save_as == "viewer") {
    while (dev.cur() > 1) grDevices::dev.off()
    message("\U0001F4CA Forest plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
  }

  # Plot
  meta::forest(
    m,
    sortvar = m$TE,
    leftlabs = c("Study", "N (Exp/Ctrl)"),
    rightlabs = c("Mean Difference [95% CI]", "Weight"),
    print.w = TRUE,
    label.e = "Experimental",
    label.c = "Control",
    byvar= byvar,
    subgroup.name = "Subgroup",
    layout = tolower(layout),
    prediction = TRUE,
    print.pred = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.Q = TRUE,
    fontsize = fontsize,
    col.square = "blue",
    col.square.lines = "blue",
    col.diamond = "blue",
    col.diamond.lines = "blue",
    col.predict = "darkgreen",
    ...
  )

  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("\u2705 Forest plot saved as '{filename}' in the working directory."))
  }

  if (save_as == "viewer" && k > 40) {
    message("\u2139\ufe0f Plot may exceed viewer margins. Use `save_as = 'pdf'` or `'png'` to export the full plot.")
  }

  invisible(TRUE)
}
