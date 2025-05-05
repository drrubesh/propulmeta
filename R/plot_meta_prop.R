#' Forest plot for meta-analysis of proportions (publication-ready)
#'
#' @param x A `meta_prop` object from `propulmeta::meta_prop()`
#' @param filename Output file name (used only if `save_as` is "pdf" or "png")
#' @param save_as Either "pdf", "png", or "viewer" (default = "viewer")
#' @param layout Forest plot layout: "revman5", "default", "JAMA", etc.
#' @param ... Additional arguments passed to `meta::forest()`
#'
#' @return A forest plot either displayed in viewer or saved to file
#' @keywords internal
plot_meta_prop <- function(x,
                           filename = NULL,
                           save_as = c("viewer", "pdf", "png"),
                           layout = "revman5",
                           height = NULL,
                           width = NULL,
                           ...) {
  checkmate::assert_class(x, "meta_prop")
  save_as <- match.arg(save_as)

  m <- x$meta
  table <- x$table
  k <- length(m$studlab)

  # Auto-adjust plot size
  sizing <- .auto_plot_sizing(k, height = height, width = width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  # Subgroup support
  byvar <- if (isTRUE(x$subgroup) && !all(is.na(table$subgroup))) {
    tmp <- as.factor(table$subgroup)           # Step 1: make sure it's a factor
    attr(tmp, "label") <- "Subgroup"           # Step 2: set label cleanly
    tmp
  } else {
    NULL
  }

  # Default filename
  if (is.null(filename) && save_as != "viewer") {
    filename <- switch(save_as,
                       pdf = "forest_plot_prop.pdf",
                       png = "forest_plot_prop.png")
  }

  # Open device if exporting
  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  }
  if (save_as == "viewer") {
    while (dev.cur() > 1) grDevices::dev.off()
    message("📊 Forest plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
  }



  # Publication-ready forest plot
  meta::forest(
    m,

    print.tau2 = TRUE,
    print.Q = TRUE,
    print.pval.Q = TRUE,
    print.I2 = TRUE,
    print.w = TRUE,
    pooled.totals = TRUE,
    prediction = TRUE,
    print.pred = TRUE,
    weight.study = "random",
    leftcols = c("studlab", "event", "n",  "w.random", "effect", "ci"),
    leftlabs = c("Study", "Events", "Total", "Weight", "Proportion", "95% CI"),
    xlab = "Proportion (%)",
    smlab = "",
    xlim = c(0, 15),
    pscale = 100,
    squaresize = 0.5,
    fs.hetstat = fontsize,
    digits = 2,
    col.square = "blue",
    col.square.lines = "blue",
    col.diamond = "blue",
    col.diamond.lines = "blue",
    col.predict = "darkgreen",
    layout = tolower(layout),
    byvar= byvar,
    subgroup.name = "Subgroup",

    ...
  )

  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Forest plot saved as '{filename}' in the working directory."))
  }

  if (save_as == "viewer" && k > 40) {
    message("ℹ️ Plot may exceed viewer margins. Use `save_as = 'pdf'` or `'png'` to export the full plot.")
  }

  invisible(TRUE)
}
