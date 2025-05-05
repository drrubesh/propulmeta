#' Forest plot for meta-analysis of ratio-type outcomes (e.g., OR, RR, HR)
#'
#' @param x A `meta_ratio` object from `propulmeta::meta_ratio()`
#' @param filename Output file name (used only if `save_as` is "pdf" or "png")
#' @param save_as Either "pdf", "png", or "viewer" (default = "viewer")
#' @param layout Forest plot layout: "revman5", "default", "JAMA", etc.
#' @param ... Additional arguments passed to `meta::forest()`
#'
#' @return A forest plot either displayed in viewer or saved to file
#' @keywords internal
plot_meta_ratio <- function(x,
                            filename = NULL,
                            save_as = c("viewer", "pdf", "png"),
                            layout = "revman5",
                            height= NULL,
                            width= NULL,
                            ...) {
  checkmate::assert_class(x, "meta_ratio")
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
  events_total <- paste(m$event.e, "/", m$n.e)
  labs <- data.frame(Study = m$studlab, `Events/Total` = events_total)

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
    filename <- switch(save_as,
                       pdf = "forest_plot.pdf",
                       png = "forest_plot.png")
  }

  # Open device if exporting
  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  }
  if (save_as == "viewer") {
    while (dev.cur() > 1) grDevices::dev.off()  # Close any open non-null devices
  }
  if (save_as == "viewer") {
    message("📊 Forest plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
  }
  # Clean subgroup label
  if (!is.null(byvar)) {
    attr(byvar, "label") <- "Subgroup"
  }

  # Plot
  meta::forest(
    m,
    sortvar = m$TE,
    leftlabs = c("Study", "Events/Total"),
    rightlabs = c("Effect size [95% CI]", "Weight"),
    print.w = TRUE,
    label.e = "Events (Exp)",
    label.c = "Events (Ctrl)",
    byvar= byvar,
    subgroup.name = "Subgroup",
    pooled.totals = TRUE,
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
    layout = tolower(layout),
    ...
  )

  # Close device if export
  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Forest plot saved as '{filename}' in the working directory."))
  }

  # Viewer warning if large study
  if (save_as == "viewer" && k > 40) {
    message("ℹ Plot may exceed viewer margins. Use `save_as = 'pdf'` or `'png'` to export the full plot.")
  }

  invisible(TRUE)
}
