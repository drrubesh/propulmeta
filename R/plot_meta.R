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
    stop("'x' must be a meta_ratio, meta_mean, or meta_prop object.", call. = FALSE)
  }
}

#' @noRd
#' @keywords internal
plot_meta_ratio <- function(x,
                            filename = NULL,
                            save_as = c("viewer", "pdf", "png"),
                            layout = "revman5",
                            height = NULL,
                            width = NULL,
                            ...) {
  if (!inherits(x, "meta_ratio")) stop("Input must be a 'meta_ratio' object.")
  save_as <- match.arg(save_as)

  m <- x$meta
  table <- x$table
  k <- length(m$studlab)

  sizing <- .auto_plot_sizing(k, height, width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  if (is.null(filename) && save_as != "viewer") {
    ext <- switch(save_as, pdf = "pdf", png = "png")
    filename <- paste0("forest_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
  }

  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  } else if (interactive()) {
    try(grDevices::dev.off(), silent = TRUE)  # Safely close broken device
    grDevices::dev.new(width = width, height = height)  # Fresh device
  }


  byvar <- if (isTRUE(x$subgroup) && !all(is.na(table$subgroup))) {
    tmp <- table$subgroup
    attr(tmp, "label") <- "Subgroup"
    tmp
  } else NULL

  meta::forest(
    m,
    sortvar = m$TE,
    leftlabs = c("Study", "Events/Total"),
    rightlabs = c("Effect size [95% CI]", "Weight"),
    print.w = TRUE,
    label.e = "Events (Exp)",
    label.c = "Events (Ctrl)",
    byvar = byvar,
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

  if (save_as != "viewer") {
    grDevices::dev.off()
    message(sprintf("Forest plot saved as '%s' in the working directory.", filename))
  } else {
    message("Forest plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
    if (k > 40) message("Plot may exceed viewer margins. Export for full view.")
  }

  invisible(TRUE)
}

#' @noRd
#' @keywords internal
plot_meta_mean <- function(x,
                           filename = NULL,
                           save_as = c("viewer", "pdf", "png"),
                           layout = "revman5",
                           height = NULL,
                           width = NULL,
                           ...) {
  if (!inherits(x, "meta_mean")) stop("Input must be a 'meta_mean' object.")
  save_as <- match.arg(save_as)

  m <- x$meta
  table <- x$table
  k <- length(m$studlab)

  sizing <- .auto_plot_sizing(k, height, width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  if (is.null(filename) && save_as != "viewer") {
    ext <- switch(save_as, pdf = "pdf", png = "png")
    filename <- paste0("forest_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
  }

  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  } else if (interactive()) {
    try(grDevices::dev.off(), silent = TRUE)  # Safely close broken device
    grDevices::dev.new(width = width, height = height)  # Fresh device
  }


  byvar <- if (isTRUE(x$subgroup) && !all(is.na(table$subgroup))) {
    tmp <- table$subgroup
    attr(tmp, "label") <- "Subgroup"
    tmp
  } else NULL

  meta::forest(
    m,
    sortvar = m$TE,
    leftlabs = c("Study", "N (Exp/Ctrl)"),
    rightlabs = c("Mean Difference [95% CI]", "Weight"),
    print.w = TRUE,
    label.e = "Experimental",
    label.c = "Control",
    byvar = byvar,
    subgroup.name = "Subgroup",
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

  if (save_as != "viewer") {
    grDevices::dev.off()
    message(sprintf("Forest plot saved as '%s' in the working directory.", filename))
  } else {
    message("Forest plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
    if (k > 40) message("Plot may exceed viewer margins. Export for full view.")
  }

  invisible(TRUE)
}

#' @noRd
#' @keywords internal
plot_meta_prop <- function(x,
                           filename = NULL,
                           save_as = c("viewer", "pdf", "png"),
                           layout = "revman5",
                           height = NULL,
                           width = NULL,
                           ...) {
  if (!inherits(x, "meta_prop")) stop("Input must be a 'meta_prop' object.")
  save_as <- match.arg(save_as)

  m <- x$meta
  table <- x$table
  k <- length(m$studlab)

  sizing <- .auto_plot_sizing(k, height, width)
  height <- sizing$height
  width  <- sizing$width
  fontsize <- sizing$fontsize

  if (is.null(filename) && save_as != "viewer") {
    ext <- switch(save_as, pdf = "pdf", png = "png")
    filename <- paste0("forest_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
  }

  if (save_as == "pdf") {
    grDevices::pdf(filename, width = width, height = height)
  } else if (save_as == "png") {
    grDevices::png(filename, width = width, height = height, units = "in", res = 300)
  } else if (interactive()) {
    try(grDevices::dev.off(), silent = TRUE)  # Safely close broken device
    grDevices::dev.new(width = width, height = height)  # Fresh device
  }


  byvar <- if (isTRUE(x$subgroup) && !all(is.na(table$subgroup))) {
    tmp <- as.factor(table$subgroup)
    attr(tmp, "label") <- "Subgroup"
    tmp
  } else NULL

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
    leftcols = c("studlab", "event", "n", "w.random", "effect", "ci"),
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
    byvar = byvar,
    subgroup.name = "Subgroup",
    ...
  )

  if (save_as != "viewer") {
    grDevices::dev.off()
    message(sprintf("Forest plot saved as '%s' in the working directory.", filename))
  } else {
    message("Forest plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` for publication-quality export.")
    if (k > 40) message("Plot may exceed viewer margins. Export for full view.")
  }

  invisible(TRUE)
}
