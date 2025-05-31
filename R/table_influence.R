#' Table: Leave-One-Out Influence Analysis
#'
#' @param object A meta_prop, meta_ratio, or meta_mean object
#' @param ... Additional arguments passed to gtsummary
#' @param include_heterogeneity Logical; if TRUE, includes heterogeneity statistics.
#' @param ... Additional arguments passed to methods.
#' @return A gtsummary publication-ready table
#' @export
table_influence <- function(object, include_heterogeneity = TRUE) {
  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("❌ table_influence() supports only meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  infl <- if ("meta_prop" %in% class(object)) {
    object$influence.analysis
  } else {
    object$influence.analysis
  }

  meta_infl <- if ("meta_prop" %in% class(object)) {
    object$influence.meta
  } else {
    infl  # same object if not pre-formatted
  }

  # Build tidy table with effect estimates
  if (inherits(infl, "metainf")) {
    keep_rows <- meta_infl$studlab != " " & !is.na(meta_infl$TE)

    df <- tibble::tibble(
      Study = meta_infl$studlab[keep_rows],
      Estimate = round(
        if (object$measure == "Proportion") plogis(meta_infl$TE[keep_rows]) * 100 else exp(meta_infl$TE[keep_rows]),
        2
      ),
      lower = round(
        if (object$measure == "Proportion") plogis(meta_infl$lower[keep_rows]) * 100 else exp(meta_infl$lower[keep_rows]),
        2
      ),
      upper = round(
        if (object$measure == "Proportion") plogis(meta_infl$upper[keep_rows]) * 100 else exp(meta_infl$upper[keep_rows]),
        2
      ),
      I2 = if (include_heterogeneity) round(meta_infl$I2[keep_rows], 1) else NULL,
      Tau2 = if (include_heterogeneity) round(meta_infl$tau2[keep_rows], 3) else NULL
    )
  } else if (inherits(infl, "data.frame")) {
    df <- infl
    if ("Proportion" %in% names(df)) {
      df <- df %>% dplyr::rename(Estimate = Proportion)
    }

    if (include_heterogeneity && !is.null(object$influence.meta)) {
      meta_infl <- object$influence.meta
      keep_rows <- meta_infl$studlab != " " & !is.na(meta_infl$TE)
      df$I2 <- round(meta_infl$I2[keep_rows], 1)
      df$Tau2 <- round(meta_infl$tau2[keep_rows], 3)
    }
  } else {
    stop("❌ influence.analysis must be a tibble or metainf object.", call. = FALSE)
  }

  # Merge CI column
  df <- df %>%
    dplyr::mutate(
      `Estimate [95% CI]` = paste0(Estimate, " [", lower, " – ", upper, "]")
    ) %>%
    dplyr::select(Study, `Estimate [95% CI]`, dplyr::any_of(c("I2", "Tau2")))

  label <- switch(
    object$measure,
    "Proportion" = "Proportion (%)",
    "Mean Difference" = "Mean Difference",
    "Risk Ratio" = "Risk Ratio",
    "Odds Ratio" = "Odds Ratio",
    "Hazard Ratio" = "Hazard Ratio",
    "Estimate"
  )

  # Build GT table
  gt::gt(df) %>%
    gt::tab_header(title = "Leave-One-Out Influence Analysis") %>%
    gt::cols_label(
      Study = "Study",
      `Estimate [95% CI]` = glue::glue("{label}"),
      I2 = "I² (%)",
      Tau2 = "Tau²"
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = Study == "Pooled estimate")
    )
}
