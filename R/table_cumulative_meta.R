#' Table of Cumulative Meta-Analysis
#'
#' Produces a step-by-step table of cumulative pooled estimates.
#'
#' @param object A `meta_prop`, `meta_ratio`, or `meta_mean` object.
#' @param include_heterogeneity Logical. Show I² column (default = TRUE).
#'
#' @return A publication-ready `gt` table.
#' @export
table_cumulative_meta <- function(object, include_heterogeneity = TRUE) {
  if (!inherits(object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("❌ Only supports meta_prop, meta_ratio, or meta_mean objects.", call. = FALSE)
  }

  meta_obj <- object$meta
  if (!inherits(meta_obj, "meta")) {
    stop("❌ meta field not found in object.", call. = FALSE)
  }

  cum <- meta::metacum(meta_obj)
  keep <- !is.na(cum$TE)
  cum$studlab <- cum$studlab[keep]
  cum$TE <- cum$TE[keep]
  cum$lower <- cum$lower[keep]
  cum$upper <- cum$upper[keep]
  if (!is.null(cum$I2)) cum$I2 <- cum$I2[keep]
  if (!is.null(cum$tau2)) cum$tau2 <- cum$tau2[keep]
  if (!is.null(cum$year)) cum$year <- cum$year[keep]



  has_year <- "year" %in% names(cum)
  has_tau2 <- "tau2" %in% names(cum)
  has_I2 <- "I2" %in% names(cum)

  df <- tibble::tibble(
    Step = seq_along(cum$studlab),
    `Study Added` = cum$studlab,
    Estimate = signif(if (object$measure == "Proportion") plogis(cum$TE) * 100 else exp(cum$TE), digits = 3),
    lower = signif(if (object$measure == "Proportion") plogis(cum$lower) * 100 else exp(cum$lower), digits = 3),
    upper = signif(if (object$measure == "Proportion") plogis(cum$upper) * 100 else exp(cum$upper), digits = 3)

  )

  if (has_year) df$Year <- cum$year
  if (include_heterogeneity && has_I2) df$I2 <- round(cum$I2, 1)
  if (include_heterogeneity && has_tau2) df$Tau2 <- round(cum$tau2, 3)

  df <- df %>%
    dplyr::mutate(`Estimate [95% CI]` = paste0(Estimate, " [", lower, " – ", upper, "]")) %>%
    dplyr::select(Step, `Study Added`, dplyr::any_of("Year"),
                  `Estimate [95% CI]`, dplyr::any_of(c("I2", "Tau2")))

  label <- switch(
    object$measure,
    "Proportion" = "Pooled Proportion (%)",
    "Mean Difference" = "Mean Difference",
    "Risk Ratio" = "Risk Ratio",
    "Odds Ratio" = "Odds Ratio",
    "Hazard Ratio" = "Hazard Ratio",
    "Estimate"
  )
  df <- df %>%
    dplyr::mutate(.final = dplyr::row_number() == dplyr::n())
  gt_df <- df %>% dplyr::select(-.final)



  gt_tbl <- gt::gt(gt_df) %>%
    gt::tab_header(title = "Cumulative Meta-Analysis Summary") %>%
    gt::cols_label(
      Step = "Step",
      `Study Added` = "Study",
      `Estimate [95% CI]` = glue::glue("{label}")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = df$.final)
    )

  if ("Year" %in% names(df)) {
    gt_tbl <- gt_tbl %>% gt::cols_label(Year = "Year")
  }
  if ("I2" %in% names(df)) {
    gt_tbl <- gt_tbl %>% gt::cols_label(I2 = "I² (%)")
  }
  if ("Tau2" %in% names(df)) {
    gt_tbl <- gt_tbl %>% gt::cols_label(Tau2 = "Tau²")
  }

  gt_tbl
}

