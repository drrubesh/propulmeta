#' Generate Meta-Analysis Summary Table
#'
#' Dispatches to the appropriate internal function to generate a summary table
#' depending on the class of the meta-analysis object.
#'
#' @param meta_result A meta-analysis object of class `meta_ratio`, `meta_mean`, or `meta_prop`.
#'
#' @return A `gt` table summarizing study-level and pooled results.
#' @export
#'
table_meta <- function(meta_result) {
  stopifnot(inherits(meta_result, c("meta_ratio", "meta_mean", "meta_prop")))

  if (inherits(meta_result, "meta_ratio")) {
    return(table_meta_ratio(meta_result))
  } else if (inherits(meta_result, "meta_mean")) {
    return(table_meta_mean(meta_result))
  } else if (inherits(meta_result, "meta_prop")) {
    return(table_meta_prop(meta_result))
  }
}

#' @title Internal: Meta-analysis of Ratios Table
#' @description Generates a summary table for meta-analysis objects of class `meta_ratio`.
#' @param meta_result A `meta_ratio` object created by `meta_ratio()`.
#' @return A `gt` table with study-level ORs and a pooled estimate.
#' @noRd
table_meta_ratio <- function(meta_result) {
  df <- meta_result$table %>%
    dplyr::mutate(
      `Events` = paste0(meta_result$meta$event.e, "/", meta_result$meta$n.e,
                        " vs ", meta_result$meta$event.c, "/", meta_result$meta$n.c),
      `Weight (%)` = round(.data$weight, 1),
      `Odds Ratio [95% CI]` = paste0(round(exp(.data$TE), 2), " [",
                                     round(exp(.data$lower), 2), " – ",
                                     round(exp(.data$upper), 2), "]")
    ) %>%
    dplyr::select(.data$Study, .data$Events, `Weight (%)`, `Odds Ratio [95% CI]`)

  summary_row <- tibble::tibble(
    Study = "Pooled",
    Events = "",
    `Weight (%)` = NA_real_,
    `Odds Ratio [95% CI]` = paste0(
      round(exp(meta_result$meta$TE.random), 2), " [",
      round(exp(meta_result$meta$lower.random), 2), " – ",
      round(exp(meta_result$meta$upper.random), 2), "]",
      " (I² = ", round(meta_result$meta$I2, 1), "%)"
    )
  )

  dplyr::bind_rows(df, summary_row) %>% gt::gt()
}

#' @title Internal: Meta-analysis of Means Table
#' @description Generates a summary table for meta-analysis objects of class `meta_mean`.
#' @param meta_result A `meta_mean` object created by `meta_mean()`.
#' @return A `gt` table with study-level mean differences and a pooled estimate.
#' @noRd
table_meta_mean <- function(meta_result) {
  df <- meta_result$table %>%
    dplyr::mutate(
      `Mean (SD)` = paste0(meta_result$meta$mean.e, " (", meta_result$meta$sd.e, ") vs ",
                           meta_result$meta$mean.c, " (", meta_result$meta$sd.c, ")"),
      Total = meta_result$meta$n.e + meta_result$meta$n.c,
      `Weight (%)` = round(.data$weight, 1),
      `Mean Difference [95% CI]` = paste0(round(.data$TE, 2), " [",
                                          round(.data$lower, 2), " – ",
                                          round(.data$upper, 2), "]")
    ) %>%
    dplyr::select(.data$Study, `Mean (SD)`, .data$Total, `Weight (%)`, `Mean Difference [95% CI]`)

  summary_row <- tibble::tibble(
    Study = "Pooled",
    `Mean (SD)` = "",
    Total = NA_real_,
    `Weight (%)` = NA_real_,
    `Mean Difference [95% CI]` = paste0(
      round(meta_result$meta$TE.random, 2), " [",
      round(meta_result$meta$lower.random, 2), " – ",
      round(meta_result$meta$upper.random, 2), "]",
      " (I² = ", round(meta_result$meta$I2, 1), "%)"
    )
  )

  dplyr::bind_rows(df, summary_row) %>% gt::gt()
}

#' @title Internal: Meta-analysis of Proportions Table
#' @description Generates a summary table for meta-analysis objects of class `meta_prop`.
#' @param meta_result A `meta_prop` object created by `meta_prop()`.
#' @return A `gt` table with study-level proportions and a pooled estimate.
#' @noRd
table_meta_prop <- function(meta_result) {
  df <- meta_result$table %>%
    dplyr::mutate(
      Events = meta_result$meta$event,
      Total = meta_result$meta$n,
      `Weight (%)` = round(.data$weight, 1),
      `Proportion [95% CI]` = paste0(round(.data$Proportion, 2), "% [",
                                     round(.data$lower, 2), " – ",
                                     round(.data$upper, 2), "]")
    ) %>%
    dplyr::select(.data$Study, .data$Events, .data$Total, `Weight (%)`, `Proportion [95% CI]`)

  summary_row <- tibble::tibble(
    Study = "Pooled",
    Events = NA,
    Total = NA,
    `Weight (%)` = NA_real_,
    `Proportion [95% CI]` = paste0(
      round(meta_result$meta.summary$Estimate * 100, 2), "% [",
      round(meta_result$meta.summary$lower * 100, 2), " – ",
      round(meta_result$meta.summary$upper * 100, 2), "]",
      " (I² = ", round(meta_result$meta.summary$I2 * 100, 1), "%)"
    )
  )

  dplyr::bind_rows(df, summary_row) %>% gt::gt()
}
