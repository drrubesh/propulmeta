#' Meta-analysis of Means (Mean Difference or Standardized Mean Difference)
#'
#' Performs a meta-analysis of continuous outcomes (mean differences or standardized mean differences),
#' with support for prediction intervals, subgroup tests, and influence analysis (leave-one-out).
#'
#' @param data Dataframe containing the meta-analysis data.
#' @param mean.e Column name for mean in experimental group.
#' @param sd.e Column name for standard deviation in experimental group.
#' @param n.e Column name for total number in experimental group.
#' @param mean.c Column name for mean in control group.
#' @param sd.c Column name for standard deviation in control group.
#' @param n.c Column name for total number in control group.
#' @param studylab Column name for study labels (optional).
#' @param subgroup Column name for subgroup analysis (optional).
#' @param model "random" or "fixed" effects model (default = "random").
#' @param measure Type of summary measure ("MD" = mean difference, "SMD" = standardized mean difference; default = "MD").
#' @param tau_method Tau^2 method: "REML", "PM", "DL", "ML", "HS", "SJ", "HE", "EB".
#' @param ci_method For random model: "classic", "HK" (Hartung-Knapp), or "KR".
#' @param verbose Logical; if TRUE, prints progress messages (default is FALSE).
#'
#' @return A list containing the meta object, tidy summary table, influence analysis, and metadata.
#' @export
meta_mean <- function(data,
                      mean.e, sd.e, n.e,
                      mean.c, sd.c, n.c,
                      studylab = NULL,
                      subgroup = NULL,
                      model = "random",
                      measure = "MD",
                      tau_method = "REML",
                      ci_method = "HK",
                      verbose = FALSE) {

  if (verbose) message("Starting meta-analysis of means...")

  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The 'meta' package is required but not installed. Please install it using install.packages('meta')", call. = FALSE)
  }

  # Labels
  study_labels <- if (!is.null(studylab)) data[[studylab]] else paste0("Study_", seq_len(nrow(data)))
  subgroup_var <- if (!is.null(subgroup)) data[[subgroup]] else NULL

  # Meta-analysis
  meta_result <- meta::metacont(
    n.e = data[[n.e]],
    mean.e = data[[mean.e]],
    sd.e = data[[sd.e]],
    n.c = data[[n.c]],
    mean.c = data[[mean.c]],
    sd.c = data[[sd.c]],
    studlab = study_labels,
    sm = measure,
    method.tau = tau_method,
    method.random.ci = ci_method,
    common = (model == "fixed"),
    random = (model == "random"),
    subgroup = subgroup_var
  )

  # Correctly assign based on model
  if (model == "random") {
    weights <- meta_result$w.random
    TE_val <- meta_result$TE.random
    lower_val <- meta_result$lower.random
    upper_val <- meta_result$upper.random
  } else {
    weights <- meta_result$w.fixed
    TE_val <- meta_result$TE.common
    lower_val <- meta_result$lower.common
    upper_val <- meta_result$upper.common
  }

  # Tidy output
  tidy_tbl <- tibble::tibble(
    Study = meta_result$studlab,
    TE = TE_val,
    lower = lower_val,
    upper = upper_val,
    weight = round(weights / sum(weights) * 100, 1),
    subgroup = if (!is.null(subgroup)) subgroup_var else NA
  )
  meta.subgroup.summary <- NULL
  if (!is.null(subgroup)) {
    meta.subgroup.summary <- tibble::tibble(
      Subgroup = meta_result$subgroup.levels,
      Estimate = if (measure %in% c("OR", "RR", "HR")) exp(meta_result$TE.random.w) else meta_result$TE.random.w,
      lower = if (measure %in% c("OR", "RR", "HR")) exp(meta_result$lower.random.w) else meta_result$lower.random.w,
      upper = if (measure %in% c("OR", "RR", "HR")) exp(meta_result$upper.random.w) else meta_result$upper.random.w,
      Tau2 = round(meta_result$tau2.w, 4),
      I2 = round(meta_result$I2.w, 1)
    )
  }


  # Influence analysis
  influence <- tryCatch({
    inf <- meta::metainf(meta_result, comb.random = TRUE, comb.fixed = FALSE)
    inf$studlab <- make.unique(inf$studlab)
    inf
  }, error = function(e) NULL)

  # Return consistent structure
  structure(
    list(
      meta = meta_result,
      table = tidy_tbl,
      meta.subgroup.summary = meta.subgroup.summary,
      influence.analysis = influence,
      model = model,
      measure = measure,
      tau_method = tau_method,
      ci_method = ci_method,
      subgroup = !is.null(subgroup)
    ),
    class = "meta_mean"
  )
}
