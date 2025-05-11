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
#' @param tau_method Tau² method: "REML", "PM", "DL", "ML", "HS", "SJ", "HE", "EB".
#' @param ci_method For random model: "classic", "HK" (Hartung-Knapp), or "KR".
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
                      ci_method = "HK") {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The 'meta' package is required but not installed. Please install it using install.packages('meta')", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("The 'tibble' package is required but not installed. Please install it using install.packages('tibble')", call. = FALSE)
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

  # Weights
  weights <- if (model == "random") meta_result$w.random else meta_result$w.fixed

  # Tidy output
  tidy_tbl <- tibble::tibble(
    Study = meta_result$studlab,
    TE = meta_result$TE,
    lower = meta_result$lower,
    upper = meta_result$upper,
    weight = round(weights / sum(weights) * 100, 1),
    subgroup = if (!is.null(subgroup)) subgroup_var else NA
  )

  # Influence analysis
  influence <- tryCatch({
    inf <- meta::metainf(meta_result)
    inf$studlab <- make.unique(inf$studlab)
    inf
  }, error = function(e) NULL)


  # Return consistent structure
  structure(
    list(
      meta = meta_result,
      table = tidy_tbl,
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
