#' Meta-analysis of Proportions (Logit with Influence Analysis)
#'
#' Performs a meta-analysis of proportions using logit transformation via metagen().
#' Includes influence analysis and formatted outputs in percentages.
#'
#' @param data Dataframe with proportion data.
#' @param event Column name for event count.
#' @param n Column name for total count.
#' @param studylab Column name for study labels (optional).
#' @param model "random" or "fixed" (default = "random").
#' @param tau_method Tau² method (default = "REML").
#' @param ci_method CI method for random-effects (default = "HK").
#'
#' @return A list with meta object, tidy table, influence analysis (all in %), and metadata.
#' @export
meta_prop <- function(data,
                      event,
                      n,
                      studylab = NULL,
                      subgroup = NULL,
                      model = "random",
                      tau_method = "REML",
                      ci_method = "HK") {
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The 'meta' package is required but not installed. Please install it using install.packages('meta')", call. = FALSE)
  }

  inv_logit <- function(x) {
    suppressWarnings(x <- as.numeric(x))
    exp(x) / (1 + exp(x))
  }

  study_labels <- if (!is.null(studylab)) make.unique(data[[studylab]]) else paste0("Study_", seq_len(nrow(data)))
  subgroup_var <- if (!is.null(subgroup)) data[[subgroup]] else NULL


  meta_result <- meta::metaprop(
    event = data[[event]],
    n = data[[n]],
    studlab = study_labels,
    sm = "PLOGIT",
    method = "Inverse",
    method.tau = tau_method,
    method.random.ci = ci_method,
    random = (model == "random"),
    common = (model == "fixed"),
    incr = 0.5, # ensure continuity
    subgroup = subgroup_var,
    backtransf = TRUE
  )

  weights <- if (model == "random") meta_result$w.random else meta_result$w.fixed

  tidy_tbl <- tibble::tibble(
    Study = meta_result$studlab,
    Proportion = round(inv_logit(meta_result$TE) * 100, 1),
    lower = round(inv_logit(meta_result$lower) * 100, 1),
    upper = round(inv_logit(meta_result$upper) * 100, 1),
    weight = round(weights / sum(weights) * 100, 1),
    subgroup = if (!is.null(subgroup)) subgroup_var else NA
  )

  pooled <- tibble::tibble(
    Estimate = round(inv_logit(meta_result$TE.random) * 100, 1),
    lower = round(inv_logit(meta_result$lower.random) * 100, 1),
    upper = round(inv_logit(meta_result$upper.random) * 100, 1),
    pred.lower = if (!is.null(meta_result$lower.predict)) round(inv_logit(meta_result$lower.predict) * 100, 1) else NA,
    pred.upper = if (!is.null(meta_result$upper.predict)) round(inv_logit(meta_result$upper.predict) * 100, 1) else NA,
    I2 = meta_result$I2,
    Tau2 = meta_result$tau2
  )


  # Influence analysis
  influence_obj <- tryCatch(meta::metainf(meta_result), error = function(e) NULL)
  if (!is.null(influence_obj)) {
    influence_obj$backtransf <- TRUE
  }


  influence_data <- if (!is.null(influence_obj)) {
    keep_rows <- influence_obj$studlab != " " & !is.na(influence_obj$TE)

    tibble::tibble(
      Study = influence_obj$studlab[keep_rows],
      Proportion = round(plogis(influence_obj$TE[keep_rows]) * 100, 1),
      lower = round(plogis(influence_obj$lower[keep_rows]) * 100, 1),
      upper = round(plogis(influence_obj$upper[keep_rows]) * 100, 1)
    )
  } else {
    NULL
  }

  # Return
  structure(
    list(
      meta.summary = pooled,
      table = tidy_tbl,
      influence.analysis = influence_data,
      meta = meta_result,
      influence.meta= influence_obj,
      model = model,
      measure = "Proportion",
      tau_method = tau_method,
      ci_method = ci_method,
      subgroup = !is.null(subgroup)
    ),
    class = "meta_prop"
  )
}

