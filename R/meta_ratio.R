#' Meta-analysis of Ratios (RR, OR, HR) with Auto-Preparation and Influence Analysis
#'
#' Performs a meta-analysis of risk ratios (RR), odds ratios (OR), or hazard ratios (HR),
#' with support for prediction intervals, subgroup tests, and influence analysis (leave-one-out).
#'
#' @param data Dataframe containing the meta-analysis data.
#' @param effect Column name for effect size (optional if event data provided).
#' @param lower Column name for lower bound of 95% CI (optional).
#' @param upper Column name for upper bound of 95% CI (optional).
#' @param event.e Number of events in experimental group
#' @param n.e Sample size in experimental group
#' @param event.c Number of events in control group
#' @param n.c Sample size in control group
#' @param studylab Column name for study labels (optional).
#' @param subgroup Column name for subgroup analysis (optional).
#' @param model "random" or "fixed" (default = "random").
#' @param measure "OR", "RR", or "HR" (default = "OR").
#' @param tau_method Tau^2 method: "REML", "PM", "DL", "ML", "HS", "SJ", "HE", "EB".
#' @param ci_method For random model: "classic", "HK" (Hartung-Knapp), or "KR".
#' @param verbose Logical; if TRUE, prints progress messages (default is FALSE).
#' @importFrom stats plogis
#' @importFrom graphics axis box par
#' @importFrom magrittr %>%
#' @return A list with elements: meta (meta object), table (tidy results), and influence.analysis.
#' @export
meta_ratio <- function(data,
                       event.e = NULL, n.e = NULL,
                       event.c = NULL, n.c = NULL,
                       effect = NULL, lower = NULL, upper = NULL,
                       studylab = NULL, subgroup = NULL,
                       model = "random", measure = "OR",
                       tau_method = "REML",
                       ci_method = "classic",
                       verbose = FALSE) {

  if (verbose) message("Starting meta-analysis of ratios...")

  # Input validation
  if (!is.null(event.e) || !is.null(event.c)) {
    if (is.null(n.e) || is.null(n.c)) stop("Please provide 'n.e' and 'n.c' for event data.")
  }

  if (!is.null(effect)) {
    if (is.null(lower) || is.null(upper)) stop("Please provide 'lower' and 'upper' bounds for effect size.")
  }

  if (is.null(effect) && (is.null(event.e) || is.null(event.c))) {
    stop("Either provide event counts (event.e, event.c) or effect sizes (effect, lower, upper).")
  }

  # Study labels and subgroup
  study_labels <- if (!is.null(studylab)) data[[studylab]] else paste0("Study_", seq_len(nrow(data)))
  subgroup_var <- if (!is.null(subgroup)) data[[subgroup]] else NULL

  # Model type
  common <- model == "fixed"
  random <- model == "random"

  # Meta-analysis using metabin or metagen
  if (!is.null(event.e) && !is.null(event.c)) {
    meta_result <- meta::metabin(
      event.e = data[[event.e]],
      n.e = data[[n.e]],
      event.c = data[[event.c]],
      n.c = data[[n.c]],
      studlab = study_labels,
      sm = measure,
      method.tau = tau_method,
      method.random.ci = ci_method,
      common = common,
      random = random,
      subgroup = subgroup_var
    )
  } else {
    meta_result <- meta::metagen(
      TE = log(data[[effect]]),
      seTE = (log(data[[upper]]) - log(data[[lower]])) / (2 * 1.96),
      studlab = study_labels,
      sm = measure,
      method.tau = tau_method,
      method.random.ci = ci_method,
      common = common,
      random = random,
      subgroup = subgroup_var
    )
  }

  # Weights and tidy summary
  weights <- if (model == "random") meta_result$w.random else meta_result$w.fixed
  tidy_tbl <- tibble::tibble(
    Study = meta_result$studlab,
    TE = meta_result$TE,
    lower = meta_result$lower,
    upper = meta_result$upper,
    weight = round(weights / sum(weights) * 100, 1),
    subgroup = if (!is.null(subgroup)) subgroup_var else NA
  )

  # Influence analysis (leave-one-out)
  influence <- tryCatch({
    inf <- meta::metainf(meta_result)
    inf$studlab <- make.unique(inf$studlab)
    inf
  }, error = function(e) NULL)

  # Return structured output
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
    class = "meta_ratio"
  )
}
