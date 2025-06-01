#' Meta-analysis of Proportions (Logit with Influence Analysis)
#'
#' Performs a meta-analysis of proportions using logit transformation via metaprop().
#' Includes influence analysis and formatted outputs in percentages.
#'
#' @param data Dataframe with proportion data.
#' @param event Column name for event count.
#' @param n Column name for total count.
#' @param studylab Column name for study labels (optional).
#' @param subgroup Column name for subgroup analysis (optional).
#' @param model "random" or "fixed" (default = "random").
#' @param tau_method Tau^2 method (default = "REML").
#' @param ci_method CI method for random-effects (default = "HK").
#' @param verbose Logical; if TRUE, prints progress messages (default is FALSE).
#'
#' @return A list with meta object, tidy table, subgroup summary, influence analysis (all in %), and metadata.
#' @export
meta_prop <- function(data,
                      event,
                      n,
                      studylab = NULL,
                      subgroup = NULL,
                      model = "random",
                      tau_method = "REML",
                      ci_method = "HK",
                      verbose = FALSE) {

  if (verbose) message("Starting meta-analysis of proportions...")

  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The 'meta' package is required but not installed. Please install it using install.packages('meta')", call. = FALSE)
  }

  # Internal helper: inverse logit
  inv_logit <- function(x) {
    suppressWarnings(x <- as.numeric(x))
    exp(x) / (1 + exp(x))
  }

  # Labels
  study_labels <- if (!is.null(studylab)) make.unique(data[[studylab]]) else paste0("Study_", seq_len(nrow(data)))
  subgroup_var <- if (!is.null(subgroup)) data[[subgroup]] else NULL

  # Meta-analysis
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
    incr = 0.5, # ensure continuity correction
    subgroup = subgroup_var,
    backtransf = TRUE
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

  # Tidy study-level table
  tidy_tbl <- tibble::tibble(
    Study = meta_result$studlab,
    Proportion = round(inv_logit(TE_val) * 100, 1),
    lower = round(inv_logit(lower_val) * 100, 1),
    upper = round(inv_logit(upper_val) * 100, 1),
    weight = round(weights / sum(weights) * 100, 1),
    subgroup = if (!is.null(subgroup)) subgroup_var else NA
  )

  # Subgroup Summary
  meta.subgroup.summary <- NULL
  if (!is.null(subgroup)) {
    meta.subgroup.summary <- tidy_tbl %>%
      dplyr::group_by(.data$subgroup) %>%
      dplyr::group_split() %>%
      purrr::map_dfr(function(subgroup_data) {
        subgroup_name <- unique(subgroup_data$subgroup)
        idx <- which(tidy_tbl$subgroup == subgroup_name)

        event_sub <- data[[event]][idx]
        n_sub <- data[[n]][idx]
        studylab_sub <- study_labels[idx]

        meta_sub <- meta::metaprop(
          event = event_sub,
          n = n_sub,
          studlab = studylab_sub,
          method = "Inverse",
          method.tau = tau_method,
          sm = "PLOGIT",
          backtransf = TRUE
        )

        tibble::tibble(
          Subgroup = subgroup_name,
          Estimate = round(inv_logit(meta_sub$TE.random) * 100, 1),
          lower = round(inv_logit(meta_sub$lower.random) * 100, 1),
          upper = round(inv_logit(meta_sub$upper.random) * 100, 1),
          Tau2 = round(meta_sub$tau2, 4),
          I2 = round(meta_sub$I2, 1)
        )
      })
  }

  # Pooled summary (always random for now; could be made smarter if needed)
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
  influence_obj <- tryCatch(meta::metainf(meta_result, comb.random = TRUE, comb.fixed = FALSE),
                            error = function(e) NULL)

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

  # Return structured output
  structure(
    list(
      meta = meta_result,
      table = tidy_tbl,
      meta.summary = pooled,
      meta.subgroup.summary = meta.subgroup.summary,
      influence.analysis = influence_data,
      influence.meta = influence_obj,
      model = model,
      measure = "Proportion",
      tau_method = tau_method,
      ci_method = ci_method,
      subgroup = !is.null(subgroup)
    ),
    class = "meta_prop"
  )
}
