.summary_meta_generic <- function(object, type = c("ratio", "prop", "mean"), ...) {
  type <- match.arg(type)
  meta_result <- object$meta
  measure <- object$measure
  inv_logit <- function(x) exp(x) / (1 + exp(x))

  cat("\nMeta-analysis Summary\n")
  cat("----------------------\n")

  if (object$subgroup) {
    cat("Subgroup analysis performed.\n\n")
    for (i in seq_along(meta_result$subgroup.levels)) {
      grp <- meta_result$subgroup.levels[i]
      TE <- meta_result$TE.random.w[i]
      lower <- meta_result$lower.random.w[i]
      upper <- meta_result$upper.random.w[i]
      pred.lower <- meta_result$lower.predict.w[i]
      pred.upper <- meta_result$upper.predict.w[i]
      i2 <- meta_result$I2.w[i]
      pval <- meta_result$pval.random.w[i]

      if (type == "prop") {
        TE_pct <- inv_logit(TE) * 100
        lower_pct <- inv_logit(lower) * 100
        upper_pct <- inv_logit(upper) * 100
        pred.lower_pct <- if (!is.na(pred.lower)) inv_logit(pred.lower) * 100 else NA
        pred.upper_pct <- if (!is.na(pred.upper)) inv_logit(pred.upper) * 100 else NA

        cat(sprintf("Subgroup: %s\n  Pooled Proportion = %.1f%% (95%% CI: %.1f%% - %.1f%%)\n",
                    grp, TE_pct, lower_pct, upper_pct))

        if (!is.na(pred.lower_pct) && !is.na(pred.upper_pct)) {
          cat(sprintf("  Prediction Interval: %.1f%% - %.1f%%\n", pred.lower_pct, pred.upper_pct))
        }

        cat(sprintf("  I² = %.1f%%\n\n", i2))

      } else {
        TE <- if (type == "ratio") exp(TE) else TE
        lower <- if (type == "ratio") exp(lower) else lower
        upper <- if (type == "ratio") exp(upper) else upper
        pred.lower <- if (type == "ratio") exp(pred.lower) else pred.lower
        pred.upper <- if (type == "ratio") exp(pred.upper) else pred.upper

        cat(sprintf("Subgroup: %s\n  Pooled %s = %.2f (95%% CI: %.2f - %.2f)\n",
                    grp, measure, TE, lower, upper))

        if (!is.na(pred.lower) && !is.na(pred.upper)) {
          cat(sprintf("  Prediction Interval: %.2f - %.2f\n", pred.lower, pred.upper))
        }

        cat(sprintf("  p-value = %.3g\n  I² = %.1f%%\n\n", pval, i2))
      }
    }

    if (!is.null(meta_result$Q.b.random)) {
      cat("Test for subgroup differences (random effects model):\n")
      cat(sprintf("  Q = %.2f, df = %d, p = %.3g\n\n",
                  meta_result$Q.b.random,
                  meta_result$df.Q.b.random,
                  meta_result$pval.Q.b.random))
    }

  } else {
    TE <- meta_result$TE.random
    lower <- meta_result$lower.random
    upper <- meta_result$upper.random
    pred.lower <- meta_result$lower.predict
    pred.upper <- meta_result$upper.predict
    i2 <- meta_result$I2
    tau2 <- meta_result$tau2
    pval <- meta_result$pval.random

    if (type == "prop") {
      TE_pct <- inv_logit(TE) * 100
      lower_pct <- inv_logit(lower) * 100
      upper_pct <- inv_logit(upper) * 100
      pred.lower_pct <- if (!is.na(pred.lower)) inv_logit(pred.lower) * 100 else NA
      pred.upper_pct <- if (!is.na(pred.upper)) inv_logit(pred.upper) * 100 else NA

      cat(sprintf("Pooled Proportion = %.1f%% (95%% CI: %.1f%% - %.1f%%)\n", TE_pct, lower_pct, upper_pct))
      if (!is.na(pred.lower_pct) && !is.na(pred.upper_pct)) {
        cat(sprintf("Prediction Interval: %.1f%% - %.1f%%\n", pred.lower_pct, pred.upper_pct))
      }

    } else {
      TE <- if (type == "ratio") exp(TE) else TE
      lower <- if (type == "ratio") exp(lower) else lower
      upper <- if (type == "ratio") exp(upper) else upper
      pred.lower <- if (type == "ratio") exp(pred.lower) else pred.lower
      pred.upper <- if (type == "ratio") exp(pred.upper) else pred.upper

      cat(sprintf("Pooled %s = %.2f (95%% CI: %.2f - %.2f)\n", measure, TE, lower, upper))
      if (!is.na(pred.lower) && !is.na(pred.upper)) {
        cat(sprintf("Prediction Interval: %.2f - %.2f\n", pred.lower, pred.upper))
      }

      cat(sprintf("p-value = %.3g\n", pval))
    }

    cat(sprintf("I² = %.1f%%\nTau² = %.4f\n", i2, tau2))
  }

  cat("----------------------\n\nNotes:\n")
  if (type == "prop") {
    cat("- Proportions are pooled using logit transformation and back-transformed.\n")
    cat("- Random intercept logistic regression model used.\n")
  } else if (measure == "OR") {
    cat("- Pooled OR < 1 suggests lower odds compared to comparator.\n")
  } else if (measure == "RR") {
    cat("- Pooled RR < 1 suggests lower risk compared to comparator.\n")
  } else if (measure == "HR") {
    cat("- Pooled HR < 1 suggests lower hazard (event rate) compared to comparator.\n")
  } else {
    cat(sprintf("- Continuous outcomes pooled using %s.\n", measure))
  }

  cat("- I² measures heterogeneity across studies.\n")
  cat("- Tau² estimates between-study variance.\n")
  cat("- Hartung-Knapp or selected CI adjustment used for random-effects models.\n\n")

  cat("Full Meta-analysis Results:\n----------------------------\n")
  if (type == "prop") {
    cat("\nFormatted Study-Level Results:\n------------------------------\n")
    study_tbl <- tibble::tibble(
      Study = meta_result$studlab,
      Proportion = sprintf("%.1f%%", inv_logit(meta_result$TE) * 100),
      CI = sprintf("[%.1f%% – %.1f%%]", inv_logit(meta_result$lower) * 100, inv_logit(meta_result$upper) * 100)
    )
    print(study_tbl, n = nrow(study_tbl))
  } else {
    print(meta_result)
  }

  invisible(object)
}


#' @export
summary.meta_ratio <- function(object, ...) {
  .summary_meta_generic(object, type = "ratio", ...)
}

#' @export
summary.meta_mean <- function(object, ...) {
  .summary_meta_generic(object, type = "mean", ...)
}
#' @export
print.meta_prop <- function(x, ...) {
  # Print the object as usual (e.g., list of components)
  class(x) <- "list"
  print(x, ...)

  # Add the caveat as a plain-language footer

  cat("\n---\nCaution:\n")
  cat("-- Use $table, $meta.summary, and $influence.analysis for interpretation (%);
      $meta and $influence.meta are on the logit (decimal) scale for compatibility with plots.\n")
  cat("---\n")

}

#' @export
summary.meta_prop <- function(object, ...) {
  .summary_meta_generic(object, type = "prop", ...)
}
#' @export
summary.meta_reg <- function(object, ...) {
  cat("\nMeta-regression Summary\n------------------------\n")
  print(object$meta.summary)

  cat("\nTidy Coefficient Table:\n------------------------\n")
  print(object$table)

  cat("\nNotes:\n")
  cat("- Estimates are on the logit scale unless transformed prior to modeling.\n")
  cat("- R² analog shows % of between-study variance explained by moderators.\n")
  cat("- Use `object$meta` to inspect the full rma model from the metafor package.\n")
  cat("- Use `predict()` or `fitted()` on `object$meta` for further insight.\n")
  invisible(object)
}

