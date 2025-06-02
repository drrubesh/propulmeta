#' @keywords internal
.summary_meta_generic <- function(object, type = c("ratio", "prop", "mean"), ...) {
  type <- match.arg(type)
  meta_result <- object$meta
  model <- object$model
  measure <- object$measure

  cat("\nMeta-analysis Summary\n")
  cat("----------------------\n")

  # Basic header
  cat(sprintf("Number of studies: %d\n", meta_result$k))

  if (type != "prop" && all(c("n.e", "n.c") %in% names(meta_result))) {
    total_n <- sum(meta_result$n.e, meta_result$n.c, na.rm = TRUE)
    cat(sprintf("Total observations: %s (%s in experimental, %s in control)\n",
                formatC(total_n, format = "d", big.mark = ","),
                formatC(sum(meta_result$n.e, na.rm = TRUE), format = "d", big.mark = ","),
                formatC(sum(meta_result$n.c, na.rm = TRUE), format = "d", big.mark = ",")))
    if (!is.null(meta_result$event.e) && !is.null(meta_result$event.c)) {
      cat(sprintf("Total events: %s\n",
                  formatC(sum(meta_result$event.e, meta_result$event.c, na.rm = TRUE),
                          format = "d", big.mark = ",")))
    }
  }

  # Main Pooled Results (No Subgroup)
  if (!object$subgroup) {
    cat("\n")

    if (type == "prop") {
      est <- object$meta.summary$Estimate
      lci <- object$meta.summary$lower
      uci <- object$meta.summary$upper
      pi_lower <- object$meta.summary$pred.lower
      pi_upper <- object$meta.summary$pred.upper

      cat(sprintf("Pooled Proportion = %.1f%% (95%% CI: %.1f%%, %.1f%%)\n", est, lci, uci))
      if (!is.na(pi_lower) && !is.na(pi_upper)) {
        cat(sprintf("Prediction Interval: %.1f%% - %.1f%%\n", pi_lower, pi_upper))
      }

      cat(sprintf("I^2 = %.1f%%\n", object$meta.summary$I2))
      cat(sprintf("Tau^2 = %.4f\n", object$meta.summary$Tau2))

    } else {
      est <- exp(meta_result$TE.random)
      lci <- exp(meta_result$lower.random)
      uci <- exp(meta_result$upper.random)
      pi_lower <- exp(meta_result$lower.predict)
      pi_upper <- exp(meta_result$upper.predict)

      cat(sprintf("Pooled %s = %.2f (95%% CI: %.2f, %.2f)\n", measure, est, lci, uci))
      if (!is.na(pi_lower) && !is.na(pi_upper)) {
        cat(sprintf("Prediction Interval: %.2f - %.2f\n", pi_lower, pi_upper))
      }

      cat(sprintf("p-value = %.3g\n", meta_result$pval.random))
      cat(sprintf("I^2 = %.1f%%\n", meta_result$I2))
      cat(sprintf("Tau^2 = %.4f\n", meta_result$tau2))
    }
  }

  # Subgroup Results
  if (object$subgroup) {
    cat("\nSubgroup Results:\n")
    cat("------------------\n")

    if (!is.null(object$meta.subgroup.summary)) {
      print(object$meta.subgroup.summary)
    }

    if (!is.null(meta_result$Q.b.random)) {
      cat("\nTest for subgroup differences:\n")
      cat(sprintf("Q = %.2f, df = %d, p-value = %.4g\n",
                  meta_result$Q.b.random, meta_result$df.Q.b.random, meta_result$pval.Q.b.random))
    }
  }

  # Notes
  cat("\n----------------------\n\nNotes:\n")
  if (type == "prop") {
    cat("- Proportions are pooled using logit transformation and back-transformed to percentages.\n")
    cat("- P-value is omitted as it is not meaningful for pooled proportions; focus on CI and heterogeneity.\n")
    if (model == "random") {
      cat("- Random-effects logistic model used.\n")
    } else {
      cat("- Fixed-effects logistic model used.\n")
    }
  } else if (measure == "OR") {
    cat("- Pooled OR < 1 suggests lower odds compared to comparator.\n")
  } else if (measure == "RR") {
    cat("- Pooled RR < 1 suggests lower risk compared to comparator.\n")
  } else if (measure == "HR") {
    cat("- Pooled HR < 1 suggests lower hazard compared to comparator.\n")
  } else {
    cat(sprintf("- Continuous outcomes pooled using %s.\n", measure))
  }

  cat("- I² measures the percentage of variability due to heterogeneity rather than chance.\n")
  if (model == "random") {
    cat("- Tau² estimates between-study variance under a random-effects model.\n")
    cat("- Hartung-Knapp or selected CI adjustment used for random-effects models.\n")
  } else {
    cat("- Fixed-effects model assumes no between-study variability (Tau² = 0).\n")
  }

  cat("\n")
  cat("For detailed study-level results, use `object$table`.\n")
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
  cat("- R² analog shows percentage of between-study variance explained by moderators.\n")
  cat("- Use `object$meta` to inspect the full rma() model from the metafor package.\n")
  cat("- Use `predict()` or `fitted()` on `object$meta` for further insights.\n")
  invisible(object)
}

#' @export
print.meta_prop <- function(x, ...) {
  print(unclass(x))  # default printing
  cat("\nWarning: `$meta` and `$influence.meta` contains raw estimates (e.g., logits for proportions).\n")
  cat("For interpretation, please use `summary()`, $table, $meta.summary, and $influence.analysis for interpretation (%).\n")
  invisible(x)
}
