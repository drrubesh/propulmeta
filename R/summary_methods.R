#' @keywords internal
.summary_meta_generic <- function(object, type = c("ratio", "prop", "mean"), ...) {
  type <- match.arg(type)
  meta_result <- object$meta
  measure <- object$measure
  model <- object$model
  inv_logit <- function(x) exp(x) / (1 + exp(x))

  cat("\nMeta-analysis Summary\n")
  cat("----------------------\n")

  ## Basic header
  if (type != "prop" && all(c("n.e", "n.c") %in% names(meta_result))) {
    total_n <- sum(meta_result$n.e, meta_result$n.c, na.rm = TRUE)
    cat(sprintf("Number of studies: %d\n", meta_result$k))
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

  ## Subgroup Results
  if (object$subgroup) {
    cat("\nSubgroup Results:\n")
    cat("------------------\n")
    if (!is.null(object$meta.summary)) {
      # For meta_prop
      print(object$meta.summary)
    } else if (!is.null(object$table$subgroup)) {
      # For meta_ratio and meta_mean
      subgroup_summary <- object$table |>
        dplyr::group_by(subgroup) |>
        dplyr::summarise(
          k = dplyr::n(),
          Estimate = if (type == "prop") inv_logit(mean(TE)) else exp(mean(TE)),
          lower = if (type == "prop") inv_logit(mean(lower)) else exp(mean(lower)),
          upper = if (type == "prop") inv_logit(mean(upper)) else exp(mean(upper)),
          Tau2 = meta_result$tau2.within,
          I2 = meta_result$I2.within
        )

      subgroup_split <- split(subgroup_summary, subgroup_summary$subgroup)

      for (sg in names(subgroup_split)) {
        sg_row <- subgroup_split[[sg]]
        cat(sprintf("%s:\n", sg_row$subgroup))
        if (type == "prop") {
          cat(sprintf("  Pooled Proportion = %.2f (95%% CI: %.2f, %.2f)\n", sg_row$Estimate, sg_row$lower, sg_row$upper))
        } else {
          cat(sprintf("  Pooled Estimate = %.2f (95%% CI: %.2f, %.2f)\n", sg_row$Estimate, sg_row$lower, sg_row$upper))
        }
        cat(sprintf("  Tau² = %.4f, I² = %.1f%%\n\n", sg_row$Tau2, sg_row$I2))
      }
    }

    # Test for subgroup differences
    if (!is.null(meta_result$Q.b.random)) {
      cat("Test for subgroup differences:\n")
      cat(sprintf("Q = %.2f, df = %d, p-value = %.4g\n",
                  meta_result$Q.b.random, meta_result$df.Q.b.random, meta_result$pval.Q.b.random))
    }
  }

  ## Notes
  cat("\n----------------------\n\nNotes:\n")
  if (type == "prop") {
    cat("- Proportions are pooled using logit transformation and back-transformed.\n")
    cat("- P-value is omitted because hypothesis testing is not meaningful for proportions; focus on the pooled estimate and confidence intervals.\n")
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
    cat("- Pooled HR < 1 suggests lower hazard (event rate) compared to comparator.\n")
  } else {
    cat(sprintf("- Continuous outcomes pooled using %s.\n", measure))
  }
  cat("- I² measures heterogeneity across studies.\n")
  if (model == "random") {
    cat("- Tau² estimates between-study variance.\n")
    cat("- Hartung-Knapp or selected CI adjustment used for random-effects models.\n")
  } else {
    cat("- Fixed-effects model assumes no between-study heterogeneity.\n")
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
  cat("- R^2 analog shows % of between-study variance explained by moderators.\n")
  cat("- Use `object$meta` to inspect the full `rma()` model from the metafor package.\n")
  cat("- Use `predict()` or `fitted()` on `object$meta` for further insight.\n")
  invisible(object)
}

#' @export
print.meta_prop <- function(x, ...) {
  print(unclass(x))  # default printing
  cat("\nWarning: `$meta` contains raw estimates (e.g., logits for proportions).\n")
  cat("For interpretation, please use `summary()`.\n")
  invisible(x)
}
