#' Publication Bias Inspection for Meta-Analysis (Single Plot Only)
#'
#' Performs Egger’s and Begg’s tests, and optionally displays one publication bias plot.
#'
#' @param object A meta-analysis object from meta_ratio, meta_mean, or meta_prop.
#' @param plot_method Character string: "original", "trimfill", "contour", or "limitmeta".
#' @return A list with Egger and Begg test results and optional models.
#' @export
publication_bias <- function(object, plot_method = NULL) {
  if (!requireNamespace("meta", quietly = TRUE)) install.packages("meta")
  if (!requireNamespace("metafor", quietly = TRUE)) install.packages("metafor")
  if (!requireNamespace("metasens", quietly = TRUE)) install.packages("metasens")

  meta_obj <- object$meta
  k <- meta_obj$k
  results <- list()

  cat("\nPublication Bias Assessment\n----------------------------\n")
  cat(paste("Total studies included:", k, "\n"))

  if (k < 10) {
    cat("\n⚠️  Publication bias tests are not recommended when <10 studies.\n")
    cat("➡️  Consider plotting a DOI plot using the 'dmetar::doi.plot()' function.\n\n")
    return(invisible(NULL))
  }

  begg <- tryCatch(meta::metabias(meta_obj, method.bias = "rank"), error = function(e) NULL)
  egger <- tryCatch(meta::metabias(meta_obj, method.bias = "linreg"), error = function(e) NULL)

  results$begg <- begg
  results$egger <- egger

  if (!is.null(egger)) cat(sprintf("Egger’s Test:\n  z = %.2f, p = %.4f\n", egger$statistic, egger$p.value))
  if (!is.null(begg)) cat(sprintf("Begg’s Test (Rank Correlation):\n  z = %.2f, p = %.4f\n", begg$statistic, begg$p.value))

  cat("\nInterpretation Notes:\n")
  cat("- Egger's p < 0.05 suggests small-study effects or publication bias.\n")
  cat("- Begg's p < 0.05 suggests asymmetry in funnel plot.\n")
  cat("- Visual inspection is crucial; statistical tests can be underpowered.\n\n")

  if (!is.null(plot_method)) {
    plot_method <- tolower(plot_method)
    plot_method <- gsub("countour", "contour", plot_method)  # typo fix
    plot_method <- match.arg(plot_method, choices = c("original", "trimfill", "contour", "limitmeta"))

    # Respect existing mfrow layout (do not reset user-defined layout)
    par(mar = c(5, 4, 3, 2), xpd= NA)  # Optional: tighten margins for better grid layout


    if (plot_method == "original") {
      cat("→ Plotting: Funnel Plot (Original)\n")
      tryCatch(meta::funnel(meta_obj, main = "Funnel Plot"), error = function(e) {
        message("⚠️  Could not plot: ", e$message)
      })
    }

    if (plot_method == "trimfill") {
      cat("→ Applying: Trim-and-Fill Method\n")
      tf <- tryCatch(meta::trimfill(meta_obj), error = function(e) NULL)
      if (!is.null(tf)) {
        results$trimfill <- tf
        tryCatch(meta::funnel(tf, main = "Trim-and-Fill Funnel"), error = function(e) {
          message("⚠️  Failed to plot Trim-and-Fill funnel.")
        })
      }
    }

    if (plot_method == "contour") {
      cat("→ Generating: Contour-Enhanced Funnel Plot\n")
      rma_obj <- tryCatch(metafor::rma.uni(yi = meta_obj$TE, sei = meta_obj$seTE, method = "REML"), error = function(e) NULL)
      if (!is.null(rma_obj)) {
        tryCatch(metafor::funnel(rma_obj,
                                 main = "Contour Funnel",
                                 shade = c("white", "gray85", "gray70"),
                                 legend = TRUE), error = function(e) {
                                   message("⚠️  Failed to plot contour-enhanced funnel.")
                                 })
      }
    }

    if (plot_method == "limitmeta") {
      cat("→ Running: Limit Meta-Analysis (metasens)\n")
      lm <- tryCatch(metasens::limitmeta(meta_obj), error = function(e) {
        message("❌ limitmeta() failed: ", e$message)
        return(NULL)
      })
      if (!is.null(lm)) {
        results$limitmeta <- lm
        tryCatch({
          metasens::funnel.limitmeta(lm, backtransf = TRUE, main = "Limit meta-analysis")
        }, error = function(e) {
          message("⚠️  Failed to plot limitmeta result: ", e$message)
        })
      }
    }
  }

  invisible(results)
}
