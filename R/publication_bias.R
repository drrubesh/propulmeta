#' Publication Bias Inspection for Meta-Analysis (Single Plot Option)
#'
#' Performs Egger’s and Begg’s tests, and optionally displays one publication bias plot.
#'
#' @param object A meta-analysis object from `meta_ratio`, `meta_mean`, or `meta_prop`.
#' @param plot_method One of: "original", "trimfill", "contour", "limitmeta".
#' @param save_as "viewer", "pdf", or "png". Default is "viewer".
#' @param filename Optional filename for export.
#' @param width,height Export dimensions in inches (default 10x8).
#'
#' @return A list with Egger and Begg test results and optional models.
#' @export
publication_bias <- function(object,
                             plot_method = NULL,
                             save_as = c("viewer", "pdf", "png"),
                             filename = NULL,
                             width = 10,
                             height = 8) {
  save_as <- match.arg(save_as)

  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("The 'meta' package is required. Please install it using install.packages('meta').", call. = FALSE)
  }
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("The 'metafor' package is required. Please install it using install.packages('metafor').", call. = FALSE)
  }
  if (!requireNamespace("metasens", quietly = TRUE)) {
    stop("The 'metasens' package is required. Please install it using install.packages('metasens').", call. = FALSE)
  }

  meta_obj <- object$meta
  k <- meta_obj$k
  results <- list()

  message("\n📉 Publication Bias Assessment")
  message("----------------------------")
  message("Total studies included: ", k)

  if (k < 10) {
    message("⚠️  Publication bias tests are not recommended when <10 studies.")
    message("➡️  Consider plotting a DOI plot using dmetar::doi.plot().")
    return(invisible(NULL))
  }

  begg <- tryCatch(meta::metabias(meta_obj, method.bias = "rank"), error = function(e) NULL)
  egger <- tryCatch(meta::metabias(meta_obj, method.bias = "linreg"), error = function(e) NULL)

  results$begg <- begg
  results$egger <- egger

  if (!is.null(egger)) {
    message(sprintf("Egger’s Test: z = %.2f, p = %.4f", egger$statistic, egger$p.value))
  }
  if (!is.null(begg)) {
    message(sprintf("Begg’s Test (Rank Correlation): z = %.2f, p = %.4f", begg$statistic, begg$p.value))
  }

  message("\nInterpretation Notes:")
  message("- Egger's p < 0.05 may suggest publication bias or small-study effects.")
  message("- Begg's p < 0.05 may suggest asymmetry in the funnel plot.")
  message("- Visual inspection is still essential.\n")

  if (!is.null(plot_method)) {
    plot_method <- tolower(gsub("countour", "contour", plot_method))
    plot_method <- match.arg(plot_method, choices = c("original", "trimfill", "contour", "limitmeta"))

    original_device <- grDevices::dev.cur()

    if (save_as != "viewer") {
      if (is.null(filename)) {
        ext <- switch(save_as, pdf = "pdf", png = "png")
        filename <- paste0("publication_bias_", plot_method, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
      }
      if (save_as == "pdf") {
        grDevices::pdf(filename, width = width, height = height)
      } else {
        grDevices::png(filename, width = width, height = height, units = "in", res = 300)
      }
    }

    # Plot logic
    if (plot_method == "original") {
      message("📊 Plotting: Funnel Plot (Original)")
      tryCatch(meta::funnel(meta_obj, main = "Funnel Plot"), error = function(e) {
        message("⚠️  Could not plot funnel: ", e$message)
      })
    }

    if (plot_method == "trimfill") {
      message("🔍 Applying: Trim-and-Fill Method")
      tf <- tryCatch(meta::trimfill(meta_obj), error = function(e) NULL)
      if (!is.null(tf)) {
        results$trimfill <- tf
        tryCatch(meta::funnel(tf, main = "Trim-and-Fill Funnel"), error = function(e) {
          message("⚠️  Failed to plot Trim-and-Fill funnel.")
        })
      }
    }

    if (plot_method == "contour") {
      message("🎨 Generating: Contour-Enhanced Funnel Plot")
      rma_obj <- tryCatch(metafor::rma.uni(yi = meta_obj$TE, sei = meta_obj$seTE, method = "REML"), error = function(e) NULL)
      if (!is.null(rma_obj)) {
        tryCatch(
          metafor::funnel(
            rma_obj,
            main = "Contour Funnel",
            shade = c("white", "gray85", "gray70"),
            legend = TRUE
          ),
          error = function(e) {
            message("⚠️  Failed to plot contour-enhanced funnel: ", e$message)
          }
        )
      }
    }

    if (plot_method == "limitmeta") {
      message("📐 Running: Limit Meta-Analysis (metasens)")
      lm <- tryCatch(metasens::limitmeta(meta_obj), error = function(e) {
        message("❌ limitmeta() failed: ", e$message)
        return(NULL)
      })
      if (!is.null(lm)) {
        results$limitmeta <- lm
        tryCatch({
          metasens::funnel.limitmeta(lm, backtransf = TRUE, main = "Limit meta-analysis")
        }, error = function(e) {
          message("⚠️  Failed to plot limitmeta funnel: ", e$message)
        })
      }
    }

    if (save_as %in% c("pdf", "png")) {
      grDevices::dev.off()
      message(glue::glue("✅ Publication bias plot saved as '{filename}'"))
    } else {
      message("📊 Publication bias plot displayed in Viewer.")
    }
  }

  invisible(results)
}
