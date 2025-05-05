#' Bubble Plot for Meta-Regression
#'
#' Displays a bubble plot for a meta_reg object using metafor::regplot().
#'
#' @param meta_reg_object A `meta_reg` object created with `meta_reg()`.
#' @param moderator Character. Name of moderator variable (required if multiple predictors).
#' @param plot_all_levels Logical. For categorical moderators, plot each dummy variable (default = TRUE).
#' @param ... Additional graphical arguments passed to `metafor::regplot()`.
#'
#' @return A bubble plot to the graphics device.
#' @export
bubble_plot <- function(meta_reg_object, moderator = NULL, plot_all_levels = TRUE, ...) {
  if (!inherits(meta_reg_object, "meta_reg")) {
    stop("Input must be a 'meta_reg' object.")
  }

  cat("Bubble Plot for Meta-Regression\n")
  cat("-------------------------------\n")
  cat(paste0("Effect Measure: ", meta_reg_object$measure, "\n"))
  cat(paste0("R2 Analog: ", meta_reg_object$r2_analog, "% of heterogeneity explained\n\n"))

  model <- meta_reg_object$meta
  model_matrix <- model$X
  mod_cols <- colnames(model_matrix)
  data_used <- model$data

  if (is.null(moderator)) {
    if (ncol(model_matrix) > 2) {
      stop("❌ Must specify the 'moderator' argument when multiple predictors are present.")
    }
    tryCatch({
      # Respect existing mfrow layout (do not reset user-defined layout)
      par(mar = c(5, 4, 3, 2), xpd= NA)  # Optional: tighten margins for better grid layout

      metafor::regplot(model, ...)
    }, error = function(e) {
      message("❌ Failed to generate bubble plot: ", e$message)
    })
    return(invisible(NULL))
  }

  # Check if the moderator is categorical
  is_cat <- is.factor(data_used[[moderator]]) || is.character(data_used[[moderator]])

  # Handle categorical moderators
  if (is_cat && plot_all_levels) {
    dummy_terms <- grep(paste0("^", moderator), mod_cols, value = TRUE)
    if (length(dummy_terms) == 0) {
      stop("❌ No dummy variables found for categorical moderator: ", moderator)
    }

    for (term in dummy_terms) {
      message("📌 Plotting for: ", term)
      tryCatch({
        # Respect existing mfrow layout (do not reset user-defined layout)
        par(mar = c(5, 4, 3, 2), xpd= NA)  # Optional: tighten margins for better grid layout

        metafor::regplot(model, mod = which(mod_cols == term), ...)
      }, error = function(e) {
        message("❌ Failed to generate bubble plot for ", term, ": ", e$message)
      })
    }

  } else {
    # Handle numeric or single dummy variable
    mod_index <- if (moderator %in% mod_cols) {
      which(mod_cols == moderator)
    } else {
      match_term <- grep(paste0("^", moderator), mod_cols, ignore.case = TRUE)
      if (length(match_term) == 0) stop("❌ Could not find the moderator in model matrix.")
      if (length(match_term) > 1) {
        message("ℹ️  Multiple dummy variables found for moderator '", moderator, "'. Using first match: ", mod_cols[match_term[1]])
      }
      match_term[1]
    }

    tryCatch({
      # Respect existing mfrow layout (do not reset user-defined layout)
      par(mar = c(5, 4, 3, 2), xpd= NA)  # Optional: tighten margins for better grid layout

      metafor::regplot(model, mod = mod_index, ...)
    }, error = function(e) {
      message("❌ Failed to generate bubble plot: ", e$message)
    })
  }

  invisible(NULL)
}
