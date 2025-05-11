#' Bubble Plot for Meta-Regression
#'
#' Displays a bubble plot for a `meta_reg` object using `metafor::regplot()`.
#'
#' @param meta_reg_object A `meta_reg` object created with `meta_reg()`.
#' @param moderator Character. Name of moderator variable (required if multiple predictors).
#' @param plot_all_levels Logical. For categorical moderators, plot each dummy variable (default = TRUE).
#' @param save_as "viewer", "pdf", or "png". Default is "viewer".
#' @param filename Optional file name if exporting.
#' @param width,height Dimensions in inches for export. Defaults: width = 10, height = 8.
#' @param ... Additional arguments passed to `metafor::regplot()`.
#'
#' @return A bubble plot rendered or saved.
#' @export
bubble_plot <- function(meta_reg_object,
                        moderator = NULL,
                        plot_all_levels = TRUE,
                        save_as = c("viewer", "pdf", "png"),
                        filename = NULL,
                        width = 10,
                        height = 8,
                        ...) {
  save_as <- match.arg(save_as)

  if (!inherits(meta_reg_object, "meta_reg")) {
    stop("❌ Input must be a 'meta_reg' object.")
  }

  model <- meta_reg_object$meta
  model_matrix <- model$X
  mod_cols <- colnames(model_matrix)
  data_used <- model$data

  # File handling
  original_device <- grDevices::dev.cur()
  if (save_as != "viewer") {
    if (is.null(filename)) {
      ext <- switch(save_as, pdf = "pdf", png = "png")
      filename <- paste0("bubble_plot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", ext)
    }
    if (save_as == "pdf") {
      grDevices::pdf(filename, width = width, height = height)
    } else {
      grDevices::png(filename, width = width, height = height, units = "in", res = 300)
    }
  }

  # Auto handling if only one moderator
  if (is.null(moderator)) {
    if (ncol(model_matrix) > 2) {
      stop("❌ Must specify the 'moderator' argument when multiple predictors are present.")
    }
    tryCatch({
      metafor::regplot(model, ...)
    }, error = function(e) {
      message("❌ Failed to generate bubble plot: ", e$message)
    })
    if (save_as %in% c("pdf", "png")) {
      grDevices::dev.off()
      message(glue::glue("✅ Bubble plot saved as '{filename}'"))
    } else {
      message("📊 Bubble plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` to export.")
    }
    return(invisible(TRUE))
  }

  # Categorical?
  is_cat <- is.factor(data_used[[moderator]]) || is.character(data_used[[moderator]])

  if (is_cat && plot_all_levels) {
    dummy_terms <- grep(paste0("^", moderator), mod_cols, value = TRUE)
    if (length(dummy_terms) == 0) {
      stop("❌ No dummy variables found for categorical moderator: ", moderator)
    }

    for (term in dummy_terms) {
      message("📌 Plotting for: ", term)
      tryCatch({
        metafor::regplot(model, mod = which(mod_cols == term), ...)
      }, error = function(e) {
        message("❌ Failed to generate bubble plot for ", term, ": ", e$message)
      })
    }
  } else {
    mod_index <- if (moderator %in% mod_cols) {
      which(mod_cols == moderator)
    } else {
      match_term <- grep(paste0("^", moderator), mod_cols, ignore.case = TRUE)
      if (length(match_term) == 0) stop("❌ Could not find the moderator in model matrix.")
      if (length(match_term) > 1) {
        message("ℹ️  Multiple matches for '", moderator, "'. Using: ", mod_cols[match_term[1]])
      }
      match_term[1]
    }

    tryCatch({
      metafor::regplot(model, mod = mod_index, ...)
    }, error = function(e) {
      message("❌ Failed to generate bubble plot: ", e$message)
    })
  }

  if (save_as %in% c("pdf", "png")) {
    grDevices::dev.off()
    message(glue::glue("✅ Bubble plot saved as '{filename}'"))
  } else {
    message("📊 Bubble plot displayed in Viewer. Use `save_as = 'pdf'` or `'png'` to export.")
  }

  invisible(TRUE)
}
