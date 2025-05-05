#' Meta-regression for meta-analysis results
#'
#' Performs meta-regression on a fitted meta-analysis object from meta_ratio, meta_mean, or meta_prop.
#'
#' @param meta_object A fitted object from meta_ratio, meta_mean, or meta_prop.
#' @param data The original dataset used to fit the meta-analysis model.
#' @param moderators A formula specifying the moderators (e.g., ~ age + gender).
#' @param studylab Character string naming the study label column in `data` (e.g., "author").
#' @return A list of class 'meta_reg' with the regression model, tidy coefficients, R² analog, and tau² estimates.
#' @export
meta_reg <- function(meta_object, data, moderators, studylab) {
  # Check class
  if (!inherits(meta_object, c("meta_prop", "meta_ratio", "meta_mean"))) {
    stop("meta_object must be from meta_prop, meta_ratio, or meta_mean.")
  }

  if (missing(moderators)) {
    stop("You must provide a moderators formula, e.g., ~ age or ~ group + dose.")
  }

  if (missing(studylab)) {
    stop("You must provide the study label column name in your data (e.g., studylab = 'author').")
  }

  # Extract table and check for study variable
  table <- meta_object$table
  if (!"Study" %in% names(table)) {
    if (studylab %in% names(table)) {
      table$Study <- table[[studylab]]
    } else {
      stop("Study label not found in meta_object$table. Provide a correct 'studylab'.")
    }
  }

  if (!(studylab %in% names(data))) {
    stop("Study label not found in data. Provide a correct 'studylab'.")
  }

  # Restrict to relevant and unique rows
  study_labels <- unique(table$Study)
  data <- data[data[[studylab]] %in% study_labels, , drop = FALSE]
  data <- data[!duplicated(data[[studylab]]), ]
  data$Study <- data[[studylab]]

  # Merge meta table with original data
  df <- merge(table, data, by = "Study")

  # Calculate effect size inputs
  if (inherits(meta_object, "meta_prop")) {
    df$yi <- meta_object$meta$TE
    df$vi <- meta_object$meta$seTE^2
    measure <- "Proportion (logit)"
  } else if (inherits(meta_object, "meta_ratio")) {
    df$yi <- meta_object$meta$TE
    df$vi <- meta_object$meta$seTE^2
    measure <- paste0(meta_object$measure, " (logit)")
  } else if (inherits(meta_object, "meta_mean")) {
    df$yi <- meta_object$meta$TE
    df$vi <- meta_object$meta$seTE^2
    measure <- "MD (logit)"
  }

  # Run meta-regression
  reg_model <- metafor::rma(yi = df$yi, vi = df$vi, mods = moderators, data = df, method = "REML")

  # Tau² null and model
  tau2_null <- meta_object$meta$tau2
  tau2_model <- reg_model$tau2
  r2_analog <- round(100 * (tau2_null - tau2_model) / tau2_null, 2)

  # Tidy coefficient table
  tidy_tbl <- tibble::tibble(
    Term = rownames(reg_model$beta),
    Estimate = as.numeric(reg_model$beta),
    CI.Lower = reg_model$ci.lb,
    CI.Upper = reg_model$ci.ub,
    p.value = reg_model$pval
  )

  # Summary
  meta_summary <- tibble::tibble(
    tau2_null = round(tau2_null, 2),
    tau2 = round(tau2_model, 2),
    R2_analog = r2_analog,
    QE_pval = reg_model$QEp,
    QM = as.numeric(reg_model$QM)
  )

  structure(list(
    model = "meta-regression",
    meta = reg_model,
    table = tidy_tbl,
    meta.summary = meta_summary,
    r2_analog = r2_analog,
    measure = measure,
    call = match.call()
  ), class = "meta_reg")
}
