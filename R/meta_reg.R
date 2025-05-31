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

  # Extract study labels
  meta_studies <- meta_object$meta$studlab
  meta_studies <- make.unique(meta_studies)  # Force study labels unique like in meta

  # Force original data study labels unique too
  data[[studylab]] <- make.unique(data[[studylab]])

  # Match data to meta-analysis studies
  regression_data <- data[data[[studylab]] %in% meta_studies, , drop = FALSE]
  if (nrow(regression_data) != length(meta_studies)) {
    stop("Mismatch: Number of studies in meta-analysis and meta-regression data differ. Check study labels.")
  }

  # Add effect size inputs
  regression_data$yi <- meta_object$meta$TE
  regression_data$vi <- meta_object$meta$seTE^2

  # Run meta-regression
  reg_model <- metafor::rma(yi = regression_data$yi, vi = regression_data$vi, mods = moderators, data = regression_data, method = "REML")

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
    measure = meta_object$measure,
    call = match.call()
  ), class = "meta_reg")
}
