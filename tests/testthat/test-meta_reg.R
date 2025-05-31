# tests/testthat/test-meta_reg.R

test_that("meta_reg works for basic univariate meta-regression", {
  data("Olkin95", package = "propulmeta")
  result <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author",
    model = "random",
    measure = "OR"
  )

  reg_result <- meta_reg(
    meta_object = result,
    data = Olkin95,
    moderators = ~ year,
    studylab = "author"
  )

  expect_s3_class(reg_result, "meta_reg")
  expect_true(all(c("meta", "table", "meta.summary", "measure") %in% names(reg_result)))
  expect_equal(reg_result$model, "meta-regression")
})

test_that("meta_reg works with multiple moderators", {
  data("Olkin95", package = "propulmeta")
  Olkin95$year_cat <- ifelse(Olkin95$year < 1975, "Before 1975", "After 1975")

  result <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author",
    model = "random",
    measure = "OR"
  )

  reg_result_multi <- meta_reg(
    meta_object = result,
    data = Olkin95,
    moderators = ~ year + year_cat,
    studylab = "author"
  )

  expect_s3_class(reg_result_multi, "meta_reg")
  expect_gt(nrow(reg_result_multi$table), 1)  # Should have multiple coefficients
})

test_that("meta_reg handles interaction terms", {
  data("Olkin95", package = "propulmeta")
  Olkin95$year_cat <- ifelse(Olkin95$year < 1975, "Before 1975", "After 1975")

  result <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author",
    model = "random",
    measure = "OR"
  )

  reg_result_interaction <- meta_reg(
    meta_object = result,
    data = Olkin95,
    moderators = ~ year * year_cat,
    studylab = "author"
  )

  expect_s3_class(reg_result_interaction, "meta_reg")
  expect_true(any(grepl(":", reg_result_interaction$table$Term))) # Check interaction present
})

test_that("meta_reg output contains valid numeric columns", {
  data("Olkin95", package = "propulmeta")

  result <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author",
    model = "random",
    measure = "OR"
  )

  reg_result <- meta_reg(
    meta_object = result,
    data = Olkin95,
    moderators = ~ year,
    studylab = "author"
  )

  expect_true(is.numeric(reg_result$meta.summary$tau2))
  expect_true(is.numeric(reg_result$meta.summary$R2_analog))
  expect_true(is.numeric(reg_result$meta.summary$QE_pval))
})
