test_that("meta_prop() works correctly on dat_bcg", {
  data("dat_bcg", package = "propulmeta")

  result <- meta_prop(
    data = dat_bcg,
    event = "tpos",
    n = "npos",
    studylab = "author"
  )

  expect_s3_class(result, "meta_prop")
  expect_true(!is.null(result$meta))
  expect_gt(nrow(result$table), 0)
})
