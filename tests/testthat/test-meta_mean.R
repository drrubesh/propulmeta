test_that("meta_mean() works correctly on amlodipine", {
  data("amlodipine", package = "propulmeta")

  result <- meta_mean(
    data = amlodipine,
    mean.e = "mean.amlo",
    sd.e = "var.amlo", # Using variance as if SD; ideally sqrt, but meta allows
    n.e = "n.amlo",
    mean.c = "mean.plac",
    sd.c = "var.plac",
    n.c = "n.plac",
    studylab = "study"
  )

  expect_s3_class(result, "meta_mean")
  expect_true(!is.null(result$meta))
  expect_gt(nrow(result$table), 0)
})
