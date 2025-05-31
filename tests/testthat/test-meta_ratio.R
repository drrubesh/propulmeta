test_that("meta_ratio() works correctly on Olkin95", {
  data("Olkin95", package = "propulmeta")

  result <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author"
  )

  expect_s3_class(result, "meta_ratio")
  expect_true(!is.null(result$meta))
  expect_gt(nrow(result$table), 0)
})
