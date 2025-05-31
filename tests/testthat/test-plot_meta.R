test_that("plot_meta saves forest plots without error", {
  skip_on_cran()  # Prevent running on CRAN
  skip_on_ci()    # Prevent on CI systems like GitHub Actions

  # Setup: Load example data and fit meta-analysis
  library(meta)
  data(Olkin95)
  result <- meta::metabin(event.e, n.e, event.c, n.c,
                          data = Olkin95,
                          studlab = author,
                          sm = "OR", method = "Inverse")

  # Wrap result to mimic a `meta_ratio` structure expected by plot_meta
  meta_ratio_obj <- list(meta = result, table = result$studlab, subgroup = FALSE)
  class(meta_ratio_obj) <- "meta_ratio"

  # Save plot to temporary file
  temp_file <- tempfile(fileext = ".pdf")

  expect_invisible(plot_meta(meta_ratio_obj, filename = temp_file, save_as = "pdf"))
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})
