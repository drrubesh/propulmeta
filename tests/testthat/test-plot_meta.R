test_that("plot_meta dispatches correctly and works invisibly", {
  skip_on_cran()  # CRAN policy: no opening graphical devices

  # Load example data
  data("Olkin95", package = "propulmeta")
  data("dat_normand1999", package = "propulmeta")
  data("dat_bcg", package = "propulmeta")

  # Fit meta-analysis models
  result_ratio <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author",
    model = "random",
    measure = "OR"
  )

  result_mean <- meta_mean(
    data = dat_normand1999,
    mean.e = "m1i",
    sd.e = "sd1i",
    n.e = "n1i",
    mean.c = "m2i",
    sd.c = "sd2i",
    n.c = "n2i",
    studylab = "source",
    model = "random",
    measure = "MD"
  )

  result_prop <- meta_prop(
    data = dat_bcg,
    event = "tpos",
    n = "npos",
    studylab = "author",
    model = "random"
  )

  # Open a null device to avoid opening real graphics window
  pdf(NULL)

  # Expect invisibility for plot_meta
  expect_invisible(plot_meta(result_ratio, save_as = "viewer"))
  expect_invisible(plot_meta(result_mean, save_as = "viewer"))
  expect_invisible(plot_meta(result_prop, save_as = "viewer"))

  # Close the dummy device
  dev.off()

  # Check error for wrong class input
  dummy_object <- list()
  class(dummy_object) <- "unknown_meta"
  expect_error(plot_meta(dummy_object), "'x' must be a meta_ratio, meta_mean, or meta_prop object.")
})
