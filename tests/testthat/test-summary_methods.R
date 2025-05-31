test_that("summary() methods run without error", {
  data("dat_bcg", package = "propulmeta")
  data("Olkin95", package = "propulmeta")
  data("amlodipine", package = "propulmeta")

  # meta_prop
  meta_p <- meta_prop(
    data = dat_bcg,
    event = "tpos",
    n = "npos",
    studylab = "author"
  )

  # meta_ratio
  meta_r <- meta_ratio(
    data = Olkin95,
    event.e = "event.e",
    n.e = "n.e",
    event.c = "event.c",
    n.c = "n.c",
    studylab = "author"
  )

  # meta_mean
  amlodipine$sd.amlo <- sqrt(amlodipine$var.amlo)
  amlodipine$sd.plac <- sqrt(amlodipine$var.plac)

  meta_m <- meta_mean(
    data = amlodipine,
    mean.e = "mean.amlo",
    sd.e = "sd.amlo",
    n.e = "n.amlo",
    mean.c = "mean.plac",
    sd.c = "sd.plac",
    n.c = "n.plac",
    studylab = "study"
  )

  expect_invisible(summary(meta_p))
  expect_invisible(summary(meta_r))
  expect_invisible(summary(meta_m))
})
