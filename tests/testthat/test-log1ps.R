test_that("log1ps & expm1s: default", {
  x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)

  out <- log1ps(x)
  target <- sign(x) * log1p(abs(x))

  expect_identical(out, target)

  y <- expm1s(target)
  expect_equal(x, y, tolerance = 1e-14)
})

test_that("log1ps & expm1s: base", {
  the_base <- 10L

  x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)

  out <- log1ps(x, base = the_base)
  target <- sign(x) * log1p(abs(x)) / log(the_base)

  expect_identical(out, target)

  y <- expm1s(target, base = the_base)
  expect_equal(x, y, tolerance = 1e-14)
})

test_that("log1ps & expm1s: NA", {
  x <- c(-100, NA_real_, -1, NA_real_, 0, 0.1, NA_real_, 10, NA_real_)

  out <- log1ps(x)
  target <- sign(x) * log1p(abs(x))

  expect_identical(out, target)

  y <- expm1s(target)
  expect_equal(x, y, tolerance = 1e-14)
})

test_that("log1ps10 & expm1s10", {
  x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)

  out <- log1ps10(x)
  target <- sign(x) * log1p(abs(x)) / log(10)

  expect_identical(out, target)

  y <- expm1s10(target)
  expect_equal(x, y, tolerance = 1e-14)
})
