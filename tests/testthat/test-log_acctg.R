test_that("log_acctg & exp_acctg: default", {
  x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)
  out <- log_acctg(x)
  target <- c(-2, -1, -1/10, -0.1/10, 0, 0.1/10, 1/10, 1, 2)
  expect_identical(out, target)

  y <- exp_acctg(target)
  expect_identical(y, x)

})

test_that("log_acctg & exp_acctg: base", {
  the_base <- exp(1)

  x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)
  out <- log_acctg(x, base = the_base)
  target <- c(-log(100), -log(10),
              -1/the_base, -0.1/the_base, 0, 0.1/the_base, 1/the_base,
              log(10), log(100))
  expect_identical(out, target)

  y <- exp_acctg(target, base = the_base)
  expect_equal(y, x, tolerance = 1e-14)
})

test_that("log_acctg & exp_acctg: NA", {
  x <- c(-100, NA_real_, -1, NA_real_, 0, 0.1, NA_real_, 10, NA_real_)
  out <- log_acctg(x)
  target <- c(-2, NA_real_, -1/10, NA_real_, 0, 0.1/10, NA_real_, 1, NA_real_)
  expect_identical(out, target)

  y <- exp_acctg(target)
  expect_identical(y, x)

})
