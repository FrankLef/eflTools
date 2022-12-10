test_that("new_xprts", {
  xprts_test <- make_xprts()

  out <- new_xprts()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")
  target <- xprts_test$plain
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")
  expect_identical(out, target)
})

test_that("validate_xprts", {
  xprts_test <- make_xprts()
  expect_s3_class(validate_xprts(xprts_test$plain), class ="xprts")
})


test_that("xprts", {
  xprts_test <- make_xprts()

  out <- xprts()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")
  target <- xprts_test$plain
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")
  expect_identical(out, target)
})

test_that("get/set bag.xprts", {
  xprts_test <- make_xprts()
  out <- get_bag(xprts_test$full, name = "ggp_iris")
  target <- xprts_test$full$bag$ggp_iris
  expect_identical(out, target)

  obj <- xprts_test$full
  new <- xprts_test$specs$ggp_iris
  out <- set_bag(obj, name = "ggp_iris", value = new)
  expect_identical(out$bag$ggp_iris, new)
})


test_that("xprt_classes.xprts", {
  xprts_test <- make_xprts()
  out <- xprt_classes(xprts_test$full)
  target <- xprts_test$specs$class_df
  expect_identical(out, target)
})


test_that("xprt_ggplot.xprts", {
  xprts_test <- make_xprts()
  dir <- tempdir()
  out <- xprt_ggplot.xprts(xprts_test$full, path = dir)
  out_file <- file.path(dir, "ggp_iris.png")
  expect_true(file.exists(out_file))
  unlink(out_file)  # delete file
  expect_identical(out, xprts_test$full)
})

test_that("xprt_plotly.xprts", {
  xprts_test <- make_xprts()
  dir <- tempdir()
  out <- xprt_plotly.xprts(xprts_test$full, path = dir)
  out_file <- file.path(dir, "ply_iris.qs")
  expect_true(file.exists(out_file))
  unlink(out_file)  # delete file
  expect_identical(out, xprts_test$full)
})

test_that("xprt_gt.xprts", {
  xprts_test <- make_xprts()
  dir <- tempdir()
  out <- xprt_gt.xprts(xprts_test$full, path = dir)
  out_file <- file.path(dir, "gt_iris.html")
  expect_true(file.exists(out_file))
  unlink(out_file)  # delete file
  expect_identical(out, xprts_test$full)
})

test_that("xprt_all.xprts", {
  xprts_test <- make_xprts()
  dir <- tempdir()
  out <- xprt_all.xprts(xprts_test$full, path = dir, is_xprt = TRUE)

  out_file <- file.path(dir, "ggp_iris.png")
  expect_true(file.exists(out_file))
  unlink(out_file)  # delete file

  out_file <- file.path(dir, "ply_iris.qs")
  expect_true(file.exists(out_file))
  unlink(out_file)  # delete file

  out_file <- file.path(dir, "gt_iris.html")
  expect_true(file.exists(out_file))
  unlink(out_file)  # delete file

  expect_identical(out, xprts_test$full)

})

test_that("empty xprts", {
  # when xprts is empty, the sripct cause an error in RStudio

  out <- xprts()
  dir <- tempdir()
  expect_output(xprt_all.xprts(out, path = dir, is_xprt = FALSE),
                regexp = "The export object is empty. Nothing done.")
})
