test_that("input errors", {
  a_file <- file.path(getwd(), "wrong.xlsx")
  rgx <- "Assertion on 'file' failed"
  expect_error(gather_acctg_tags(file = a_file), regexp = rgx)
})


test_that("process file", {
  a_file <- system.file("extdata", "fs_tags.xlsx", package = "eflTools",
                        mustWork = TRUE)
  out <- readxl::read_xlsx(a_file, sheet = "tbl_imp_tags")
  # cat("\n")
  # str(out)
  # cat("\n")
  expect_s3_class(out, class = "data.frame")
  expect_identical(dim(out), c(83L, 47L))
})

test_that("gather the data", {
  a_file <- system.file("extdata", "fs_tags.xlsx", package = "eflTools",
                        mustWork = TRUE)
  out <- gather_acctg_tags(a_file)
  # cat("\n")
  # str(out)
  # cat("\n")

  expect_s3_class(out, class = "data.frame")
  expect_identical(dim(out), c(3569L, 6L))
})
