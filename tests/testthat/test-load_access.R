test_that("input errors", {
  rgx <- "Assertion on 'file' failed"
  expect_error(load_access(dir = getwd(), db = "wrong.accdb"), regexp = rgx)

  # invalid file format
  a_file <- system.file("extdata", "db_error.accdb", package = "eflTools",
                        mustWork = TRUE)
  a_dir <- dirname(a_file)
  a_db <- basename(a_file)
  rgx <- "Assertion on 'qries' failed"
  expect_error(load_access(dir = a_dir, db = a_db), regexp = rgx)
})

test_that("get data from MS Access", {
  a_file <- system.file("extdata", "db_MsAccess.accdb", package = "eflTools",
                        mustWork = TRUE)
  expect_true(file.exists(a_file))
  a_dir <- dirname(a_file)
  a_db <- basename(a_file)
  out <- load_access(dir = a_dir, db = a_db, qries = c("clients" = "qry_clients"))
  expect_identical(out, 1)
  # must remove clients from environment after the test
  rm(clients, envir = .GlobalEnv)
})
