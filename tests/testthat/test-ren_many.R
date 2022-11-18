test_that("multiplication works", {
  data(iris)
  new_names <- c("sepal.length" = "Sepal.Length", "flower" = "Species")
  iris <- ren_many(iris, ren = new_names)
  expect_identical(names(iris),
                   c("sepal.length", "Sepal.Width", "Petal.Length",
                     "Petal.Width", "flower"))
})
