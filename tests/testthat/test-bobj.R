
test_that("init_roles", {
  bobj_test <- make_bobj()

  out <- init_roles(data = bobj_test$specs$data,
                    formula = bobj_test$specs$formula,
                    id = bobj_test$specs$id)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- bobj_test$specs$roles
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")
  expect_identical(out, target)
})


test_that("new_bobj", {
  bobj_test <- make_bobj()

  out <- new_bobj(data = bobj_test$specs$data,
                    formula = bobj_test$specs$formula,
                    id = bobj_test$specs$id)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")
  target <- bobj_test$plain
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")
  expect_identical(out, target)
})

test_that("validate_bobj", {
  bobj_test <- make_bobj()
  expect_true(validate_bobj(bobj_test$full))
})

test_that("bobj", {
  bobj_test <- make_bobj()

  out <- bobj(data = bobj_test$specs$data,
              formula = bobj_test$specs$formula,
              id = bobj_test$specs$id)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")
  target <- bobj_test$plain
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")
  expect_identical(out, target)
})

test_that("get_data.bobj: TypeError", {
  bobj_test <- make_bobj()
  out <- bobj_test$specs$data
  expect_error(get_data(out), class = "TypeError")
})

test_that("get_data.bobj", {
  bobj_test <- make_bobj()
  out <- get_data(bobj_test$plain)
  target <- bobj_test$plain$data
  expect_identical(out, target)
})

test_that("set_data.bobj", {
  bobj_test <- make_bobj()
  df <- bobj_test$specs$data
  out <- eflTools::set_data(x = bobj_test$plain, data = df)
  target <- bobj_test$plain$data
  expect_identical(out$data, target)
})

test_that("get_formula.bobj", {
  bobj_test <- make_bobj()
  out <- get_formula(bobj_test$plain)
  target <- bobj_test$plain$formula
  expect_identical(out, target)
})

test_that("set_formula.bobj", {
  bobj_test <- make_bobj()
  frml <- bobj_test$specs$formula
  out <- eflTools::set_formula(x = bobj_test$plain, formula = frml)
  target <- bobj_test$plain$formula
  expect_identical(out$formula, target)
})

test_that("get/set roles.bobj", {
  bobj_test <- make_bobj()
  out <- get_roles(bobj_test$plain)
  target <- bobj_test$plain$roles
  expect_identical(out, target)

  obj <- bobj_test$plain
  new <- bobj_test$plain$roles |>
    bind_rows(list("variable" = "num",
                "type" ="numeric",
                "role" = "test",
                "info" = NA_character_))
  out <- set_roles(obj, roles = new)
  target <- new
  expect_identical(out$roles, target)
})


test_that("get/set info.bobj", {
  bobj_test <- make_bobj()
  out <- get_info(bobj_test$full, name = "colors")
  target <- bobj_test$full$info$colors
  expect_identical(out, target)

  obj <- bobj_test$full
  new <- c(bobj_test$full$info$colors, "new" = "black")
  out <- set_info(obj, name = "colors", value = new)
  expect_identical(out$info$colors, new)
})


test_that("get/set bag.bobj", {
  bobj_test <- make_bobj()
  out <- get_bag(bobj_test$full, name = "clusters")
  target <- bobj_test$full$bag$clusters
  expect_identical(out, target)

  obj <- bobj_test$full
  new <- bobj_test$full$bag$clusters |>
    bind_rows(list("clust" = 6, "name" = letters[6]))
  out <- set_bag(obj, name = "clusters", value = new)
  expect_identical(out$bag$clusters, new)
})
