make_bobj <- function() {
  specs <- list(
    data = data.frame(
      int = 1:5,
      num = 1:5 * 1.25,
      char = letters[1:5],
      fact = factor(letters[1:5]),
      supp = LETTERS[1:5]),
    formula = num ~ int + char + fact,
    id = "int",
    roles = data.frame(
      variable = c("num", "int", "char", "fact", "int"),
      type = c("numeric", "numeric", "nominal", "nominal", "numeric"),
      role = c("outcome", "predictor", "predictor", "predictor", "id"),
      info = NA_character_),
    info = list(
      colors = c("1" = "red", "2" = "blue", "3" = "yellow")),
    bag = list(
      clusters = data.frame(
        "clust" = 1:5,
        "name" = letters[1:5]))
    )
  plain <- structure(
    list(
      data = specs$data,
      formula = specs$formula,
      id = specs$id,
      roles = specs$roles,
      info = list(),
      bag = list()),
    class = "bobj")
  full <- structure(
    list(
      data = specs$data,
      formula = specs$formula,
      id = specs$id,
      roles = specs$roles,
      info = specs$info,
      bag = specs$bag),
    class = "bobj")
  list("specs" = specs, "plain" = plain, "full" = full)
}

make_xprts <- function() {

  ggp <- ggplot2::ggplot(iris,
                         ggplot2::aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
    ggplot2::geom_point()
  ply <- plotly::plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species) |>
    plotly::add_markers()
  gt <- iris |>
    group_by(Species) |>
    summarize(Sepal.mean = mean(Sepal.Length),
              Petal.mean = mean(Petal.Length)) |>
    gt::gt(rowname_col = "Species")
  class_df <- data.frame(
    "name" = c("ggp_iris", "gt_iris", "ply_iris"),
    "class" = c("ggplot", "gt", "plotly"),
    row.names = NULL) |>
    dplyr::arrange(name)

  specs <- list(
    "ggp_iris" = ggp,
    "ply_iris" = ply,
    "gt_iris" = gt,
    "class_df" = class_df)

  plain <- structure(
    list("bag" = list()),
    class = "xprts")

  full <- structure(
    list(
      "bag" = list("ggp_iris" = specs$ggp,
                   "ply_iris" = specs$ply,
                   "gt_iris" = specs$gt)),
    class = "xprts")

  list("specs" = specs, "plain" = plain, "full" = full)
}
