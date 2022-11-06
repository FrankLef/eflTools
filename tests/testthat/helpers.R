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
