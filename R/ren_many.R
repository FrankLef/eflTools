#' Rename Many Columns
#'
#' Rename many columns  using a named vector.
#'
#' Rename many columns using a named character vector where the names are the
#' new name and the element the old name. See the example.
#'
#' @param data Dataframe.
#' @param ren Named character vector with c("new name" = "old name").
#' @param ignore_case Ignore case if \code{TRUE} (default).
#'
#' @return Dataframe with new names.
#'
#' @examples
#' df <- iris
#' df <- ren_many(df,
#'  ren = c("sepal.length" = "Sepal.Length", "flower" = "Species"))
#' stopifnot(identical(
#'  names(df),
#'  c("sepal.length", "Sepal.Width", "Petal.Length", "Petal.Width", "flower")
#'  ))
#' @export
ren_many <- function(data, ren, ignore_case = TRUE) {

  if(ignore_case) {
    pos <- match(tolower(names(data)), tolower(ren), nomatch = 0L)
  } else {
    pos <- match(names(data), ren, nomatch = 0L)
  }

  names(data)[pos != 0] <- names(ren)[pos]

  data
}
