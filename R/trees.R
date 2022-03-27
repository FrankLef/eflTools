
#' Create a subtree dataframe
#'
#' Create a subtree dataframe.
#'
#' Create a subtree dataframe used to build a tree with integer as id.
#'
#' @param parents Integer() with parent's ids.
#' @param specs List of tree specs.
#' @param level The tree level.
#'
#' @return Dataframe with subtree.
#' @export
#'
#' @examples
#' root <- 1L
#' specs <- list(
#'  l1 = list(digits = 100, nnodes = 9),
#'  l2 = list(digits = 100, nnodes = 19),
#'  l3 = list(digits = 100, nnodes = 29))
#' df <- data.frame(
#'  parent = root,
#'  child = root,
#'  level = 0L)
#'  out <- create_subtree(parents = df$child, specs[[1]], level = 1)
#' stopifnot((nrow(out) == specs[[1]]$nnodes))
create_subtree <- function(parents, specs, level) {
  checkmate::assert_integerish(parents)
  checkmate::assert_list(specs)
  checkmate::assert_count(level, positive = FALSE)

  assertthat::assert_that(specs$nnodes < specs$digits)

  ids <- seq_len(specs$nnodes)
  ls <- lapply(X = parents, FUN = function(x) {
    data.frame(parent = as.integer(x),
               child = as.integer(x * specs$digits + ids),
               level = as.integer(level))
  })
  do.call(rbind, ls)
}


#' Create a tree with integer id
#'
#' Create a tree with integer id.
#'
#' Create a tree with integer id and level in a third column.
#'
#' @param specs List of tree specs.
#' @param root Integer identifying the integer, default is 1.
#'
#' @return Dataframe with tree.
#' @export
#'
#' @examples
#' specs <- list(
#'  l1 = list(digits = 100, nnodes = 9),
#'  l2 = list(digits = 100, nnodes = 19),
#'  l3 = list(digits = 100, nnodes = 29))
#' create_tree(specs)
create_tree <- function(specs, root = 1L) {
  checkmate::assert_list(specs)
  checkmate::assert_number(root, finite = TRUE)

  lst <- vector(mode = "list", length = length(specs) + 1)

  # create root
  lst[[1]] <- data.frame(
    parent = root,
    child = root,
    level = 0L
  )

  # create the subtrees
  for (i in seq_along(specs)) {
    lst[[i + 1]] <- create_subtree(parents = lst[[i]]$child,
                                   specs = specs[[i]],
                                   level = i)
  }

  # put all subtrees in a tree
  out <- do.call(rbind, lst)
  row.names(out) <- seq_len(nrow(out))

  out
}
