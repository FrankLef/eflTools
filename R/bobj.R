#' Create New Business Object
#'
#' @param data Dataframe.
#' @param formula Formula of business model.
#' @param id Character vector of key variables.
#'
#' @return Object of class \emph{bobj}.
#' @export
new_bobj <- function(data, formula, id) {
  checkmate::assert_data_frame(data)
  checkmate::assert_formula(formula)
  checkmate::assert_character(id)

  roles <- init_roles(data, formula, id)

  structure(
    list(
      "data" = data,
      "formula" = formula,
      "id" = id,
      "roles" = roles,
      "info" = list(),
      "bag" = list()),
    class = "bobj")
}

#' Initialize Roles of a Business Object
#'
#' @inheritParams new_bobj
#'
#' @return Dataframe of roles.
#' @export
init_roles <- function(data, formula, id) {
  out_vars <- formula.tools::lhs.vars(formula)
  pred_vars <- formula.tools::rhs.vars(formula)
  vars <- c(out_vars, pred_vars, id)
  checkmate::assert_names(vars, subset.of = names(data))

  # get the types
  types <- sapply(X = vars, FUN = function(x) {
    y <- data[, x, drop = TRUE]
    if(is.character(y) | is.factor(y)) {
      out <- "nominal"
    } else if (is.numeric(y)) {
      out <- "numeric"
    } else {
      msg <- sprintf("The type of %s could not be found", x)
      stop(msg)}
    out
  })
  out <- data.frame(
    "variable" = vars,
    "type" = types,
  row.names = NULL)

  # set the roles
  roles <- c(rep("outcome", times = length(out_vars)),
                 rep("predictor", times = length(pred_vars)),
                 rep("id", times = length(id)))

  out <- out |>
    mutate(role = roles,
           info = NA_character_)

  out
}

#' Validate Business Object
#'
#' @param obj Object of class \emph{bobj}.
#'
#' @return Object of class \emph{bobj}.
#' @export
validate_bobj <- function(obj) {
  checkmate::check_class(obj, "bobj")
  checkmate::check_names(x = c(all.vars(obj$formula), obj$id),
               subset.of = names(obj$data))
}

#' Create a Business Object
#'
#' @inheritParams new_bobj
#'
#' @return Object of class \emph{bobj}.
#' @export
bobj <- function(data, formula, id) {
  me <- new_bobj(data = data, formula = formula, id = id)
  validate_bobj(me)
  me
}

#' Print a Business Object
#'
#' @param x Object of class \emph{bobj}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Formatted message to console.
#' @export
print.bobj <- function(x, ...) {
  msg <- paste("data: %s", "formula: %s variables", "id: %s",
               "info: %s", "bag: %s",
               sep = "\n")
  msg <- sprintf(msg,
                 toString(dim(x$data)),
                 length(all.vars(x$formula)),
                 x$id,
                 toString(names(x$info)),
                 toString(names(x$bag)))
  msg_style <- cli::combine_ansi_styles(cli::style_bold, cli::col_yellow)
  cat(msg_style(msg))
}

#' Get Data from a Business Object
#'
#' @param x Object of class \emph{bobj}.
#'
#' @return Data from an object of class \emph{bobj}.
#' @export
get_data.bobj <- function(x) {
  x$data
}

#' Get Formula from a Business Object
#'
#' @param x Object of class \emph{bobj}.
#'
#' @return Formula from an object of class \emph{bobj}.
#' @export
get_formula.bobj <- function(x) {
  x$formula
}

#' Get Roles from a Business Object
#'
#' @param x Object of class \emph{bobj}.
#'
#' @return Roles from an object of class \emph{bobj}.
#' @export
get_roles.bobj <- function(x) {
  x$roles
}

#' Set Roles from a Business Object
#'
#' @param x Object of class \emph{bobj}.
#' @param roles New values to assign to the roles.
#'
#' @return Object of class \emph{bobj}.
#' @export
set_roles.bobj <- function(x, roles) {
  x$roles <- roles
  x
}

#' Get Info Item from a Business Object
#'
#' @inheritParams set_info.bobj
#'
#' @return Info from an object of class \emph{bobj}.
#' @export
get_info.bobj <- function(x, name) {
  checkmate::assert_names(name, subset.of = names(x$info))
  x$info[[name]]}

#' Set Info Item from a Business Object
#'
#' @param x Object of class \emph{bobj}.
#' @param name Name of the info item.
#' @param value New value to assign to the info.
#'
#' @return Object of class \emph{bobj}.
#' @export
set_info.bobj <- function(x, name, value) {
  is_exist <- name %in% names(x$info)

  if (is_exist) {
    x$info[[name]] <- value
  } else {
    x$info <- c(x$info, NA)
    names(x$info)[length(x$info)] <- name
    x$info[[length(x$info)]] <- value
  }
  x
}


#' Get Bag Item from a Business Object
#'
#' @inheritParams set_bag.bobj
#'
#' @return Bag from an object of class \emph{bobj}.
#' @export
get_bag.bobj <- function(x, name) {
  checkmate::assert_names(name, subset.of = names(x$bag))
  x$bag[[name]]
}

#' Set Bag Item from a Business Object
#'
#' @param x Object of class \emph{bobj}.
#' @param name Name of the bag item.
#' @param value New value to assign to the bag.
#'
#' @return Object of class \emph{bobj}.
#' @export
set_bag.bobj <- function(x, name, value) {
  is_exist <- name %in% names(x$bag)

  if (is_exist) {
    x$bag[[name]] <- value
  } else {
    x$bag <- c(x$bag, NA)
    names(x$bag)[length(x$bag)] <- name
    x$bag[[length(x$bag)]] <- value
  }
  x
}
