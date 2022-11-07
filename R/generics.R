#' Get Data from a Object
#'
#' @param x Object.
#'
#' @export
get_data <- function(x) UseMethod("get_data")

#' Get Data Default Method
#'
#' @inheritParams get_data
#'
#' @return Data from an object.
#' @export
get_data.default <- function(x){
  rlang::abort(sprintf("No get_data() for class %s", class(x)))
}

#' Get Formula from a Object
#'
#' @param x Object.
#'
#' @export
get_formula <- function(x) UseMethod("get_formula")

#' Get Formula Default Method
#'
#' @inheritParams get_formula
#'
#' @return Data from an object.
#' @export
get_formula.default <- function(x){
  rlang::abort(sprintf("No get_formula() for class %s", class(x)))
}

#' Get Roles from a Object
#'
#' @param x Object.
#'
#' @export
get_roles <- function(x) UseMethod("get_roles")

#' Get Roles Default Method
#'
#' @inheritParams get_roles
#'
#' @return Data from an object.
#' @export
get_roles.default <- function(x){
  rlang::abort(sprintf("No get_roles() for class %s", class(x)))
}


#' Set Roles of a Object
#'
#' @param x Object.
#' @param roles Dataframe of roles.
#'
#' @export
set_roles <- function(x, roles) UseMethod("set_roles")

#' Set Roles Default Method
#'
#' @inheritParams set_roles
#'
#' @return Data from an object.
#' @export
set_roles.default <- function(x, roles){
  rlang::abort(sprintf("No set_roles() for class %s", class(x)))
}

#' Set Info of a Object
#'
#' @param x Object.
#' @param name Name of the info item.
#' @param value Value of the info item.
#'
#' @export
set_info <- function(x, name, value) UseMethod("set_info")


#' Set Info Default Method
#'
#' @inheritParams set_info
#'
#' @return Data from an object.
#' @export
set_info.default <- function(x, name, value){
  rlang::abort(sprintf("No set_info() for class %s", class(x)))
}

#' Get Info from a Object
#'
#' @param x Object.
#' @param name Name of info item.
#'
#' @export
get_info <- function(x, name) UseMethod("get_info")

#' Get Info Default Method
#'
#' @inheritParams get_info
#'
#' @return Data from an object.
#' @export
get_info.default <- function(x, name){
  rlang::abort(sprintf("No get_info() for class %s", class(x)))
}

#' Set Bag of a Object
#'
#' @param x Object.
#' @param name Name of bag item.
#' @param value Value of bag item.
#'
#' @export
set_bag <- function(x, name, value) UseMethod("set_bag")

#' Set Bag Default Method
#'
#' @inheritParams set_bag
#'
#' @return Data from an object.
#' @export
set_bag.default <- function(x, name, value){
  rlang::abort(sprintf("No set_bag() for class %s", class(x)))
}

#' Get Bag from a Object
#'
#' @param x Object.
#' @param name Name of bag item.
#'
#' @export
get_bag <- function(x, name) UseMethod("get_bag")

#' Get Bag Default Method
#'
#' @inheritParams get_bag
#'
#' @return Data from an object.
#' @export
get_bag.default <- function(x, name){
  rlang::abort(sprintf("No get_bag() for class %s", class(x)))
}
