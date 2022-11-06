get_data <- function(x) UseMethod("get_data")
get_data.default <- function(x){
  rlang::abort(sprintf("No get_data() for class %s", class(x)))
}

get_formula <- function(x) UseMethod("get_formula")
get_formula.default <- function(x){
  rlang::abort(sprintf("No get_formula() for class %s", class(x)))
}

get_roles <- function(x) UseMethod("get_roles")
get_roles.default <- function(x){
  rlang::abort(sprintf("No get_roles() for class %s", class(x)))
}

set_roles <- function(x, roles) UseMethod("set_roles")
set_roles.default <- function(x){
  rlang::abort(sprintf("No set_roles() for class %s", class(x)))
}

set_info <- function(x, name, value) UseMethod("set_info")
set_bag.default <- function(x, name, value){
  rlang::abort(sprintf("No set_info() for class %s", class(x)))
}

get_info <- function(x, name) UseMethod("get_info")
get_info.default <- function(x, name){
  rlang::abort(sprintf("No get_info() for class %s", class(x)))
}

set_bag <- function(x, name, value) UseMethod("set_bag")
set_bag.default <- function(x, name, value){
  rlang::abort(sprintf("No set_bag() for class %s", class(x)))
}

get_bag <- function(x, name) UseMethod("get_bag")
get_bag.default <- function(x, name){
  rlang::abort(sprintf("No get_bag() for class %s", class(x)))
}
