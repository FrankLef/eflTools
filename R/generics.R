
# bobj --------------------------------------------------------------------


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
  msg_head <- cli::col_yellow(sprintf("No get_data() for class %s", class(x)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(x)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}

#' Set Data from a Object
#'
#' @param x Object.
#' @param data Dataframe.
#'
#' @export
set_data <- function(x, data) UseMethod("set_data")


#' Set Data Default Method
#'
#' @inheritParams set_data
#'
#' @return Data from an object.
#' @export
set_data.default <- function(x, data){
  msg_head <- cli::col_yellow(sprintf("No set_data() for class %s", class(x)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(x)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
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
  msg_head <- cli::col_yellow(sprintf("No get_formula() for class %s", class(x)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(x)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}


#' Set Formula from a Object
#'
#' @param x Object.
#' @param formula Formula.
#'
#' @export
set_formula <- function(x, formula) UseMethod("set_formula")


#' Set Formula Default Method
#'
#' @inheritParams set_formula
#'
#' @return Data from an object.
#' @export
set_formula.default <- function(x, formula){
  msg_head <- cli::col_yellow(sprintf("No set_formula() for class %s", class(x)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(x)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
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


# xprts -------------------------------------------------------------------

#' Get the Names and Classes from an Export Object
#'
#' @param obj Object of class \emph{xprts}.
#'
#' @return Datafame with names and classes.
#' @export
xprt_classes <- function(obj) UseMethod("xprt_classes")

#' Get the Names and Classes from an Export Object
#'
#' @param obj Object of class \emph{xprts}.
#'
#' @return Datafame with names and classes.
#' @export
xprt_classes.default <- function(obj){
  msg_head <- cli::col_yellow(sprintf("No get_data() for class %s", class(obj)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(obj)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}


#' Save GGPlot to PNG
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#' @param width Width of plot.
#' @param height Height of plot.
#' @param units UOM used for width and height.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_ggplot <- function(obj, path,width = height, height, units) {
  UseMethod("xprt_ggplot")
}

#' Save GGPlot to PNG
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#' @param width Width of plot.
#' @param height Height of plot.
#' @param units UOM used for width and height.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_ggplot.default <- function(obj, path,width = height, height, units){
  msg_head <- cli::col_yellow(sprintf("No get_data() for class %s", class(obj)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(obj)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}


#' Save Plotly to QS
#'
#' @param obj Object of class \emph{xprts}.
#'
#' @return \code{obj} invisibly.
#' @param path Path used for file.
#' @export
xprt_plotly <- function(obj, path) {
  UseMethod("xprt_plotly")
}

#' Save Plotly to QS
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_plotly.default <- function(obj, path){
  msg_head <- cli::col_yellow(sprintf("No get_data() for class %s", class(obj)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(obj)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}


#' Save GT to HTML
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#' @param inline_css See \code{gt::gtsave}. Must absolutely be set to TRUE
#'  to include the gt table in a markdown document. Don't touch it unless you
#'  know what you are doing.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_gt <- function(obj, path, inline_css) {
  UseMethod("xprt_gt")
}

#' Save GT to HTML
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#' @param inline_css See \code{gt::gtsave}. Must absolutely be set to TRUE
#'  to include the gt table in a markdown document. Don't touch it unless you
#'  know what you are doing.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_gt.default <- function(obj, path, inline_css){
  msg_head <- cli::col_yellow(sprintf("No get_data() for class %s", class(obj)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(obj)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}


#' Export all objects from Export Object of class \code{xprts}
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path for exported file.
#' @param is_xprt TRUE = Export objects, FALSE = Don't export.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_all <- function(obj, path, is_xprt) {
  UseMethod("xprt_all")
}

#' Export all objects from Export Object of class \code{xprts} - Default
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path for exported file.
#' @param is_xprt TRUE = Export objects, FALSE = Don't export.
#'
#' @return \code{obj} invisibly.
#' @export
xprt_all.default <- function(obj, path, is_xprt){
  msg_head <- cli::col_yellow(sprintf("No get_data() for class %s", class(obj)))
  msg_body <- c("x" = sprintf("Invalid class: %s", class(obj)))
  msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  rlang::abort(
    message = msg,
    class = "TypeError")
}
