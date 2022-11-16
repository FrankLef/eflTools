
#' Create New Export Object of class \code{xprts}
#'
#'
#' @return Object of class \emph{xprts}.
#' @export
new_xprts <- function() {
  structure(
    list("bag" = list()),
    class = "xprts"
  )
}

#' Validate Export Object of class \code{xprts}
#'
#' @param obj Object of class \emph{xprts}.
#'
#' @return Object of class \emph{xprts}.
#' @export
validate_xprts <- function(obj) {
  checkmate::check_class(obj, "xprts")
}


#' Create an Export Object of class \code{xprts}
#'
#'
#' @return Object of class \emph{xprts}.
#' @export
xprts <- function() {
  me <- new_xprts()
  validate_xprts(me)
  me
}

#' Print an Export Object of class \code{xprts}
#'
#' @param x Object of class \emph{xprts}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Formatted message to console.
#' @export
print.xprts <- function(x, ...) {
  the_classes <- xprt_classes.xprts(x)
  msg <- sprintf("Export details for %d items.", nrow(the_classes))
  msg_style <- cli::combine_ansi_styles(cli::style_bold, cli::col_yellow)
  cat(msg_style(msg), "\n",
      paste(rep("-", nchar(msg)), collapse = ""), "\n")
  print(the_classes)
}


#' Get Bag Item from an Export Object
#'
#' @inheritParams set_bag.xprts
#'
#' @return Bag from an object of class \emph{xprts}.
#' @export
get_bag.xprts <- function(x, name) {
  checkmate::assert_names(name, subset.of = names(x$bag))
  x$bag[[name]]
}

#' Set Bag Item from an Export Object
#'
#' @param x Object of class \emph{xprts}.
#' @param name Name of the bag item.
#' @param value New value to assign to the bag.
#'
#' @return Object of class \emph{xprts}.
#' @export
set_bag.xprts <- function(x, name, value) {
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

#' Get the Names and Classes from an Export Object
#'
#' @param obj Object of class \emph{xprts}.
#'
#' @return Datafame with names and classes.
#' @export
xprt_classes.xprts <- function(obj) {
  checkmate::assert_list(obj$bag)

  if (length(obj$bag)) {
    the_classes <- sapply(obj$bag, function(x) {
      out <- NA_character_
      if (inherits(x, what = "ggplot")) {
        out <- "ggplot"
      } else if (inherits(x, what = "plotly")) {
        out <- "plotly"
      } else if (inherits(x, what = "gt_tbl")) {
        out <- "gt"
      }
      out
      })
    df <- data.frame(
      name = names(obj$bag),
      class = the_classes,
      row.names = NULL) |>
      dplyr::arrange(.data$name)

  } else {
    df <- data.frame(
      name = character(),
      class = character(),
      row.names = NULL)
  }
  df
}

#' Save GGPlot to PNG
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#' @param width Width of plot.
#' @param height Height of plot.
#' @param units UOM used for width and height.
#'
#' @importFrom ggplot2 ggsave
#'
#' @return \code{obj} invisibly.
#' @export
xprt_ggplot.xprts <- function(obj, path = getwd(),
                        width = height * 16/9, height = 20, units = "cm") {
  checkmate::assert_directory_exists(path)

  msg_style <- cli::combine_ansi_styles(cli::col_blue)

  df <- xprt_classes(obj) |>
    filter(class == "ggplot")
  n <- nrow(df)

  if (n) {
    for (x in df$name) {
      msg <- sprintf("Exporting %s", x)
      cat(msg_style(msg), "\n")
      a_file <- file.path(path, paste(x, "png", sep = "."))
      ggplot2::ggsave(filename = a_file, plot = obj$bag[[x]],
                      width = width, height = height, units = units)
    }
    msg <- sprintf("%d ggplots exported.", n)
  } else {
    msg <- "No ggplot available for export. Skip it."
  }

  cat(msg_style(msg), "\n")
  invisible(obj)
}

#' Save Plotly to QS
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#'
#' @importFrom qs qsave
#'
#' @return \code{obj} invisibly.
#' @export
xprt_plotly.xprts <- function(obj, path = getwd()) {
  checkmate::assert_directory_exists(path)

  msg_style <- cli::combine_ansi_styles(cli::col_blue)

  df <- xprt_classes(obj) |>
    filter(class == "plotly")
  n <- nrow(df)

  if (n) {
    for (x in df$name) {
      msg <- sprintf("Exporting %s", x)
      cat(msg_style(msg), "\n")
      a_file <- file.path(path, paste(x, "qs", sep = "."))
      qs::qsave(x = obj$bag[[x]], file = a_file)
    }
    msg <- sprintf("%d plotlys exported.", n)
  } else {
    msg <- "No plotly available for export. Skip it."
  }

  cat(msg_style(msg), "\n")
  invisible(obj)
}


#' Save GT to HTML
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path used for file.
#' @param inline_css See \code{gt::gtsave}. Must absolutely be set to TRUE
#'  to include the gt table in a markdown document. Don't touch it unless you
#'  know what you are doing.
#'
#' @importFrom gt gtsave
#'
#' @return \code{obj} invisibly.
#' @export
xprt_gt.xprts <- function(obj, path = getwd(), inline_css = TRUE) {
  checkmate::assert_directory_exists(path)

  msg_style <- cli::combine_ansi_styles(cli::col_blue)

  df <- xprt_classes(obj) |>
    filter(class == "gt")
  n <- nrow(df)

  if (n) {
    for (x in df$name) {
      msg <- sprintf("Exporting %s", x)
      cat(msg_style(msg), "\n")
      a_file <- paste(x, "html", sep = ".")
      # IMPORTANT: Must have inline_css = TRUE to work properly
      #           with rmarkdown. Otherwise huge output files are created.
      gt::gtsave(data = obj$bag[[x]],
                 filename = a_file,
                 path = path,
                 inline_css = inline_css)
    }
    msg <- sprintf("%d gts exported.", n)
  } else {
    msg <- "No gt available for export. Skip it."
  }

  cat(msg_style(msg), "\n")
  invisible(obj)
}

#' Export all objects from Export Object of class \code{xprts}
#'
#' @param obj Object of class \emph{xprts}.
#' @param path Path for exported file.
#' @param is_xprt TRUE = Export objects, FALSE = Don't export (default).
#'
#' @return \code{obj} invisibly.
#' @export
xprt_all.xprts <- function(obj, path = getwd(), is_xprt = FALSE) {
  checkmate::assert_directory_exists(path)
  checkmate::assert_flag(is_xprt)

  msg_style <- cli::combine_ansi_styles(cli::col_green)

  df <- xprt_classes(obj)
  n <- nrow(df)

  if (is_xprt) {
    msg <- sprintf("Exporting %d objects\U2026", n)
    cat(msg_style(msg), "\n")

    eflTools::xprt_ggplot(obj, path = path)
    eflTools::xprt_plotly(obj, path = path)
    eflTools::xprt_gt(obj, path = path)

    msg <- sprintf("%d objects exported.", n)
  } else {
    msg <- "Not exporting objects."
  }
  cat(msg_style(msg), "\n")
  invisible(obj)
}
