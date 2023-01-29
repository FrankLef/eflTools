#' Signed \code{log1p} With a Base
#'
#' Signed \code{log1p} with a base.
#'
#' Compute \code{log1ps(x, base) = sign(x) * log1p(abs(x)) / log(base)}.
#' This function is the inverse of \code{expm1s}.
#'
#' @param x Numerical vector.
#' @param base Positive number >= 2. Default value is exp(1).
#'
#' @return Numeric vector.
#' @export
#'
#' @seealso expm1s
#'
#' @examples
#' x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)
#' y <- log1ps(x, base = 10)
#' # this test if the inverse is ok
#' stopifnot(isTRUE(all.equal(x, eflTools::expm1s(y, base = 10))))
log1ps <- function(x, base = exp(1)) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(base, lower = 2, finite = TRUE)

  sign(x) * log1p(abs(x)) / log(base)
}

#' Signed \code{expm1} With a Base
#'
#' Signed \code{expm1} With a base.
#'
#' Compute \code{expm1s(x, base) = sign(x) * (base ^ abs(x) - 1)}.
#' This function is the inverse of \code{log1ps}.
#'
#' @inheritParams log1ps
#'
#' @return Numeric vector.
#' @export
#'
#' @seealso log1ps
#'
#' @examples
#' x <- c(-2, -1, -1/10, -0.1/10, 0, 0.1/10, 1/10, 1, 2)
#' y <- expm1s(x, base = 10)
#' # this test if the inverse is ok
#' stopifnot(isTRUE(all.equal(x, eflTools::log1ps(y, base = 10))))
expm1s <- function(x, base = exp(1)) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(base, lower = 2, finite = TRUE)

  sign(x) * (base ^ abs(x) - 1)
}
