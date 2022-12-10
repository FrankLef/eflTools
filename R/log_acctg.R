#' Signed \code{log1p} With a Base
#'
#' Signed \code{log1p} with a base.
#'
#' Compute \code{log_acctg(x, base) = sign(x) * log1p(abs(x)) / log(base)}.
#' This function is the inverse of \code{exp_acctg}.
#'
#' @param x Numerical vector.
#' @param base Positive number >= 2. Default value is 10.
#'
#' @return Numeric vector.
#' @export
#'
#' @seealso exp_acctg
#'
#' @examples
#' x <- c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)
#' y <- log_acctg(x)
#' # this test if the inverse is ok
#' stopifnot(isTRUE(all.equal(x, eflTools::exp_acctg(y))))
log_acctg <- function(x, base = 10L) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(base, lower = 2, finite = TRUE)

  sign(x) * log1p(abs(x)) / log(base)
}

#' Signed Exponentiation With a Base
#'
#' Signed exponentiation With a base.
#'
#' Compute \code{exp_acctg(x, base) = sign(x) * (base ^ abs(x) - 1)}.
#' This function is the inverse of \code{log_acctg}.
#'
#' @inheritParams log_acctg
#'
#' @return Numeric vector.
#' @export
#'
#' @seealso log_acctg
#'
#' @examples
#' x <- c(-2, -1, -1/10, -0.1/10, 0, 0.1/10, 1/10, 1, 2)
#' y <- exp_acctg(x)
#' # this test if the inverse is ok
#' stopifnot(isTRUE(all.equal(x, eflTools::log_acctg(y))))
exp_acctg <- function(x, base = 10L) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(base, lower = 2, finite = TRUE)

  sign(x) * (base ^ abs(x) - 1)
}
