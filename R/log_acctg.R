#' Log Using Accounting Rule
#'
#' Log using accounting rule.
#'
#' Compute the log for number whose absolute value is greater than of equal to
#' the base and change the sign of the result to be the same as the input. If
#' the absolute value of the number is less than the base, then divide it
#' by the base.
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
#' stopifnot(identical(y, c(-2, -1, -1/10, -0.1/10, 0, 0.1/10, 1/10, 1, 2)))
log_acctg <- function(x, base = 10L) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(base, lower = 2, finite = TRUE)

  y <- abs(x)
  ifelse(y >= base, sign(x) * log(y, base = base), x / base)
}

#' Exponentiation Using Accounting Rule
#'
#' Exponentiation using accounting rule.
#'
#' Exponentiate the number whose absolute value is greater than of equal to
#' the base and change the sign of the result to be the sae as the input. If
#' the absolute value of the number is less than the base, then multiply it
#' by the base. This function is the inverse of \code{log_acctg}.
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
#' stopifnot(identical(y, c(-100, -10, -1, -0.1, 0, 0.1, 1, 10, 100)))
exp_acctg <- function(x, base = 10L) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(base, lower = 2, finite = TRUE)

  y <- abs(x)
  ifelse(y >= 1L, sign(x) * base ^ y, x * base)
}
