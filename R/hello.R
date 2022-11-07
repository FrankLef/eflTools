#' Example function that shows message that begins with "Hello" to console.
#'
#' @param msg String with a message that begins with "Hello". Default message
#'  is "Hello, world!".
#'
#' @importFrom cli col_yellow
#' @importFrom rlang format_error_bullets abort
#'
#' @return A message to console that starts with "Hello"
hello <- function(msg = "Hello, world!") {
  if (grepl(pattern = "^hello.*", x = msg, ignore.case = TRUE)) {
    message(msg)
  } else {
    msg_head <- cli::col_yellow("Message must start with \"hello\"")
    msg_body <- c("i" = sprintf("Message: %s", msg))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ValueError")
  }
  invisible(msg)
}
