
#' Run R scripts in sequence
#'
#' Run R scripts in sequence
#'
#' Run R scripts named after a pattern in a specified directory.
#'
#' @param dir String. Name of the directory where the R scripts are located.
#' @param pattern Regex pattern. Pattern to filter the source files to run.
#'
#' @return Number of source files run.
#' @export
#'
#' @examples
#' \dontrun{
#' run_eda(dir = file.path(getwd(), "src"), pattern = "^eda.*[.]R$")
#' }
run_eda <- function(dir = file.path(getwd(), "src"), pattern = "^eda.*[.]R$") {

    # verify input
    checkmate::assertDirectoryExists(x = dir)

    # get the files in the dir, only file with R extension
    the_files <- list.files(dir, pattern)

    # return zero exit if no files
    if(length(the_files) == 0) {
      run_eda_msg(0L)
      return(0L)
      }

    # make sure files are in ascending order
    the_files <- sort(the_files, decreasing = FALSE)

    # inform user that the eda starts
    message(sprintf("Running %d scripts:", length(the_files)))

    # run the script, one by one
    files_nb <- 0
    for (a_file in the_files) {
      # run the script
      message(sprintf(" Running eda script: %s", a_file))
      # the encoding is important!!!
      source(file.path(dir, a_file), encoding = "UTF-8")

      # increment the file nb
      files_nb <- files_nb + 1

    }

    # inform user of success
    run_eda_msg(files_nb)

    files_nb
}


#' Console message when run_eda() is completed successfully
#'
#' Console message when run_eda() is completed successfully
#'
#' This is used by run_eda() and not exported.
#'
#' @param nb number of scripts successfully completed
#'
#' @return output to console
#' @importFrom beepr beep
#' @export
#'
#' @examples
#' run_eda_msg(1)
run_eda_msg <- function(nb) {

  checkmate::assertCount(nb)

  if (nb != 0) {
    w <- "scripts"
    w <- if(nb == 1) w <- "script"
    msg <- sprintf("\nSuccessfully completed %d %s\n", nb, w)
    cat(crayon::black$bgGreen$bold(msg))
    # message(crayon::black$bgGreen$bold(msg))
  } else {
    msg <- "\nNo script to process\n"
    cat(crayon::black$bgYellow$bold(msg))
  }

  beepr::beep(1)

}
