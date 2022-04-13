#' Import acctg tags from an excel file
#'
#' Import acctg tags from an excel file.
#'
#' Import acctg tags from an excel file in a long format. Usually to be exported
#' to MS Access.
#'
#' @param file Path and file name of the excel file.
#' @param sheet Name of the excel sheet.
#' @param cols Character vector with the names of the columns not pivoted.
#'
#' @importFrom readxl read_xlsx
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#'
#' @return Dataframe.
#' @export
gather_acctg_tags <- function(file, sheet = "tbl_imp_tags",
                           cols = c("concept", "entity", "fsstatus", "curr_no")) {
  checkmate::assert_file_exists(file)
  checkmate::assert_character(file)

  df <- readxl::read_xlsx(file, sheet = sheet)

  tidyr::pivot_longer(df, cols = !tidyselect::matches(cols))
}
