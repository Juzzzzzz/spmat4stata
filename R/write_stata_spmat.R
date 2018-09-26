#' Write apatial weight matrix to a `.txt` file which can be read by Stata15
#'
#' @param long2mat_list output of `long2mat`
#' @param file_name just path and file name
#' @param save_check_list save ID check list as both `save_check_list = TRUE` and `id_is_int = FALSE`
#' @examples
#' \dontrun{
#' write_stata_spmat(long2mat_list = queen_list, file_name = "World_spmat", save_check_list = TRUE)
#' }
#' @export
#' @importFrom stringr str_c str_glue
#' @importFrom readr write_csv
#' @importFrom utils write.table
#'


write_stata_spmat <- function(long2mat_list, file_name, save_check_list = FALSE) {
  m_df <- long2mat_list$m_df
  n <- nrow(m_df)
  m <- data.frame(apply(m_df, 1, function(x) str_c(x, collapse = ' ')))
  write.table(m, str_glue("{file_name}.txt"), quote = FALSE, row.names = FALSE)
  if (!long2mat_list$id_is_int & save_check_list) {
    write_csv(long2mat_list$check_list, str_glue("{file_name}_ID_check_list.csv"))
  }
}
