#' Submit on enter
#'
#' @param button
#'
#' @noRd
submit_on_enter <- function(btn_id) {
  paste0("
  $(document).on('keypress', function(e) {
    if(e.which == 17) {
      $('#", btn_id, "').click();
    }
  });")
}


#' Load data
#'
#' @param file_name String specifying the file name
#'
#' @importFrom magrittr %>%
#'
#' @noRd
load_datasets <- function(file_name) {
  suppressMessages(readr::read_csv(paste0("data_inputs/", file_name))) %>%
    assign(x = paste0("df_", stringr::str_remove(file_name, ".csv")),
           envir = .GlobalEnv)
}

# load_datasets <- purrr::as_mapper(
#   ~ readr::read_csv(paste0("data_inputs/", .x)) %>%
#     assign(x = paste0("df_", str_remove(.x, ".csv")),
#            envir = .GlobalEnv)
#   )





