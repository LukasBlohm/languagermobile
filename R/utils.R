#' Submit on enter
#'
#' @param btn_id
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



#' Generate_df_ui_labels
#'
#' Looks for data frames in .GlobalEnv (which start with df_) and creates
#' labels for the UI accordingly
#'
#' @importFrom magrittr %>%
#'
#' @return A character vector of the labels
#'
#' @noRd
generate_df_ui_labels <- function() {
  stringr::str_remove(
    names(.GlobalEnv)[stringr::str_starts(
      names(.GlobalEnv), pattern = stringr::fixed("df_")
  )], "df_")
}


#' Get languages
#'
#' @param input
#'
#' @importFrom magrittr %>%
#'
#' @return A character vector of names of columns that correspond to a language
#'
#' @noRd
get_languages <- function(input) {

  df <- .GlobalEnv$l_colnames[[paste0("df_", input$dataset)]]

  df[stringr::str_detect(df, "^FR|^EN|^DE")]
}











