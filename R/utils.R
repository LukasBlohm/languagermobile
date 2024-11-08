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
  readr::read_csv(paste0("data_inputs/", file_name), show_col_types = FALSE) %>%
    suppressMessages() %>%
    assign(x = paste0("df_", stringr::str_remove(file_name, ".csv")),
           envir = .GlobalEnv)
}



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

  v_names <- names(.GlobalEnv) %>%
    purrr::keep(~ stringr::str_starts(.x, stringr::fixed("df_"))) %>%
    stringr::str_remove("df_")

  v_names
}


#' Get languages
#'
#' @param input
#'
#' @return A character vector of names of columns that correspond to a language
#'
#' @noRd
get_languages <- function(input) {

  df <- .GlobalEnv$l_colnames[[paste0("df_", input$dataset)]]

  df[stringr::str_detect(df, "^FR|^EN|^DE")]
}


