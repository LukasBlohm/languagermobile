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
  readr::read_csv(paste0("data/", file_name), show_col_types = FALSE) %>%
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



create_message_function <- function(type) {

  fn_name <- paste0("cli_alert", ifelse(type != "alert", paste0("_", type), ""))

  message_function <- function(msg, call = rlang::caller_env()) {
    id_prefix <- rlang::try_fetch(paste(get("id", envir = call), "-"), error = \(cnd) "")
    cli_function <- get(fn_name, envir = asNamespace("cli"))
    eval(call("cli_function", paste(id_prefix, msg), .envir = call))
  }
  assign(type, message_function, .GlobalEnv)
}


purrr::walk(c("alert", "info", "success", "warning", "danger"), create_message_function)


#' Show content of variable
#'
#' @param var Symbol
#' @param call Environment
#'
#' @return Nothing
var_info <- function(var, call = rlang::caller_env()) {
  assign("var_name", var, envir = call)
  info("{var_name}: {get(var_name)}")
  rm("var_name", envir = call)
}

# var_info("h")


#' Show variable / expression
#'
#' Print info message of the type x: x_value, where x is an expression and
#' x_value the result of its evaluation. Adds the `id` of call as a prefix if it
#' exists.
#'
#' @param x Symbol or expression
#' @param call Environment
#'
#' @return Nothing
show_content <- function(x, call = rlang::caller_env()) {
  x_expr <- rlang::enexpr(x)
  x_value <- rlang::inject(!! x_expr, env = call)
  assign("x_name", rlang::as_label(x_expr), envir = call)
  rlang::eval_tidy(rlang::expr(info("{x_name}: {x_value}")), call)
  rm("x_name", envir = call)
}

# show_content(h)
#
# t <- new.env()
# t$h <- "xyz"
# show_content(h, t)
#
# show_content(colnames(mtcars), t)
# show_content(colnames(mtcars))



