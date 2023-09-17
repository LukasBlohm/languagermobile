

#' Submit on enter
#'
#' @param button
#'
#' @noRd
submit_on_enter <- function(btn_id) {
  paste0("
  $(document).on('keypress', function(e) {
    if(e.which == 13) {
      $('#", btn_id, "').click();
    }
  });")
}
