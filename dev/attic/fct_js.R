

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

bind_keys_to_buttons <- function(keys, btn_ids, ns = identity) {
  js_code <- paste0(
    "$(document).on('keypress', function(e) {",
    paste(sapply(1:length(keys), function(i) {
      sprintf("if(e.which == %d) { $('#%s').click(); } ", keys[i], ns(btn_ids[i]))
    }), collapse = " else "),
    "});"
  )
  tags$script(HTML(js_code))
}




# Enter: 13
# Spacebar: 32
# Escape: 27
# Up Arrow: 38
# Down Arrow: 40
# Left Arrow: 37
# Right Arrow: 39
# Delete: 46
# Backspace: 8
# Tab: 9
# Shift: 16
# Control: 17
# Alt: 18
# Page Up: 33
# Page Down: 34
# End: 35
# Home: 36
# Insert: 45
# A - Z: 65 - 90 (e.g., 65 for "A", 66 for "B", etc.)
# 0 - 9: 48 - 57 (top row numbers)
# F1 - F12: 112 - 123






