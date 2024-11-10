

path_dropbox <- "dropbox_notes.csv"

main_color = "deeppurple"
main_color_hex = "#673ab7"


filter <- \() cli::cli_abort("Add dplyr:: prefix")


.dstate <- new.env(parent = emptyenv())



drop_auth_RT <- function(
    new_user = FALSE,
    key = "mmhfsybffdom42w",
    secret = "l8zeqqqgm1ne5z0",
    cache = TRUE,
    rdstoken = NA
) {

  if (new_user == FALSE && !is.na(rdstoken)) {
    if (file.exists(rdstoken)) {
      .dstate$token <- readRDS(rdstoken)
    }
    else {
      cli::cli_abort("token file not found")
    }
  }
  else {
    if (new_user && file.exists(".httr-oauth")) {
      cli::cli_alert_info("Removing old dropbox credentials...")
      file.remove(".httr-oauth")
    }
    dropbox <- httr::oauth_endpoint(
      authorize = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
      access = "https://api.dropbox.com/oauth2/token"
    )
    # added "?token_access_type=offline" to the "authorize" parameter so that it can return an access token as well as a refresh token
    dropbox_app <- httr::oauth_app("dropbox", key, secret)
    dropbox_token <- httr::oauth2.0_token(dropbox, dropbox_app, cache = cache)

    if (!inherits(dropbox_token, "Token2.0")) {
      cli::cli_abort("something went wrong, try again")
    }
    .dstate$token <- dropbox_token
  }
}

# Once
# drop_auth_RT(rdstoken = "droptoken.rds", new_user = TRUE)
# saveRDS(.dstate$token, "droptoken.rds")

rlang::try_fetch(
  {
    .GlobalEnv$token <- drop_auth_RT(rdstoken = "droptoken.rds")
    rdrop2::drop_auth(rdstoken = "droptoken.rds")
  },
  error = \(cnd) {
    cli::cli_alert_info("Skip dropbox")
  }
)


