pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

# Set port according to environment variable (from Heroku) if there is one.
port <- Sys.getenv("PORT")
if (port == "") port <- 3838
options('shiny.port' = as.numeric(port), shiny.host = '0.0.0.0')
languagermobile::run_app()
