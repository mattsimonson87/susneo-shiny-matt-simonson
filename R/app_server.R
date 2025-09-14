#' Application server
#' @noRd
app_server <- function(input, output, session) {
  dm <- mod_data_upload_server("upload")
  mod_dashboard_server("dash", dm = dm)
}