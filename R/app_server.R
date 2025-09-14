#' Application server
app_server <- function(input, output, session) {
  # Upload module should return a DataModel (we'll wire later)
  dm <- mod_data_upload_server("upload")
  mod_dashboard_server("dash", dm = dm)
}