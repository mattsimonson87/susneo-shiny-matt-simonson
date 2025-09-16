#' Application server
#' @noRd
app_server <- function(input, output, session) {
  # Load data/upload module
  dm <- mod_data_upload_server("upload")
  
  # Dark flag for downstream (e.g., plotly)
  is_dark <- shiny::reactive(isTRUE(input$dark_mode))
  
  # Dashboard
  mod_dashboard_server("dash", dm = dm, is_dark = is_dark)
}