#' Application server
#' @noRd
app_server <- function(input, output, session) {
  # load the data/upload module 
  dm <- mod_data_upload_server("upload")
  
  # define two themes 
  light_theme <- bslib::bs_theme(
    version = 5, bootswatch = "flatly",
    primary = "#15B3A8", secondary = "#0F8C86", info = "#3AC7B9", success = "#1BBE9C",
    base_font = bslib::font_google("Inter"), heading_font = bslib::font_google("Poppins")
  )
  dark_theme <- bslib::bs_theme(
    version = 5, bootswatch = "darkly",
    primary = "#5FD4C9", secondary = "#15B3A8",
    base_font = bslib::font_google("Inter"), heading_font = bslib::font_google("Poppins"),
    bg = "#0e1117", fg = "#e6edf3"
  )
  
  # toggle theme based on the navbar checkbox
  observe({
    if (isTRUE(input$dark_mode)) {
      session$setCurrentTheme(dark_theme)
    } else {
      session$setCurrentTheme(light_theme)
    }
  })
  
  # simple reactive flag 
  is_dark <- reactive(isTRUE(input$dark_mode))
  
  # call dashboard, passing both the model and dark-mode flag
  mod_dashboard_server("dash", dm = dm, is_dark = is_dark)
}