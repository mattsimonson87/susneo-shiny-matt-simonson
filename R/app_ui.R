#' Application UI
app_ui <- function() {
  # brand-ish palette from the site screenshot (soft teal)
  theme <- bslib::bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#15B3A8",  # teal
    secondary  = "#0F8C86",  # darker teal
    info       = "#3AC7B9",
    success    = "#1BBE9C",
    base_font  = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Poppins")
  )
  # light gradient navbar + soft KPI cards
  theme <- bslib::bs_add_rules(theme, "
    .navbar { 
      background: linear-gradient(90deg, #0EA5A7 0%, #5FD4C9 100%) !important;
    }
    .kpi-card {
      padding: 10px; border-radius: 12px; 
      background: #f8faf9; 
      box-shadow: 0 1px 3px rgba(0,0,0,.06);
      margin-bottom: 8px;
    }
    .btn, .form-select, .form-control { border-radius: 10px; }
  ")
  
  bslib::page_fillable(
    bslib::page_navbar(
      title = "SUSNEO \u2014 Energy & Emissions",
      theme = theme,
      bslib::nav_panel("Dashboard",
                       bslib::layout_sidebar(
                         sidebar = bslib::sidebar(
                           shiny::h4("Filters & Upload"),
                           mod_data_upload_ui("upload")
                         ),
                         mod_dashboard_ui("dash")
                       )
      )
    )
  )
}