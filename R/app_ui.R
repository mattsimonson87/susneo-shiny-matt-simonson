#' Application UI
app_ui <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#15B3A8",
    secondary = "#0F8C86",
    info = "#3AC7B9",
    success = "#1BBE9C",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Poppins")
  )
  
  theme <- bslib::bs_add_rules(theme, "
    .navbar { background: linear-gradient(90deg, #0EA5A7 0%, #5FD4C9 100%) !important; }
    .kpi-card { padding: 10px; border-radius: 12px; background: #f8faf9;
                box-shadow: 0 1px 3px rgba(0,0,0,.06); margin-bottom: 8px; }
    .btn, .form-select, .form-control { border-radius: 10px; }
  ")
  
  shiny::tagList(
    # --- a11y: styles for the skip link + focus rings ---
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .skip-link { position:absolute; left:-9999px; }
        .skip-link:focus { left: 8px; top: 8px; z-index: 10000; background: #fff;
                           padding: .5rem .75rem; border-radius: .5rem; border: 2px solid #0d6efd; }
        :focus { outline: 2px solid #0d6efd; outline-offset: 2px; }
        .sr-only { position:absolute!important; width:1px; height:1px; padding:0; margin:-1px; 
        overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap; border:0;
        }
        .dataTable td.focus,
        table.dataTable tbody td.focus {
          outline: 2px solid #0d6efd !important;
          outline-offset: -2px;
            }
      "))
    ),
    
    bslib::page_fillable(
      bslib::page_navbar(
        # Put the skip link first in the title so it’s first in tab order
        title = shiny::tagList(
          shiny::tags$a(href = "#main", class = "skip-link", tabindex = "0", "Skip to content"),
          "SUSNEO - Energy & Emissions"
        ),
        theme = theme,
        bslib::nav_panel(
          "Dashboard",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              width = 360, open = "desktop",
              shiny::h4("Filters & Upload"),
              mod_data_upload_ui("upload")
            ),
            # Real <main> landmark = skip target
            shiny::tags$main(id = "main", role = "main", tabindex = "-1",
                             mod_dashboard_ui("dash")
            )
          )
        )
      )
    )
  )
}
