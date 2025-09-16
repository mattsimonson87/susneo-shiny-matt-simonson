#' Application UI
app_ui <- function() {
  # Base theme (light). We'll flip to dark in server via update, but
  # these CSS rules below handle the contrast and component colors.
  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary   = "#15B3A8",
    secondary = "#0F8C86",
    info      = "#3AC7B9",
    success   = "#1BBE9C",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Poppins")
  )
  
  # Component styles that adapt to Bootstrap CSS variables.
  theme <- bslib::bs_add_rules(theme, "
    /* Gradient navbar stays; we control text colors below */
    .navbar {
      background: linear-gradient(90deg, #0EA5A7 0%, #5FD4C9 100%) !important;
    }
    /* Give navbar content breathing room so the toggle isn't glued to edge */
    .navbar .container-fluid { padding-inline: 16px; }

    /* KPI card uses token so it adapts in dark */
    .kpi-card {
      padding: 10px; border-radius: 12px;
      background: var(--bs-card-bg);
      box-shadow: 0 1px 3px rgba(0,0,0,.06);
      margin-bottom: 8px;
    }
    .btn, .form-select, .form-control { border-radius: 10px; }

    /* --- Accessibility helpers --- */
    .skip-link { position:absolute; left:-9999px; }
    .skip-link:focus {
      left: 8px; top: 8px; z-index: 10000; background: #fff;
      padding: .5rem .75rem; border-radius: .5rem; border: 2px solid #0d6efd;
    }
    :focus { outline: 2px solid #0d6efd; outline-offset: 2px; }
    .sr-only { position:absolute!important; width:1px; height:1px; padding:0; margin:-1px;
      overflow:hidden; clip:rect(0,0,0,0); white-space:nowrap; border:0;
    }

    /* --- Inputs & menus follow body tokens (dark-friendly) --- */
    .form-control, .form-select, .selectize-input {
      background-color: var(--bs-body-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: color-mix(in srgb, var(--bs-body-color) 25%, transparent);
    }
    .selectize-dropdown, .dropdown-menu {
      background-color: var(--bs-body-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: color-mix(in srgb, var(--bs-body-color) 25%, transparent) !important;
    }

    /* DataTable readability + keyboard focus ring */
    .dataTable td.focus, table.dataTable tbody td.focus {
      outline: 2px solid var(--bs-primary) !important; outline-offset: -2px;
    }
    .table.dataTable, .table.dataTable thead th,
    .dataTables_info, .dataTables_paginate {
      background-color: var(--bs-body-bg) !important;
      color: var(--bs-body-color) !important;
    }

    /* --- Navbar text colors: light vs dark --------------------- */
    /* Light: brand teal; links dark for contrast on the teal gradient */
    [data-bs-theme='light'] .navbar .navbar-brand { color: #0F8C86 !important; }
    [data-bs-theme='light'] .navbar .nav-link     { color: #0e2f2e !important; }
    [data-bs-theme='light'] .navbar .nav-link.active {
      color: #0F8C86 !important; border-bottom: 2px solid currentColor;
    }

    /* Dark: brand light teal; links nearly white */
    [data-bs-theme='dark'] .navbar { background: linear-gradient(90deg, #0b5b5a 0%, #1e7f78 100%) !important; }
    [data-bs-theme='dark'] .navbar .navbar-brand { color: #5FD4C9 !important; }
    [data-bs-theme='dark'] .navbar .nav-link     { color: #e6edf3 !important; }
    [data-bs-theme='dark'] .navbar .nav-link.active {
      color: #ffffff !important; border-bottom: 2px solid #5FD4C9;
    }
  ")
  
  shiny::tagList(
    shiny::tags$head(),  # keep a <head> for the CSS above
    
    bslib::page_fillable(
      bslib::page_navbar(
        # Skip link first in tab order
        title = shiny::tagList(
          shiny::tags$a(href = "#main", class = "skip-link", tabindex = "0", "Skip to content"),
          "SUSNEO – Energy & Emissions"
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
            # Landmarked main region (target for skip link)
            shiny::tags$main(id = "main", role = "main", tabindex = "-1",
                             mod_dashboard_ui("dash")
            )
          )
        ),
        
        # Right-aligned dark-mode toggle with nicer spacing
        bslib::nav_spacer(),
        bslib::nav_item(
          shiny::div(class = "ms-auto me-3 d-flex align-items-center gap-2",
                     shiny::span(class = "navbar-text", "Dark mode"),
                     shiny::checkboxInput("dark_mode", label = NULL, value = FALSE, width = "auto")
          )
        )
      )
    )
  )
}
