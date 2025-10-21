#' Application UI
app_ui <- function() {
  # Single base theme (Flatly). We do NOT swap themes on toggle; CSS handles dark.
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
  
  # Custom rules keyed off [data-bs-theme] + Bootstrap variables
  theme <- bslib::bs_add_rules(theme, "
    /* ---------- Dark-mode variable overrides (Bootstrap 5.3) ---------- */
    [data-bs-theme='dark'] {
      --bs-body-bg: #0e1117;
      --bs-body-color: #e6edf3;
      --bs-border-color: #2a2a2a;
      --bs-secondary-bg: #1a1a1a;
      --bs-tertiary-bg: #111111;

      --bs-card-bg: #1a1a1a;
      --bs-card-border-color: #2a2a2a;

      --bs-table-bg: #1a1a1a;
      --bs-table-border-color: #2a2a2a;

      --bs-form-control-bg: #000000;
      --bs-form-control-color: #e6edf3;
      --bs-form-control-border-color: #3a3a3a;

      --bs-form-select-bg: #000000;
      --bs-form-select-color: #e6edf3;
      --bs-form-select-border-color: #3a3a3a;

      --bs-dropdown-bg: #000000;
      --bs-dropdown-link-color: #e6edf3;
    }

    /* Base */
    body { background-color: #fafafa; transition: background-color 0.3s ease; }
    [data-bs-theme='dark'] body { background-color: var(--bs-body-bg); }

    /* Main surface areas */
    [data-bs-theme='dark'] .bslib-page-fillable,
    [data-bs-theme='dark'] .main-panel,
    [data-bs-theme='dark'] .tab-content,
    [data-bs-theme='dark'] .tab-pane {
      background: var(--bs-tertiary-bg) !important;
    }

    /* Sidebar */
    [data-bs-theme='dark'] .bslib-sidebar-layout > .sidebar,
    [data-bs-theme='dark'] .bslib-sidebar-layout .sidebar {
      background: var(--bs-secondary-bg) !important;
      border-right: 1px solid var(--bs-border-color) !important;
    }

    /* Navbar gradient (light base) */
    .navbar { background: linear-gradient(90deg, #0EA5A7 0%, #5FD4C9 100%) !important; }
    .navbar .container-fluid { padding-inline: 16px; }

    /* KPI cards */
    .kpi-card {
      padding: 10px; border-radius: 12px; background: var(--bs-card-bg);
      box-shadow: 0 1px 3px rgba(0,0,0,.06); margin-bottom: 8px;
    }
    [data-bs-theme='dark'] .kpi-card { background: #1a1a1a !important; border: 1px solid #2a2a2a; }

    /* Buttons and inputs */
    .btn, .form-select, .form-control { border-radius: 10px; }

    /* Inputs + selectize */
    [data-bs-theme='dark'] .form-control, 
    [data-bs-theme='dark'] .form-select, 
    [data-bs-theme='dark'] .selectize-input {
      background-color: #000000 !important; color: #e6edf3 !important; border-color: #3a3a3a !important;
    }
    [data-bs-theme='light'] .form-control, 
    [data-bs-theme='light'] .form-select, 
    [data-bs-theme='light'] .selectize-input {
      background-color: #ffffff !important; color: #0f172a !important; border-color: #dee2e6 !important;
    }
    [data-bs-theme='dark'] .selectize-dropdown {
      background-color: #000000 !important; border-color: #3a3a3a !important;
    }

    /* Data tables */
    [data-bs-theme='dark'] .table.dataTable { background-color: #1a1a1a !important; }

    /* Navbar text colors - force white in light only so dark rules can apply */
    [data-bs-theme='light'] .navbar * { color: #ffffff !important; }
    [data-bs-theme='light'] .navbar .nav-link.active { border-bottom: 3px solid #ffffff !important; font-weight: 600 !important; }

    /* Dark-mode navbar look */
    [data-bs-theme='dark'] .navbar { background: linear-gradient(90deg, #0b5b5a 0%, #1e7f78 100%) !important; }
    [data-bs-theme='dark'] .navbar .navbar-brand { color: #5FD4C9 !important; }
    [data-bs-theme='dark'] .navbar .nav-link,
    [data-bs-theme='dark'] .navbar-nav .nav-link,
    [data-bs-theme='dark'] .navbar .navbar-text { color: #e6edf3 !important; }
    [data-bs-theme='dark'] .navbar .nav-link.active { color: #ffffff !important; border-bottom: 3px solid #5FD4C9 !important; font-weight: 600 !important; }

    /* Dark mode toggle styling */
    .dark-mode-navbar-toggle { display: flex; align-items: center; gap: 8px; margin-left: auto; }
    .form-switch .form-check-input {
      width: 44px; height: 24px; background-color: rgba(255,255,255,0.3);
      border-color: rgba(255,255,255,0.5); cursor: pointer;
    }
    .form-switch .form-check-input:checked { background-color: #5FD4C9; border-color: #5FD4C9; }
    [data-bs-theme='dark'] .form-switch .form-check-input:not(:checked) { background-color: #3a3a3a; border-color: #3a3a3a; }
    
        /* Chat styling */
    .chat-messages::-webkit-scrollbar {
      width: 8px;
    }
    .chat-messages::-webkit-scrollbar-track {
      background: var(--bs-secondary-bg);
      border-radius: 10px;
    }
    .chat-messages::-webkit-scrollbar-thumb {
      background: var(--bs-border-color);
      border-radius: 10px;
    }
    [data-bs-theme='dark'] .chat-input-area {
      background: var(--bs-secondary-bg) !important;
      border: 1px solid var(--bs-border-color);
    }
  ")
  
  shiny::tagList(
    shiny::tags$head(
      # One self-contained script in <head>: set initial theme, wire the toggle, notify Shiny
      shiny::tags$script(shiny::HTML(
        "(function(){
          var saved = localStorage.getItem('susneo_theme') || 'light';
          function applyTheme(mode){
            document.documentElement.setAttribute('data-bs-theme', mode);
            if (document.body) document.body.setAttribute('data-bs-theme', mode);
          }
          applyTheme(saved);

          function wire(){
            var t = document.getElementById('dark_mode');
            if(!t) return;
            t.checked = (saved === 'dark');

            if(window.Shiny && Shiny.setInputValue){
              Shiny.setInputValue('dark_mode', saved === 'dark', {priority:'event'});
            }

            t.addEventListener('change', function(){
              var mode = this.checked ? 'dark' : 'light';
              localStorage.setItem('susneo_theme', mode);
              applyTheme(mode);
              if(window.Shiny && Shiny.setInputValue){
                Shiny.setInputValue('dark_mode', mode === 'dark', {priority:'event'});
              }
            });
          }
          if(document.readyState === 'loading'){ document.addEventListener('DOMContentLoaded', wire); } else { wire(); }
        })();"
      ))
      
    ),
    
    bslib::page_fillable(
      bslib::page_navbar(
        title = "SUSNEO - Energy & Emissions",
        theme = theme,
        
        bslib::nav_panel(
          "Dashboard",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              width = 360, 
              open  = "desktop",
              shiny::h4("Filters & Upload"),
              mod_data_upload_ui("upload")
            ),
            shiny::tags$main(
              id = "main", role = "main", class = "main-panel",
              mod_dashboard_ui("dash")
            )
          )
        ),
        
        # Chat Bot
        bslib::nav_panel(
          "Chat Assistant",
          shiny::div(
            class = "container-fluid",
            style = "padding: 20px;",
            shiny::h3("Energy Data Assistant"),
            shiny::p("Ask questions about your energy consumption and emissions data."),
            mod_chatbot_ui("chat")
          )
        ),
        
        # Dark mode control in the navbar
        bslib::nav_spacer(),
        bslib::nav_item(
          shiny::div(
            class = "dark-mode-navbar-toggle",
            shiny::span(class = "navbar-text", "Dark mode"),
            shiny::div(
              class = "form-check form-switch",
              shiny::tags$input(
                type  = "checkbox",
                class = "form-check-input",
                id    = "dark_mode",
                role  = "switch"
              )
            )
          )
        )
      )
    )
  )
}
