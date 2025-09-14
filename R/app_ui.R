#' Application UI
app_ui <- function() {
  bslib::page_fillable(
    bslib::page_navbar(
      title = "SUSNEO — Energy & Emissions",
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
