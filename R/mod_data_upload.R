# UI
mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Source toggle
    shiny::radioButtons(
      ns("source"), label = "Data source",
      choices = c("Use bundled sample", "Upload CSV"),
      selected = "Use bundled sample", inline = TRUE
    ),
    
    # File selector shown only when Upload CSV is chosen
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'Upload CSV'", ns("source")),
      shiny::fileInput(
        ns("upload_csv"), "Choose CSV",
        accept = ".csv",
        buttonLabel = "Browse…",
        placeholder = "No file selected"
      ),
      shiny::helpText("Required columns: id, site, date, type, value, carbon_emission_kgco2e")
    ),
    
    # Reset and status
    shiny::actionButton(ns("reset"), "Reset to sample"),
    shiny::hr(),
    shiny::uiOutput(ns("status"))
  )
}

# Server
mod_data_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    dm <- DataModel$new()
    
    output$status <- shiny::renderUI({
      st <- dm$status()
      shiny::HTML(sprintf(
        "Rows: <b>%s</b> | Sites: <b>%s</b> | Dates: <b>%s</b> \u2192 <b>%s</b> | Sources: <b>%s</b>",
        st$n_rows, st$n_sites, st$date_min, st$date_max, st$sources_count
      ))
    })
    
    shiny::reactive(dm)
  })
}