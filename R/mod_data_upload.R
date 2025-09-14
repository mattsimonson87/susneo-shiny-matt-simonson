# UI
mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("source"), "Data source",
                        choices = c("Use bundled sample", "Upload CSV"),
                        selected = "Use bundled sample"
    ),
    shiny::fileInput(ns("file"), "Upload CSV", accept = ".csv"),
    shiny::actionButton(ns("reset"), "Reset to sample"),
    shiny::uiOutput(ns("status")) 
  )
}

# Server
mod_data_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    dm <- DataModel$new()  
    
    output$status <- shiny::renderUI({
      st <- dm$status()
      shiny::HTML(
        sprintf("Rows: <b>%s</b> | Sites: <b>%s</b> | Dates: <b>%s</b> → <b>%s</b> | Sources: <b>%s</b>",
                st$n_rows, st$n_sites, st$date_min, st$date_max, st$sources_count)
      )
    })
    
    shiny::reactive(dm)
  })
}