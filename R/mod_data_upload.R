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
    shiny::div(id = ns("status"), shiny::em("Status: sample loaded"))
  )
}

# Server
mod_data_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # TODO: instantiate DataModel; load sample on start
    # TODO: on file upload -> canonicalize, validate, merge
    # TODO: on reset -> dm$reset()
    shiny::reactiveVal(NULL)  # placeholder so app compiles
  })
}
