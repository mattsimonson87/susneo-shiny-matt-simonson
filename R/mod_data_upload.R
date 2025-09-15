# UI
mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Source toggle
    shiny::radioButtons(
      ns("source"), label = "Data source",
      choices = c("Use bundled sample", "Upload File"),
      selected = "Use bundled sample", inline = TRUE
    ),
    
    # File selector shown only when Upload CSV is chosen
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'Upload File'", ns("source")),
      shiny::fileInput(
        ns("upload_csv"), "Upload data (CSV only)",
        accept = c(".csv", "text/csv", "text/comma-separated-values"),
        width = "100%",
        buttonLabel = "Browse...",
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
    
    # ---- model + small helpers ----
    rv_model    <- shiny::reactiveVal(DataModel$new())
    status_bump <- shiny::reactiveVal(0)
    
    # Read CSV as character so canonicalize() owns parsing (incl. dates)
    read_csv_as_char <- function(path) {
      readr::read_csv(
        file = path,
        col_types = readr::cols(.default = readr::col_character()),
        locale = readr::locale(),
        show_col_types = FALSE,
        trim_ws = TRUE
      )
    }
    
    # ---- status panel ----
    output$status <- shiny::renderUI({
      status_bump()
      st <- rv_model()$status()
      shiny::HTML(sprintf(
        "Rows: <b>%s</b> | Sites: <b>%s</b> | Dates: <b>%s</b> -> <b>%s</b> | Sources: <b>%s</b> | Uploads: <b>%s</b> | Last: <b>%s</b> at <b>%s</b>",
        st$n_rows, st$n_sites, st$date_min, st$date_max, st$sources_count, st$uploads_total,
        ifelse(is.na(st$last_source), "-", st$last_source),
        ifelse(is.na(st$last_time), "-", format(st$last_time, "%Y-%m-%d %H:%M"))
      ))
    })
    
    # ---- reset paths ----
    shiny::observeEvent(input$reset, {
      rv_model(DataModel$new()); status_bump(status_bump() + 1)
      shiny::showNotification("Reset to bundled sample.", type = "message", duration = 3)
    })
    shiny::observeEvent(input$source, {
      if (identical(input$source, "Use bundled sample")) {
        rv_model(DataModel$new()); status_bump(status_bump() + 1)
      }
    }, ignoreInit = TRUE)
    
    # ---- upload -> canonicalize -> validate -> merge (CSV only) ----
    shiny::observeEvent(input$upload_csv, {
      shiny::req(input$upload_csv$datapath, input$upload_csv$name)
      
      # Enforce CSV only (accept= in UI is just a hint; this is the hard stop)
      ext <- tolower(tools::file_ext(input$upload_csv$name))
      if (ext != "csv") {
        shiny::showNotification(
          "Please upload a CSV file (Excel: File -> Save As -> CSV).",
          type = "error", duration = 8
        )
        return(invisible(NULL))  # stop processing non-CSV uploads
      }
      
      # Read CSV as character so canonicalize() controls types (esp. dates)
      df <- tryCatch(
        read_csv_as_char(input$upload_csv$datapath),
        error = function(e) {
          shiny::showNotification(paste("Failed to read CSV:", e$message),
                                  type = "error", duration = NULL)
          return(NULL)
        }
      )
      shiny::req(df)
      
      dm   <- rv_model()
      df_c <- dm$canonicalize(df)   # reparses/normalizes dates
      v    <- dm$validate(df_c)
      
      if (isTRUE(v)) {
        src   <- paste0("upload:", input$upload_csv$name)
        stats <- dm$merge(df_c, source_tag = src)
        
        # reassign to bump reactives downstream
        rv_model(dm$clone(deep = FALSE))
        status_bump(status_bump() + 1)
        
        shiny::showNotification(
          sprintf("Merge complete: appended %d, replaced %d, new sites %d, new types %d.",
                  stats$appended, stats$replaced, stats$new_sites, stats$new_types),
          type = "message", duration = 6
        )
      } else {
        shiny::showNotification(
          paste0("Validation failed:\n- ", paste(v$errors, collapse = "\n- ")),
          type = "error", duration = NULL
        )
      }
    })
    
    # ---- expose model to other modules ----
    rv_model
  })
}