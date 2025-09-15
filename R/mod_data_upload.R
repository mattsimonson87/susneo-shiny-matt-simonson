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
        ns("upload_csv"), "Choose CSV or Excel",
        accept = c(".csv", ".xlsx"),
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      shiny::helpText("Required columns (CSV or XLSX): id, site, date, type, value, carbon_emission_kgco2e")
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
    
    # Helper Function for reading from xlsx or csv
    read_upload_as_char <- function(datapath, name) {
      if (is.null(name) || !nzchar(name)) name <- basename(datapath)
      ext <- tolower(tools::file_ext(name))
      if (ext == "csv") {
        readr::read_csv(
          datapath,
          col_types = readr::cols(.default = readr::col_character()),
          show_col_types = FALSE, progress = FALSE
        )
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(datapath, sheet = 1, col_types = "text"))
      } else {
        stop("Unsupported file type: .", ext)
      }
    }
    
    # Hold the model as a reactiveVal
    rv_model <- shiny::reactiveVal(DataModel$new())
    
    # A small bump to refresh the status UI
    status_bump <- shiny::reactiveVal(0)
    
    # Status panel
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
    
    # Reset: return to bundled sample
    shiny::observeEvent(input$reset, {
      rv_model(DataModel$new())
      status_bump(status_bump() + 1)
      shiny::showNotification("Reset to bundled sample.", type = "message", duration = 3)
    })
    
    # Switching radio back to "Use bundled sample" also resets
    shiny::observeEvent(input$source, {
      if (identical(input$source, "Use bundled sample")) {
        rv_model(DataModel$new())
        status_bump(status_bump() + 1)
      }
    }, ignoreInit = TRUE)
    
    # Upload -> canonicalize -> validate -> merge
    shiny::observeEvent(input$upload_csv, {
      shiny::req(input$upload_csv)
      
      # read as character to handle mixed formats
      df <- tryCatch(
        read_upload_as_char(input$upload_csv$datapath, input$upload_csv$name),
        error = function(e) {
          shiny::showNotification(paste("Failed to read file:", e$message), type = "error", duration = NULL)
          return(NULL)
        }
      )
      shiny::req(df)
      
      dm <- rv_model()
      df_c <- dm$canonicalize(df)
      v    <- dm$validate(df_c)
      
      if (isTRUE(v)) {
        src   <- if (!is.null(input$upload_csv$name)) paste0("upload:", input$upload_csv$name) else "upload"
        stats <- dm$merge(df_c, source_tag = src)
        
        # IMPORTANT: replace the object to invalidate downstream reactives
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
    
    # Return the reactive model for the dashboard module
    rv_model
  })
}