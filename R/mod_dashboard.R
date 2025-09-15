# UI
mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("filters"))),
      shiny::column(8,
                    shiny::div(class = "kpis",
                               shiny::uiOutput(ns("kpi1")),
                               shiny::uiOutput(ns("kpi2")),
                               shiny::uiOutput(ns("kpi3"))
                    ),
                    shiny::plotOutput(ns("plot_ts")),
                    shiny::plotOutput(ns("plot_cmp")),
                    DT::DTOutput(ns("tbl"))
      )
    )
  )
}

# Server
mod_dashboard_server <- function(id, dm) {
  shiny::moduleServer(id, function(input, output, session) {
    model <- shiny::reactive(dm())
    
    output$filters <- shiny::renderUI({
      shiny::req(model())
      st <- model()$status(); ns <- session$ns
      shiny::tagList(
        shiny::dateRangeInput(ns("dates"), "Date range",
                              start = st$date_min, end = st$date_max,
                              min = st$date_min, max = st$date_max
        ),
        shiny::selectizeInput(ns("sites"), "Sites",
                              choices = st$sites, selected = st$sites, multiple = TRUE
        ),
        shiny::selectizeInput(ns("types"), "Types",
                              choices = st$types, selected = st$types, multiple = TRUE
        )
      )
    })
    
    shiny::observeEvent(list(input$dates, input$sites, input$types), {
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
    }, ignoreInit = TRUE)
    
    output$tbl <- DT::renderDT({
      shiny::req(model())
      df <- model()$filtered_data()
      
      # Nice column order
      preferred <- c("id","site","date","type","value","carbon_emission_kgco2e")
      df <- df[, c(intersect(preferred, names(df)), setdiff(names(df), preferred)), drop = FALSE]
      
      # Add a hidden numeric helper for ID if all non-missing IDs are digits
      columnDefs <- list()
      if ("id" %in% names(df)) {
        valid <- !is.na(df$id) & nzchar(df$id)
        all_digits <- length(valid) == 0 || all(grepl("^\\d+$", df$id[valid]))
        if (all_digits) {
          df$id_num <- suppressWarnings(as.numeric(df$id))
          id_idx    <- which(names(df) == "id") - 1L       # display col (0-based)
          idnum_idx <- which(names(df) == "id_num") - 1L   # hidden helper
          
          columnDefs <- list(
            list(visible = FALSE, targets = idnum_idx),        # hide helper
            list(orderData = idnum_idx, targets = id_idx)       # sort 'id' by helper
          )
        }
      }
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          columnDefs = columnDefs
        )
      )
    })
  })
}
