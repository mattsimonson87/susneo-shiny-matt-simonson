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
                    plotly::plotlyOutput(ns("plot_ts")),
                    plotly::plotlyOutput(ns("plot_cmp")),
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
        ),
        shiny::radioButtons(ns("compare_by"), "Compare by",
                            choices = c("type", "site"), selected = "type", inline = TRUE
        )
      )
    })
    
    output$tbl <- DT::renderDT({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      df <- model()$filtered_data()
      
      preferred <- c("id","site","date","type","value","carbon_emission_kgco2e")
      df <- df[, c(intersect(preferred, names(df)), setdiff(names(df), preferred)), drop = FALSE]
      
      columnDefs <- list()
      if ("id" %in% names(df)) {
        valid <- !is.na(df$id) & nzchar(df$id)
        all_digits <- length(valid) == 0 || all(grepl("^\\d+$", df$id[valid]))
        if (all_digits) {
          df$id_num <- suppressWarnings(as.numeric(df$id))
          id_idx    <- which(names(df) == "id") - 1L
          idnum_idx <- which(names(df) == "id_num") - 1L
          columnDefs <- list(
            list(visible = FALSE, targets = idnum_idx),
            list(orderData = idnum_idx, targets = id_idx)
          )
        }
      }
      
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, columnDefs = columnDefs))
    })
    
    fmt_num <- function(x) formatC(x, big.mark = ",", digits = 0, format = "f")
    
    output$kpi1 <- shiny::renderUI({
      shiny::req(model(), input$dates)  # create reactive dep
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        style = "padding:8px; border-radius:12px; background:#f8f9fa; margin-bottom:8px;",
        shiny::strong("Total consumption"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_total_consumption()))
      )
    })
    
    output$kpi2 <- shiny::renderUI({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        style = "padding:8px; border-radius:12px; background:#f8f9fa; margin-bottom:8px;",
        shiny::strong("Total emissions (kgCO2e)"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_total_emissions()))
      )
    })
    
    output$kpi3 <- shiny::renderUI({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        style = "padding:8px; border-radius:12px; background:#f8f9fa; margin-bottom:8px;",
        shiny::strong("Avg daily consumption"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_avg_daily_consumption()))
      )
    })
    
    # Time series (auto day vs month)
    output$plot_ts <- plotly::renderPlotly({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      
      st <- model()$status()
      by <- if (is.finite(as.numeric(st$date_max - st$date_min)) &&
                (st$date_max - st$date_min) > 180) "month" else "day"
      
      ts <- model()$timeseries(by = by)
      shiny::validate(shiny::need(nrow(ts) > 0, "No data for selected filters."))
      
      p <- plotly::plot_ly(ts, x = ~period, y = ~value, type = "scatter", mode = "lines")
      plotly::layout(p, title = paste0("Time series (", by, ")"),
                     xaxis = list(title = ""), yaxis = list(title = "Consumption"))
    })
    
    # Compare by type (horizontal bars)
    output$plot_cmp <- plotly::renderPlotly({
      shiny::req(model(), input$dates, input$compare_by)
      model()$set_filters(input$dates, input$sites, input$types)
      
      by  <- if (identical(input$compare_by, "site")) "site" else "type"
      cmp <- model()$compare(by = by)
      shiny::validate(shiny::need(nrow(cmp) > 0, "No data for selected filters."))
      
      # order largest first and set factor for stable ordering
      cmp <- cmp[order(cmp$value, decreasing = TRUE), ]
      cmp[[by]] <- factor(cmp[[by]], levels = rev(cmp[[by]]))
      
      p <- plotly::plot_ly(cmp, x = ~value, y = cmp[[by]], type = "bar", orientation = "h")
      plotly::layout(p, title = paste("By", by),
                     xaxis = list(title = "Consumption"), yaxis = list(title = ""))
    })
    
  })
}
