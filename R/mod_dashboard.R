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
                               shiny::uiOutput(ns("kpi3")),
                               shiny::uiOutput(ns("kpi4"))
                    ),
                    shiny::tags$p(id = ns("ts_desc"), class = "sr-only",
                                  "Time series of daily consumption for the selected filters."),
                    shiny::div(
                      role = "img",
                      `aria-describedby` = ns("ts_desc"),
                      plotly::plotlyOutput(ns("plot_ts"))
                    ),
                    shiny::tags$p(id = ns("cmp_desc"), class = "sr-only",
                                  "Bar chart comparing total consumption by site or type for the selected filters."),
                    shiny::div(
                      role = "img",
                      `aria-describedby` = ns("cmp_desc"),
                      plotly::plotlyOutput(ns("plot_cmp"))
                    ),
                    shiny::div(
                      style = "display:flex; justify-content:flex-end; margin: 6px 0;",
                      shiny::downloadButton(ns("download_csv"), "Download filtered CSV")
                    ),
                    DT::DTOutput(ns("tbl"))
      )
    )
  )
}

# Server
mod_dashboard_server <- function(id, dm, is_dark = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    get_dm <- if (is.function(dm)) dm else function() dm
    model  <- shiny::reactive(get_dm())
    
    # reactive brand palette that flips in dark mode
    brand <- shiny::reactive({
      if (is_dark()) {
        list(
          primary = "#5FD4C9",               # line/bars
          bg      = "rgba(0,0,0,0)",         # <-- transparent so it matches the app surface
          fg      = "#e6edf3",               # axis/title/hover
          grid    = "#2a2f3a"                # grids
        )
      } else {
        list(
          primary = "#15B3A8",
          bg      = "rgba(0,0,0,0)",         # <-- transparent in light too
          fg      = "#0f172a",
          grid    = "#e9ecef"
        )
      }
    })
    
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
      
      DT::datatable(
        df, 
        rownames = FALSE, 
        options = list(pageLength = 20, columnDefs = columnDefs),
        extensions = "KeyTable",
        caption = shiny::tags$caption(
          style = "caption-side: top; text-align:left;",
          "Summary table for current filters."
        )
        )
    })
    
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("susneo_filtered_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
      },
      content = function(path) {
        model()$set_filters(input$dates, input$sites, input$types)
        df <- model()$summary_table()  # currently == filtered_data()
        readr::write_csv(df, path, na = "")
      }
    )
    
    fmt_num <- function(x) formatC(x, big.mark = ",", digits = 0, format = "f")
    
    output$kpi1 <- shiny::renderUI({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        class = "kpi-card",
        shiny::strong("Total consumption"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_total_consumption()))
      )
    })
    
    output$kpi2 <- shiny::renderUI({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        class = "kpi-card",
        shiny::strong("Total emissions (kgCO2e)"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_total_emissions()))
      )
    })
    
    output$kpi3 <- shiny::renderUI({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        class = "kpi-card",
        shiny::strong("Avg daily consumption"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_avg_daily_consumption()))
      )
    })
    
    output$kpi4 <- shiny::renderUI({
      shiny::req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
      shiny::div(
        class = "kpi-card",
        shiny::strong("Energy intensity (per site-day)"), shiny::br(),
        shiny::span(style="font-size:1.4rem;", fmt_num(model()$kpi_energy_intensity()))
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
      
      pal <- brand()
      
      p <- plotly::plot_ly(
        ts, x = ~period, y = ~value,
        type = "scatter", mode = "lines",
        line = list(color = pal$primary, width = 3),
        hovertemplate = "%{x}<br>Consumption: %{y}<extra></extra>"
      )
      plotly::layout(
        p,
        title = paste0("Time series (", by, ")"),
        font  = list(color = pal$fg),
        xaxis = list(title = "", color = pal$fg,
                     gridcolor = pal$grid, zerolinecolor = pal$grid, tickfont = list(color = pal$fg)),
        yaxis = list(title = "Consumption", color = pal$fg,
                     gridcolor = pal$grid, zerolinecolor = pal$grid, tickfont = list(color = pal$fg)),
        paper_bgcolor = pal$bg, plot_bgcolor = pal$bg
      )
    })
    
    # Compare by type/site (horizontal bars)
    output$plot_cmp <- plotly::renderPlotly({
      shiny::req(model(), input$dates, input$compare_by)
      model()$set_filters(input$dates, input$sites, input$types)
      
      by  <- if (identical(input$compare_by, "site")) "site" else "type"
      cmp <- model()$compare(by = by)
      shiny::validate(shiny::need(nrow(cmp) > 0, "No data for selected filters."))
      
      cmp <- cmp[order(cmp$value, decreasing = TRUE), ]
      cmp[[by]] <- factor(cmp[[by]], levels = rev(cmp[[by]]))
      
      pal <- brand()
      
      p <- plotly::plot_ly(
        cmp, x = ~value, y = cmp[[by]],
        type = "bar", orientation = "h",
        marker = list(color = pal$primary)
      )
      plotly::layout(
        p,
        title = paste("By", by),
        font  = list(color = pal$fg),
        xaxis = list(title = "Consumption", color = pal$fg,
                     gridcolor = pal$grid, zerolinecolor = pal$grid, tickfont = list(color = pal$fg)),
        yaxis = list(title = "", color = pal$fg,
                     categoryorder = "array", categoryarray = levels(cmp[[by]]),
                     tickfont = list(color = pal$fg)),
        paper_bgcolor = pal$bg, plot_bgcolor = pal$bg
      )
    })
    
  })
}
