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
      req(model())
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
      req(model(), input$dates)
      model()$set_filters(input$dates, input$sites, input$types)
    }, ignoreInit = TRUE)
    
    output$tbl <- DT::renderDT({
      req(model())
      model()$filtered_data()
    }, options = list(pageLength = 10))
  })
}
