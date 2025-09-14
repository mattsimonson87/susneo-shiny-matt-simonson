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
    # TODO: render filters from dm$status()
    # TODO: apply filters via dm$set_filters()
    # TODO: render KPIs/plots/table from dm$...() methods
    output$filters <- shiny::renderUI(shiny::helpText("Filters appear after data loads."))
    output$kpi1 <- shiny::renderUI(shiny::strong("KPI 1")); output$kpi2 <- shiny::renderUI("KPI 2")
    output$kpi3 <- shiny::renderUI("KPI 3")
    output$plot_ts <- shiny::renderPlot(plot.new()); output$plot_cmp <- shiny::renderPlot(plot.new())
    output$tbl <- DT::renderDT(data.frame())
  })
}
