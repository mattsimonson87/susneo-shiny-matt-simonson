#' Launch the SUSNEO app
#'
#' Starts the Shiny application. Additional arguments are passed to
#' [golem::with_golem_options()].
#'
#' @param ... Named options passed through to [golem::with_golem_options()].
#' @return A [shiny::shinyApp] object (invisibly).
#' @export
#' @examples
#' if (interactive()) susneoShinyMatt::run_app()
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui(), server = app_server),
    golem_opts = list(), ...
  )
}
