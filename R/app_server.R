#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom plotly renderPlotly
#' @importFrom lubridate %within% int_diff
app_server <- function(input, output, session) {
  # Your application server logic
  dataset =
    fv |>
    dplyr::filter(date %within% (input$dates |> int_diff())) |>
    reactive()

  output$graph1 =
    dataset() |>
    fv_p_chart() |>
    plotly::renderPlotly()
}
