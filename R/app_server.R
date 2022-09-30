#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom plotly renderPlotly
#' @importFrom lubridate %within% int_diff
#' @importFrom cdcfluview who_nrevss
#' @importFrom dplyr filter
#' @importFrom plotly renderPlotly
app_server <- function(input, output, session) {
  # Your application server logic

  years =
    seq(
      from = input$dates[1] |> year(),
      to = input$dates[2] |> year()
    ) |>
    reactive()

  dataset0 =
    cdcfluview::who_nrevss("state", years = years())$clinical_labs |>
    reactive()

  dataset =
    dataset0() |>
    dplyr::filter(
      region %in% input$states,
      wk_date %within% (input$dates |> int_diff())) |>
    dplyr::group_by(wk_date) |>
    dplyr::summarize(
      .groups = "drop",
      total_specimens = total_specimens |> as.numeric() |> sum(),
      `TOTAL A&B` = sum(total_a |> as.numeric() + total_b |> as.numeric())
    ) |>
    reactive()

  plot1 = eventReactive(
    input$goButton,
    {
      validate(need(nrow(dataset()) > 0, "No data found for these filter settings."))
      dataset() |> fv_p_chart()
    }
  )

  output$graph1 = plotly::renderPlotly(plot1())
}
