#' Title
#'
#' @param dataset some subset of `fv`
#'
#' @return a plotly graph
#' @export
#'
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
#' @importFrom dplyr rename
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
#' @importFrom dplyr rename mutate
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
test_volume_chart = function(
    dataset =
      cdcfluview::who_nrevss(
        "state",
        years = 2016:2022)$clinical_labs |>
      filter(region == "California") |>
      dplyr::mutate(
        `total_specimens` = `total_specimens` |> as.numeric())

)
{

  dataset |>
    plotly::plot_ly(
      mode = "markers+lines",
      type = "scatter",
      x = ~wk_date,
      y = ~`total_specimens`,
      hoverinfo = "text",
      hovertext = paste("Specimen Count: ", dataset$total_specimens,
                        "<br> Date: ", dataset$wk_date)
    ) |>
    plotly::layout(
      yaxis = list(title = "Specimens per day (count)")
    )

}
