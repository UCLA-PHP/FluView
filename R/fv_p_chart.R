#' Title
#'
#' @param dataset some subset of `fv`
#' @param ... arguments passed to shewhart.hybrid::PH_Chart(...)
#'
#' @return a plotly graph
#' @export
#'
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
#' @importFrom dplyr rename
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
#' @importFrom dplyr rename mutate
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
fv_p_chart = function(
    dataset =
      presaved_CDC_data |>
      filter(region == "California") |>
      dplyr::mutate(
        `total_specimens` = `total_specimens` |> as.numeric(),
        `TOTAL POSITIVE` = total_a |> as.numeric() + total_b |> as.numeric()),
    ...
)
{


  chart = dataset |>
    dplyr::rename(
      n = `TOTAL POSITIVE`,
      N = `total_specimens`,
      date = wk_date) |>
    shewhart.hybrid::PH_Chart(...)

  chart |>
    shewhart.hybrid::plot_run_chart(suffix = "%")
}
