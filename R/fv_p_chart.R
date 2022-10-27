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
fv_p_chart = function(
    dataset =
      cdcfluview::who_nrevss(
        "state",
        years = 2016:2022)$clinical_labs |>
      filter(region == "California") |>
      dplyr::mutate(
        `total_specimens` = `total_specimens` |> as.numeric(),
        `TOTAL A&B` = total_a |> as.numeric() + total_b |> as.numeric()) #TOTAL POSITIVE

)
{

  dataset |>
    dplyr::rename(
      n = `TOTAL A&B`, #TOTAL POSITIVE
      N = `total_specimens`,
      date = wk_date) |>
    shewhart.hybrid::PH_Chart() |>
    shewhart.hybrid::plot_run_chart()
}