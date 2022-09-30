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
fv_p_chart = function(dataset = fv)
{
  dataset |>
    dplyr::rename(n = `TOTAL A&B`, N = `TOTAL SPECIMENS`) |>
    shewhart.hybrid::P_Chart() |>
    shewhart.hybrid::plot_run_chart()
}
