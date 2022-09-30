#' Title
#'
#' @param dataset some subset of `fv`
#'
#' @return a plotly graph
#' @export
#'
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
fv_p_chart = function(dataset = fv)
{
  dataset |>
    rename(n = `Total A&B`, N = `TOTAL SPECIMENS`) |>
    shewhart.hybrid::P_Chart() |>
    shewhart.hybrid::plot_run_chart()
}
