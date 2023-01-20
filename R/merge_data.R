#' Title
#'
#' @param dataset
#' @param lab_types
#' @param regions
#' @param dates
#' @param variants
#' @param verbose
#'
#' @return
#' @export
#'
merge_data = function(
    dataset,
    lab_types,
    regions,
    dates,
    variants,
    verbose = FALSE)
{
  if(verbose) message("before merging data")

  merged_data =
    dataset |>
    dplyr::filter(
      labType %in% lab_types,
      complete.cases(total_specimens),
      region %in% regions,
      wk_date %within% (dates |> lubridate::int_diff())) |>
    dplyr::group_by(wk_date) |>
    dplyr::summarize(
      .groups = "drop",
      total_specimens =
        total_specimens |>
        as.numeric() |>
        sum(na.rm = TRUE),
      `TOTAL POSITIVE` =
        variants |>
        stringr::str_replace_all(
          c("a" = "total_a", "b" = "total_b")) |>
        dplyr::across() |>
        sum(na.rm = TRUE))

  message("after merging data")

  return(merged_data)

}
