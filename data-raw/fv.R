library(vroom)

fv = vroom("inst/extdata/WHO_NREVSS_Clinical_Labs FluView 2017-18 for SPC.csv") |>
  mutate(date = glue::glue("{YEAR}-01-01") |> lubridate::as_date() + (WEEK-1)*7)


usethis::use_data(fv, overwrite = TRUE)
