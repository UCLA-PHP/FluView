load_all()

presaved_CDC_data =
  cdcfluview::who_nrevss(region = "state") |>
  rbind_labs(lab_name = c("clinical_labs", "combined_prior_to_2015_16"))

attr(presaved_CDC_data, "date") = Sys.time()

if(any(duplicated(presaved_CDC_data |> select(region, wk_date))))
  stop("Duplicate records found")

use_data(presaved_CDC_data, overwrite = TRUE)

