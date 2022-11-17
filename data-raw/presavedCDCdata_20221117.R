## code to prepare `presavedCDCdata_20221117` dataset goes here
presavedCDCdata_20221117 = cdcfluview::who_nrevss("state") |> combine_labs(lab_name = c("clinical_labs", "combined_prior_to_2015_16"))

usethis::use_data(presavedCDCdata_20221117, overwrite = TRUE)
