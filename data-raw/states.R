library(cdcfluview)
temp = who_nrevss("state")$clinical_labs
states = temp$region |> unique() |> dput()

usethis::use_data(states, overwrite = TRUE)
