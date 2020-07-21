## code to prepare `NPI` dataset goes here

usethis::use_data(NPI, overwrite = TRUE)
NPI <- readr::read_csv("data-raw/data.csv")
usethis::use_data(NPI)
