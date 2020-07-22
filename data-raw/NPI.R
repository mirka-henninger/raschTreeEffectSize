## code to prepare `NPI` dataset goes here

NPI <- readr::read_csv("data-raw/NPI.csv")
usethis::use_data(NPI, overwrite = TRUE)
