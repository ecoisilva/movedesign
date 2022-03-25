## code to prepare `df_sims` dataset goes here

df_sims <- read.csv(here::here("inst",
                               "extdata",
                               "df_sims.csv"))

usethis::use_data(df_sims, overwrite = TRUE)
