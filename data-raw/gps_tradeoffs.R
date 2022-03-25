## code to prepare `gps_tradeoffs` dataset goes here

library(dplyr)
library(ctmm)

gps_tradeoffs <- read.csv(here::here("inst",
                                     "extdata",
                                     "gps_tradeoffs.csv"))
gps_tradeoffs <- gps_tradeoffs %>%
  dplyr::mutate(
    dur_mnths = duration/4,
    dur = (duration/4) %#% 'month', # in secs
    dt = ((1/frequency)*4) %#% 'hr') # in secs

usethis::use_data(gps_tradeoffs, overwrite = TRUE)
