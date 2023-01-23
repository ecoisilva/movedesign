## code to prepare `fixrates` dataset goes here

library(ctmm)
library(dplyr)

fixrates <- data.frame(dti_notes = character(0),
                       dti = numeric(0),
                       common = character(0),
                       dti_scale = character(0),
                       dti_yn = character(0))

fixrates <- fixrates %>%
  dplyr::add_row(dti_notes = "1 fix every month", 
                 dti = 1 %#% "month",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every two weeks", 
                 dti = 2 %#% "week",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every week", 
                 dti = 1 %#% "week",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 4 days", 
                 dti = 4 %#% "days",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 2 days", 
                 dti = 2 %#% "days",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every day",   
                 dti = 1 %#% "days",
                 common = "Y", dti_scale = "1 day", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 12 hours", 
                 dti = 12 %#% "hours",
                 common = "Y", dti_scale = "12 hrs", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 8 hours",  
                 dti = 8 %#% "hours",
                 common = "Y", dti_scale = "8 hrs", dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 6 hours",  
                 dti = 6 %#% "hours",
                 common = "Y", dti_scale = "6 hrs", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 4 hours",  
                 dti = 4 %#% "hours",
                 common = "Y", dti_scale = "4 hrs", dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 3 hours",  
                 dti = 3 %#% "hours",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 2 hours",  
                 dti = 2 %#% "hours",
                 common = "N", dti_scale = "2 hrs", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every hour",  
                 dti = 1 %#% "hours",
                 common = "N", dti_scale = "1 hr", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 30 minutes", 
                 dti = 30 %#% "minutes",
                 common = "N", dti_scale = "30 mins", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 20 minutes", 
                 dti = 20 %#% "minutes",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 15 minutes", 
                 dti = 15 %#% "minutes",
                 common = "N", dti_scale = "15 mins", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 10 minutes", 
                 dti = 10 %#% "minutes",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 5 minutes", 
                 dti = 5 %#% "minutes",
                 common = "N", dti_scale = "5 mins", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 2 minutes", 
                 dti = 2 %#% "minutes",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every minute", 
                 dti = 1 %#% "minutes",
                 common = "N", dti_scale = "1 min", dti_yn = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 30 seconds", 
                 dti = 30 %#% "seconds",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 20 seconds", 
                 dti = 20 %#% "seconds",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 15 seconds", 
                 dti = 20 %#% "seconds",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 10 seconds", 
                 dti = 10 %#% "seconds",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 5 seconds", 
                 dti = 5 %#% "seconds",
                 common = "N", dti_scale = NA, dti_yn = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every second", 
                 dti = 1 %#% "seconds",
                 common = "N", dti_scale = "1 sec", dti_yn = "Y")

fixrates$frq <- 1/fixrates$dti # sampling frequency
fixrates$frq_hrs <- 1/("hour" %#% fixrates$dti)

# View(format(fixrates, scientific = FALSE))

usethis::use_data(fixrates, overwrite = TRUE)

