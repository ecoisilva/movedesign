## code to prepare `fixrates` dataset goes here

library(ctmm)
library(dplyr)

fixrates <- data.frame(dti_notes = character(0),
                       dti = numeric(0),
                       highlighted = character(0),
                       for_vhf = character(0))

fixrates <- fixrates %>%
  dplyr::add_row(dti_notes = "1 fix every month", 
                 dti = 1 %#% "month",
                 highlighted = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every two weeks", 
                 dti = 2 %#% "week",
                 highlighted = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every week", 
                 dti = 1 %#% "week",
                 highlighted = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 4 days", 
                 dti = 4 %#% "days",
                 highlighted = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every 2 days", 
                 dti = 2 %#% "days",
                 highlighted = "N") %>%
  dplyr::add_row(dti_notes = "1 fix every day",   
                 dti = 1 %#% "days",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 12 hours", 
                 dti = 12 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 8 hours",  
                 dti = 8 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 6 hours",  
                 dti = 6 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 4 hours",  
                 dti = 4 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 3 hours",  
                 dti = 3 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 2 hours",  
                 dti = 2 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every hour",  
                 dti = 1 %#% "hours",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 30 minutes", 
                 dti = 30 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 20 minutes", 
                 dti = 20 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 15 minutes", 
                 dti = 15 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 10 minutes", 
                 dti = 10 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 5 minutes", 
                 dti = 5 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 2 minutes", 
                 dti = 2 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every minute", 
                 dti = 1 %#% "minutes",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 30 seconds", 
                 dti = 30 %#% "seconds",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 20 seconds", 
                 dti = 20 %#% "seconds",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 15 seconds", 
                 dti = 20 %#% "seconds",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 10 seconds", 
                 dti = 10 %#% "seconds",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every 5 seconds", 
                 dti = 5 %#% "seconds",
                 highlighted = "Y") %>%
  dplyr::add_row(dti_notes = "1 fix every second", 
                 dti = 1 %#% "seconds",
                 highlighted = "Y")

fixrates$frq <- 1/fixrates$dti # sampling frequency
fixrates$frq_hrs <- 1/("hour" %#% fixrates$dti)

# View(format(fixrates, scientific = FALSE))

usethis::use_data(fixrates, overwrite = TRUE)
