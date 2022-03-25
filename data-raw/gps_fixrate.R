## code to prepare `gps_fixrate` dataset goes here

library(ctmm)
library(dplyr)

gps_fixrate <- data.frame(x = character(0),
                          y = numeric(0),
                          highlight = character(0),
                          choices = character(0))

gps_fixrate <- gps_fixrate %>%
  dplyr::add_row(x = "1 fix every month",   y = 1 %#% "months",
                 highlight = "Y", choices = "N")

gps_fixrate <- gps_fixrate %>%
  dplyr::add_row(x = "1 fix every 29 days", y = 29 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 28 days", y = 28 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 27 days", y = 27 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 26 days", y = 26 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 25 days", y = 25 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 24 days", y = 24 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 23 days", y = 23 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 22 days", y = 22 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 21 days", y = 21 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 20 days", y = 20 %#% "days",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 18 days", y = 18 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 16 days", y = 16 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 14 days", y = 14 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 12 days", y = 12 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 10 days", y = 10 %#% "days",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 7 days",  y = 7 %#% "days",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 6 days",  y = 6 %#% "days",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 4 days",  y = 4 %#% "days",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 2 days",  y = 2 %#% "days",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every day",     y = 1 %#% "days",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 12 hours",  y = 12 %#% "hours",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 8 hours",   y = 8 %#% "hours",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 6 hours",   y = 6 %#% "hours",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 5 hours",   y = 5 %#% "hours",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 4 hours",   y = 4 %#% "hours",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 3 hours",   y = 3 %#% "hours",
                 highlight = "N", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 2 hours",   y = 2 %#% "hours",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every hour",    y = 1 %#% "hours",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 45 minutes", y = 45 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 30 minutes", y = 30 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 25 minutes", y = 25 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 20 minutes", y = 20 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 15 minutes", y = 15 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 10 minutes", y = 10 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 5 minutes",  y = 5 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 4 minutes",  y = 4 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 3 minutes",  y = 3 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 2 minutes",  y = 2 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every minute",  y = 1 %#% "minutes",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 50 seconds", y = 50 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 40 seconds", y = 40 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 30 seconds", y = 30 %#% "seconds",
                 highlight = "Y", choices = "Y") %>%
  dplyr::add_row(x = "1 fix every 20 seconds", y = 20 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 10 seconds", y = 10 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 8 seconds",  y = 8 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 6 seconds",  y = 6 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 4 seconds",  y = 4 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every 2 seconds",  y = 2 %#% "seconds",
                 highlight = "Y", choices = "N") %>%
  dplyr::add_row(x = "1 fix every second",  y = 1 %#% "seconds",
                 highlight = "Y", choices = "N")

colnames(gps_fixrate) <- c("nu_notes", "nu", "highlight", "choices")
gps_fixrate$nu_hrs <- "hour" %#% gps_fixrate$nu
gps_fixrate$freq <- 1/gps_fixrate$nu # sampling frequency
gps_fixrate$freq_hrs <- 1/gps_fixrate$nu_hrs
# View(format(gps_fixrate, scientific = FALSE))

usethis::use_data(gps_fixrate, overwrite = TRUE)
