## code to prepare `sims_speed` dataset goes here

library(dplyr)
library(ctmm)

out <- read.csv(system.file("extdata", "sims_speed.csv",
                            package = "movedesign"))

out_summary <- out %>%
  Rmisc::summarySE(
    measurevar = "error",
    groupvar = c("tau_v", "dur", "dti"),
    conf.interval = 0.95,
    na.rm = TRUE
  )

fixes_per_day <- c(1, 2^seq(1, 11, by = 1))  # Number of fixes per day
dti_values <- round((1 %#% "day") / fixes_per_day, 0)

dti_notes <- data.frame(
  dti = dti_values,
  dti_notes = c(
    paste(dti_values[1], "fix every 24 hours"),
    paste(dti_values[2], "fix every 12 hours"),
    paste(dti_values[3], "fix every 6 hours"),
    paste(dti_values[4], "fix every 3 hours"),
    paste(dti_values[5], "fix every 1.5 hours"),
    paste(dti_values[6], "fix every 45 minutes"),
    paste(dti_values[7], "fix every 22.5 minutes"),
    paste(dti_values[8], "fix every 11.3 minutes"),
    paste(dti_values[9], "fix every 5.6 minutes"),
    paste(dti_values[10], "fix every 2.8 minutes"),
    paste(dti_values[11], "fix every 1.4 minutes"),
    paste(20, "fix every 20 seconds")
  )
)

dti_notes <- mutate(dti_notes, dti = as.numeric(dti))

out <- left_join(out, dti_notes, by = "dti")
out_summary <- left_join(out_summary, dti_notes, by = "dti")

sims_speed <- list(out, out_summary)
names(sims_speed) <- c("data", "summary")

usethis::use_data(sims_speed, overwrite = TRUE)
