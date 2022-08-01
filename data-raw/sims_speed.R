## code to prepare `sims_speed` dataset goes here

library(dplyr)
library(ctmm)

out <- read.csv(
  system.file("extdata", "sims_speed.csv",
              package = "movedesign"))

dat <- out %>%
  Rmisc::summarySE(measurevar = "error",
                   groupvar = c("tau_v", "dur", "dti"),
                   conf.interval = .95,
                   na.rm = TRUE)

# dat_lci <- out %>%
#   Rmisc::summarySE(measurevar = "error_lci",
#                    groupvar = c("tau_v", "dur", "dti"),
#                    conf.interval = .95,
#                    na.rm = TRUE) %>%
#   select(dur, dti, error_lci)
#
# dat_uci <- out %>%
#   Rmisc::summarySE(measurevar = "error_uci",
#                    groupvar = c("tau_v", "dur", "dti"),
#                    conf.interval = .95,
#                    na.rm = TRUE) %>%
#   select(dur, dti, error_uci)
#
# out_sum <- dplyr::left_join(dat, dat_lci) %>%
#   dplyr::left_join(dat_uci) %>%
#   select(tau_v, dur, dti, error, ci, error_lci, error_uci)

out_sum <- dat

n <- c(1, 2^seq(1, 11, by = 1)) # number of fixes per day
dti <- round((1 %#% "day") / n, 0)
txt_dti <- data.frame(
  c(dti[1], "1 fix every 24 hours"),
  c(dti[2], "1 fix every 12 hours"),
  c(dti[3], "1 fix every 6 hours"),
  c(dti[4], "1 fix every 3 hours"),
  c(dti[5], "1 fix every 1.5 hours"),
  c(dti[6], "1 fix every 45 minutes"),
  c(dti[7], "1 fix every 22.5 minutes"),
  c(dti[8], "1 fix every 11.3 minutes"),
  c(dti[9], "1 fix every 5.6 minutes"),
  c(dti[10], "1 fix every 2.8 minutes"),
  c(dti[11], "1 fix every 1.4 minutes"),
  c(dti[12], "1 fix every 42 seconds"),
  c(20, "1 fix every 20 seconds"))
txt_dti <- t(txt_dti)
rownames(txt_dti) <- 1:13
colnames(txt_dti) <- c("dti", "dti_notes")

txt_dti <- txt_dti %>%
  as.data.frame %>%
  dplyr::mutate(dti = round(as.numeric(dti), 0))

out <- left_join(out, txt_dti, by = "dti")
out_sum <- left_join(out_sum, txt_dti, by = "dti")

sims_speed <- list(out, out_sum)
names(sims_speed) <- c("data", "summary")

usethis::use_data(sims_speed, overwrite = TRUE)
