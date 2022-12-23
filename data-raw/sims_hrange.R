## code to prepare `sims_hrange` dataset goes here

library(dplyr)
library(ctmm)

out <- read.csv(
  system.file("extdata", "sims_hrange.csv",
              package = "movedesign"))

dat <- out %>%
  dplyr::select(tau_p, duration, error) %>%
  Rmisc::summarySE(measurevar = "error",
                   groupvar = c("duration", "tau_p"),
                   conf.interval = .95,
                   na.rm = TRUE)

dat_lci <- out %>%
  select(tau_p, duration, error_lci) %>%
  Rmisc::summarySE(measurevar = "error_lci",
                   groupvar = c("duration", "tau_p"),
                   conf.interval = .95,
                   na.rm = TRUE) %>%
  select(duration, tau_p, error_lci)

dat_uci <- out %>%
  select(tau_p, duration, error_uci) %>%
  Rmisc::summarySE(measurevar = "error_uci",
                   groupvar = c("duration", "tau_p"),
                   conf.interval = .95,
                   na.rm = TRUE) %>%
  select(duration, tau_p, error_uci)

out_sum <- dplyr::left_join(dat, dat_lci) %>%
  dplyr::left_join(dat_uci) %>%
  select(duration, tau_p, error, ci, error_lci, error_uci)

sims_hrange <- list(out, out_sum)
names(sims_hrange) <- c("data", "summary")

usethis::use_data(sims_hrange, overwrite = TRUE)
