## code to prepare `vhf_models` dataset goes here

library(ctmm)
library(dplyr)

vhf <- data.frame(model = character(0),
                  ppm = numeric(0),
                  dur = numeric(0))
vhf_weights <- data.frame(model = character(0),
                          weight_g = numeric(0))

# Model A:

vhf_weights <- vhf_weights %>%
  dplyr::add_row(model = "A1015", weight_g = 0.55) %>%
  dplyr::add_row(model = "A1025", weight_g = 0.65) %>%
  dplyr::add_row(model = "A1035", weight_g = 0.75) %>%
  dplyr::add_row(model = "A1055", weight_g = 1.00) %>%
  dplyr::add_row(model = "A1065", weight_g = 1.30)

vhf_models <- vhf %>%
  dplyr::add_row(model = "A1015", ppm = 15, dur = 45 %#% "days") %>%
  dplyr::add_row(model = "A1015", ppm = 24, dur = 30 %#% "days") %>%
  dplyr::add_row(model = "A1015", ppm = 30, dur = 24 %#% "days") %>%
  dplyr::add_row(model = "A1015", ppm = 40, dur = 18 %#% "days") %>%

  dplyr::add_row(model = "A1025", ppm = 15, dur = 68 %#% "days") %>%
  dplyr::add_row(model = "A1025", ppm = 24, dur = 45 %#% "days") %>%
  dplyr::add_row(model = "A1025", ppm = 30, dur = 36 %#% "days") %>%
  dplyr::add_row(model = "A1025", ppm = 40, dur = 28 %#% "days") %>%

  dplyr::add_row(model = "A1035", ppm = 15, dur = 90 %#% "days") %>%
  dplyr::add_row(model = "A1035", ppm = 24, dur = 60 %#% "days") %>%
  dplyr::add_row(model = "A1035", ppm = 30, dur = 48 %#% "days") %>%
  dplyr::add_row(model = "A1035", ppm = 40, dur = 37 %#% "days") %>%

  dplyr::add_row(model = "A1055", ppm = 15, dur = 135 %#% "days") %>%
  dplyr::add_row(model = "A1055", ppm = 24, dur = 89 %#% "days") %>%
  dplyr::add_row(model = "A1055", ppm = 30, dur = 72 %#% "days") %>%
  dplyr::add_row(model = "A1055", ppm = 40, dur = 55 %#% "days") %>%

  dplyr::add_row(model = "A1065", ppm = 15, dur = 216 %#% "days") %>%
  dplyr::add_row(model = "A1065", ppm = 24, dur = 143 %#% "days") %>%
  dplyr::add_row(model = "A1065", ppm = 30, dur = 116 %#% "days") %>%
  dplyr::add_row(model = "A1065", ppm = 40, dur = 88 %#% "days")

usethis::use_data(vhf_models, overwrite = TRUE)
