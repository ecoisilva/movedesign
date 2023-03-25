## code to prepare `movmods` dataset goes here

library(dplyr)

movmods <- data.frame(name = character(0),
                      name_short = character(0),
                      tau_p = character(0),
                      tau_v = character(0),
                      hrange = character(0),
                      pars = character(0))

movmods <- movmods %>%
  dplyr::add_row(
    name = "Ind. Ident. Distr. (IID)",
    name_short = "IID",
    tau_p = "No",
    tau_v = "No",
    hrange = "Yes",
    pars = paste("\u03C4", "=", "NULL")
  )

movmods <- movmods %>%
  dplyr::add_row(
    name = "Brownian Motion (BM)",
    name_short = "BM",
    tau_p = "Yes",
    tau_v = "No",
    hrange = "No",
    pars = paste("\u03C4", "=", "\u221E")
  )

movmods <- movmods %>%
  dplyr::add_row(
    name = "Ornstein–Uhlenbeck (OU)",
    name_short = "OU",
    tau_p = "Yes",
    tau_v = "No",
    hrange = "Yes",
    pars = paste("\u03C4", "=", paste0("\u03C4","\u209A"))
  )

movmods <- movmods %>%
  dplyr::add_row(
    name = "Integrated Ornstein–Uhlenbeck (IOU)",
    name_short = "OU",
    tau_p = "Yes",
    tau_v = "Yes",
    hrange = "No",
    pars = paste0("\u03C4", " = ",
                 "{", 
                 "\u221E, ", "\u03C4", "\u1D65",
                 "}")
  )

movmods <- movmods %>%
  dplyr::add_row(
    name = "Ornstein-Uhlenbeck with foraging (OUF)",
    name_short = "OUF",
    tau_p = "Yes",
    tau_v = "Yes",
    hrange = "Yes",
    pars = paste0("\u03C4", " = ",
                  "{",
                  "\u03C4", "\u209A", ", ",
                  "\u03C4", "\u1D65",
                  "}")
  )

# View(movmods)

usethis::use_data(movmods, overwrite = TRUE)
