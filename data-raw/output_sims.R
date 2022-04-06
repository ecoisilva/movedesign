## code to prepare `output_sims` dataset goes here

output_sims <- read.csv(system.file("extdata",
                                    "output_sims.csv",
                                    package = "movedesign"))

usethis::use_data(output_sims, overwrite = TRUE)
