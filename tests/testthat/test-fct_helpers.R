test_that("{abbrv_unit}, Unit abbreviation function", {
  
  ini <- c("year", "month", "week",
           "day", "hour", "minute", "second",
           "kilometer", "meter", "km", "m",
           "km^2", "m^2", "ha",
           "square kilometer", "square meter", "hectare",
           "kilometers/hour", "meters/second",
           "kilometers/day" , "meters/day")
  
  out <- c("yr", "mth", "wk",
           "d", "hr", "min", "sec",
           "km", "m", "km", "m",
           "km\u00B2", "m\u00B2", "ha",
           "km\u00B2", "m\u00B2", "ha",
           "km/h", "m/s",
           "km/day" , "m/day")
  
  for (i in 1:length(ini)) {
    expect_equal(
      movedesign:::abbrv_unit(ini[i]),
      out[i])
  }
  
})

test_that("{fix_unit} Fix unit function", {
  
  expect_equal(
    movedesign:::fix_unit(1.005, "years",
                          digits = 2,
                          ui = FALSE,
                          convert = FALSE),
    data.frame(value = 1, unit = "year"))
  
  expect_equal(
    movedesign:::fix_unit(0.845, "kilometers/hour",
                          digits = 1,
                          ui = FALSE,
                          convert = FALSE),
    data.frame(value = 0.8, unit = "km/h"))
  
  expect_equal(
    movedesign:::fix_unit(3.5^12, "square meters",
                          digits = 2,
                          ui = TRUE,
                          convert = TRUE),
    data.frame(value = 3.4, unit = "km\u00B2"))
  
})


