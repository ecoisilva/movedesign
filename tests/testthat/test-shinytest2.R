library(shinytest2)

test_that("{shinytest2} recording: app runs", {
  skip_on_cran() # potential issue with CRAN build servers
  skip_on_ci() # interactive() fails in CI
  
  skip_on_os(os = c("mac", "linux")) 
  # issue with wait_for_idle(), wait_for_value()
  
  shiny_app <- movedesign::run_app()
  app <- AppDriver$new(shiny_app, name = "workflow")
  # app$view()
  
  app$set_inputs(`tab_about_1-which_data` = "Select")
  app$set_inputs(`tab_about_1-which_question` = "Home range")
  
  app$expect_values(input = "tab_about_1-which_question", 
                    screenshot_args = FALSE)
  
  out <- app$get_values()
  expect_identical(
    out$input$`tab_about_1-which_question`,
    "Home range")
  
  app$set_inputs(`tab_about_1-which_question` = 
                   c("Home range", "Speed & distance"))
  
  app$expect_values(input = "tab_about_1-which_question", 
                    screenshot_args = FALSE)
  out <- app$get_values()
  expect_identical(
    out$input$`tab_about_1-which_question`,
    c("Home range", "Speed & distance"))
  
  app$stop()
})


test_that("{shinytest2} recording: gps simulation works", {
  skip_on_cran() # potential issue with CRAN build servers
  skip_on_ci() # interactive() fails in CI
  
  skip_on_os(os = c("mac", "linux")) 
  # issue with wait_for_idle(), wait_for_value()
  
  shiny_app <- movedesign::run_app()
  app <- AppDriver$new(shiny_app, name = "gps")
  # app$view()
  
  app$set_inputs(tabs = "regime")
  app$set_inputs(`tab_device_1-device_type` = "GPS")
  
  app$wait_for_value(output = "tab_device_1-regPlot_gps",
                     ignore = list(NULL))
  
  out <- app$get_values()
  expect_identical(
    !is.null(out$output$`tab_device_1-regPlot_gps`), 
    TRUE)
  
  app$stop()
})


test_that("{shinytest2} recording: gps plot selection works", {
  skip_on_cran() # potential issue with CRAN build servers
  skip_on_ci() # interactive() fails in CI
  
  skip_on_os(os = c("mac", "linux")) 
  # issue with wait_for_idle(), wait_for_value()
  
  set_timeout <- 3 * 60 * 1000 # 3 mins
  
  shiny_app <- movedesign::run_app()
  app <- AppDriver$new(shiny_app, name = "plot")
  # app$view()
  
  app$set_inputs(tabs = "regime")
  app$set_inputs(`tab_device_1-device_type` = "GPS")
  
  app$wait_for_value(output = "tab_device_1-regPlot_gps",
                     ignore = list(NULL),
                     timeout = set_timeout)
  
  app$set_inputs(`tab_device_1-deviceInput_log` = TRUE)
  
  app$set_inputs(`tab_device_1-regPlot_gps_selected` = "8", 
                 allow_no_input_binding_ = TRUE)
  
  app$wait_for_value(output = "tab_device_1-regPlot_gps",
                     ignore = list(NULL),
                     timeout = set_timeout)
  
  app$click("tab_device_1-validate_gps")
  
  out <- app$get_values()
  expect_identical(
    !is.null(out$output$`tab_device_1-regBlock_n`$html), 
    TRUE)
  expect_identical(
    !is.null(out$output$`tab_device_1-regPlot_gps`), 
    TRUE)
  
  app$stop()
})
