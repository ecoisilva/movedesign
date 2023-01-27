library(shinytest2)

test_that("{shinytest2} recording: app runs", {
  
  shiny_app <- movedesign::run_app()
  app <- AppDriver$new(shiny_app, name = "workflow")

  app$set_inputs(`tab_about_1-which_data` = "Select")
  app$set_inputs(`tab_about_1-which_question` = "Home range")
  
  app$expect_values(input = "tab_about_1-which_question", screenshot_args = FALSE)
  
  out <- app$get_values()
  expect_identical(out$input$`tab_about_1-which_question`, "Home range")
  
  app$stop()
})

test_that("{shinytest2} recording: gps simulation works", {
  
  shiny_app <- movedesign::run_app()
  app <- AppDriver$new(shiny_app, name = "gps")
  # app$view()
  
  app$set_inputs(tabs = "regime")
  app$set_inputs(`tab_device_1-device_type` = "GPS")
  app$wait_for_idle(1000)
  
  app$set_inputs(`tab_device_1-gps_dur` = 2)
  app$set_inputs(`tab_device_1-gps_dur_units` = "years")
  app$set_inputs(`tab_device_1-deviceInput_log` = TRUE)
  
  app$set_inputs(`tab_device_1-regPlot_gps_selected` = "8", 
                 allow_no_input_binding_ = TRUE)
  app$wait_for_idle(1000)
  
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

