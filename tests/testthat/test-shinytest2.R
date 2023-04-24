library(shinytest2)

testthat::skip_on_ci()

testthat::test_that("{shinytest2}: workflow works", {
  
  testthat::skip_on_cran()
  
  # # Set depending on interactive execution:
  # if (interactive()) {
  #   app_path <- test_path("../../")
  # } else { app_path <- movedesign::run_app() }
  
  app <- AppDriver$new(
    # app_path,
    name = "workflow",
    variant = platform_variant(),
    width = 1200, height = 960
  )
  
  # app$view()
  
  app$set_inputs(`tab_about_1-which_data` = "Select")
  app$set_inputs(`tab_about_1-which_question` = "Home range")
  
  app$expect_values(input = "tab_about_1-which_question")
  
  out <- app$get_values()
  expect_identical(
    out$input$`tab_about_1-which_question`,
    "Home range")
  
  app$set_inputs(`tab_about_1-which_question` = 
                   c("Home range", "Speed & distance"))
  
  out <- app$get_values()
  expect_identical(
    out$input$`tab_about_1-which_question`,
    c("Home range", "Speed & distance"))
  
  app$stop()
  
}) # end of test_that


testthat::test_that("{shinytest2}: gps simulation works", {
  
  testthat::skip_on_cran()
  
  app <- AppDriver$new(
    name = "gps",
    variant = platform_variant(),
    width = 1200, height = 960
  )
  
  # app$view()
  
  app$set_inputs(tabs = "device")
  app$set_inputs(`tab_design_1-device_type` = "GPS")
  
  out_plot <- app$wait_for_value(
    output = "tab_design_1-regPlot_gps",
    ignore = list(NULL))
  
  # message("\n")
  # print(substr(out_plot, 1, 12))
  
  app$set_inputs(`tab_design_1-regPlot_gps_selected` = "1",
                 allow_no_input_binding_ = TRUE, wait_ = TRUE)
  app$click("tab_design_1-validate_gps")
  
  # app$get_screenshot()
  
  out <- app$get_values()$output
  expect_identical(
    !is.null(out$`tab_design_1-regPlot_gps`),
    TRUE)
  expect_identical(
    class(out$`tab_design_1-regText`$html)[1],
    "html")
  
  app$wait_for_idle()
  app$expect_screenshot()
  app$stop()
  
}) # end of test_that


testthat::test_that("{shinytest2}: Simulate works", {
  
  testthat::skip_on_cran()
  
  app <- AppDriver$new(
    name = "simulate",
    variant = platform_variant(),
    width = 1200, height = 960)
  
  # app$view()
  
  app$set_inputs(`tab_about_1-overwrite_active` = TRUE,
                 `tab_about_1-which_data` = "Simulate",
                 `tab_about_1-which_question` = "Home range")
  
  app$set_inputs(tabs = "sims")
  
  app$click(input = "tab_sims_1-generateSeed")
  app$click(input = "tab_sims_1-run_sim") #, timeout_ = 3 * 60 * 1000)
  
  out <- app$get_values()
  # print(head(out$export$`tab_sims_1-data0`)[1,])
  # print(class(out$export$`tab_sims_1-data0`)[1])
  expect_equal(class(out$export$`tab_sims_1-data0`)[1], "telemetry")
  expect_identical(out$output$`tab_sims_1-sims_speed`,
                   "6.1 km/day")
  
  # app$expect_values()
  app$stop()
  
}) # end of test_that


testthat::test_that("{shinytest2}: Select + GPS works", {

  testthat::skip_on_cran()

  app <- AppDriver$new(
    name = "gps_and_simulate",
    variant = platform_variant(),
    width = 1200, height = 960)
  # app$view()
  
  app$set_inputs(`tab_about_1-overwrite_active` = TRUE,
                 `tab_about_1-which_data` = "Select",
                 `tab_about_1-which_question` = "Home range")

  app$set_inputs(tabs = "data_select")
  app$set_inputs(`tab_data_select_1-sp_selected` = "buffalo")
  app$set_inputs(`tab_data_select_1-id_selected` = "Cilla")

  app$click("tab_data_select_1-validate_select", 
            timeout_ = 15 * 1000)
  app$click("tab_data_select_1-selectButton_extract",
            timeout_ = 15 * 1000)

  app$set_inputs(tabs = "device")
  app$set_inputs(`tab_design_1-device_type` = "GPS")

  app$set_inputs(`tab_design_1-gps_dur` = 12,
                 `tab_design_1-gps_dur_unit` = "months",
                 `tab_design_1-gps_dti_max` = "1 fix every day")

  app$set_inputs(`tab_design_1-regPlot_gps_selected` = "1",
                 allow_no_input_binding_ = TRUE) #, wait_ = TRUE)

  out_text <- app$wait_for_value(
    output = "tab_design_1-regText",
    ignore = list(NULL))
  app$wait_for_idle()
  
  out <- app$get_values()
  
  # message("\n")
  # print(names(out$output))
  # message("\n")
  
  # out_plot <- app$wait_for_value(
  #   output = "tab_design_1-regPlot_gps",
  #   ignore = list(NULL))
  
  # message("\n")
  # print(substr(out_plot, 1, 12))
  # message("\n")
  
  app$click("tab_design_1-validate_gps", timeout_ = 15 * 1000)
  app$click("tab_design_1-run_sim_new", timeout_ = 2 * 60 * 1000)

  out <- app$get_values()$export
  # message("\n")
  # print(out)
  # message("\n")

  expect_equal(nrow(out$`tab_design_1-simulating_gps`), 18)
  # expect_equal(nrow(out$`tab_design_1-data1`)[1], 354)

  # app$expect_values()
  # app$expect_screenshot()
  app$stop()

}) # end of test_that
