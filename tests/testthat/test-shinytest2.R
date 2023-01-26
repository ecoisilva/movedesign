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

