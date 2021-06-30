function(input, output, session) {
  # initialize widget
  output$widget <- renderImportSettings({
    importSettings(layer_names)
  })
  # update text outputs
  observeEvent(input$widget, {
    output$message <-
      renderText({ paste(capture.output(str(input$widget)), collapse = "\n") })
  })
}
