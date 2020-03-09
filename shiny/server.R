library(shiny)

shinyServer(function(input, output, session) {
  
  output$graf <- renderPlot({
    x_os <- which(colnames(datoteka_za_shiny_36) == input$x_os)
    y_os <- which(colnames(datoteka_za_shiny_36) == input$y_os)
    
    ggplot(datoteka_za_shiny_36, mapping = aes_string(x=names(datoteka_za_shiny_36)[x_os], y=names(datoteka_za_shiny_36)[y_os])) + 
      geom_point()
    
  })
  
  
  
})


