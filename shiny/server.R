library(shiny)

shinyServer(function(input, output, session) {
  
  output$graf <- renderPlot({
    
    ggplot(per.36.stats, mapping = aes_string(x=input$x_os, y=input$y_os)) + geom_point()
    
  })
  
  
  
})


