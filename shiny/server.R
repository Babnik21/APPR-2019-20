library(shiny)

shinyServer(function(input, output, session) {
  
  output$graf <- renderPlot({
    x_os <- which(colnames(per.36.stats) == input$x_os)
    y_os <- which(colnames(per.36.stats) == input$y_os)
    
    ggplot(per.36.stats, mapping = aes_string(x=names(per.36.stats)[x_os], y=names(per.36.stats)[y_os])) + 
      geom_point()
    
  })
  
  
  
})


