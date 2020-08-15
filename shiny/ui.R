library(shiny)

shinyUI(fluidPage(
  titlePanel("Ostali grafi"),
  sidebarPanel(
    selectInput(inputId  = "x_os", label = "Izberite kategorijo na x osi",
                choices  = colnames(per.36.stats)[c(3:29)],
                selected = NULL, multiple = FALSE, selectize = TRUE
    ),
    selectInput(inputId  = "y_os", label = "Izberite kategorijo na y osi",
                choices  = colnames(per.36.stats)[c(3:29)] ,
                selected = NULL, multiple = FALSE, selectize = TRUE
    )
  ),
  
  mainPanel(
    plotOutput("graf")
    h4("Tukaj si lahko ogledamo še mnoge druge morebitne
povezave med dvema statističnima podatkoma.")
  )
  
  
  
  
))