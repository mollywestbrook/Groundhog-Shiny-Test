#Making a really simple app with a graphic to test Groundhog Compatability

library(groundhog)

groundhog.day <- "2025-04-15"

groundhog.library(shiny, groundhog.day)
groundhog.library(here, groundhog.day)
groundhog.library(tidyverse, groundhog.day)
groundhog.library(data.table, groundhog.day)
groundhog.library(plotly, groundhog.day)
groundhog.library(bslib, groundhog.day)

#organize iris set
iris <- force(iris)
iris <- setDT(iris)
irislong <- melt(iris, 
                 id.vars=c("Species"), 
                 measure.vars=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                 variable.name=c("MeasureType"),
                 value.name="Value")
irislong[, color := case_when(Species == "setosa" ~ "#7b68ee",
                              Species == "virginica" ~ "#ccccff",
                              Species == "versicolor" ~ "#483d8b")]

##############################################################################

ui <- fluidPage(
  theme = bs_theme(preset = "sandstone"),
  
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px; margin-bottom: 20px;",
    tags$h2(("Iris Visualizer"), style = "margin: 0;"),
    tags$img(src = "iris.jpg", height = "60px")
  ),
  
  card(
      layout_sidebar(
          sidebar = sidebar(
            "Select Iris Species",
             selectInput(
              inputId = "select_species",
              label = "Iris Species",
              choices = c("Setosa" = "setosa",
                          "Versicolor" = "versicolor",
                          "Virginica" = "virginica"),
              selected = "Setosa",
              multiple = F
            ),
          ),
          
          mainPanel(
              width="100%",
              plotlyOutput("irisplot")
          )
    )
    
  )
  
)

################################################################################

server <- function(input, output) {
  
  iris_reactive <- reactive ({
    req(input$select_species)
    specieschoice <- input$select_species
    irischoice <- irislong[Species == specieschoice, ]
    return(irischoice)
  })
  
  output$irisplot <- renderPlotly({
    iris <- iris_reactive()
    
    irisgg <- ggplot(iris, aes(x=MeasureType, y=Value, fill=color))+
      geom_boxplot()+
      theme_classic()+
      scale_fill_identity()+
      guides(fill="none")+
      theme(text=element_text(size=15))
    
    irisplotly <- ggplotly(irisgg) 
    irisplotly
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
