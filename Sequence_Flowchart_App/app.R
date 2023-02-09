#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

setwd("~/Desktop/Projects/Pitch Sequence Project/MLB Pitch Sequence Flowchart")

source("Create Line Chart Diagram.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MLB Pitch Sequence Flowchart Generator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            selectizeInput(
              inputId = "playerID",
              label = "Player ID",
              choices = c("All",sort(unique(data$BAT_ID))),
              selected = NULL
            ),
            
            selectizeInput(
              inputId = "inning",
              label = "Inning",
              choices = c("All",sort(unique(as.numeric(data$INN_CT)))),
              selected = "All",
              multiple = TRUE
            ),
            
            actionButton(
              inputId = "go_button",
              label = "Go"
            )
        ),

        mainPanel(
           plotOutput("flowchart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$flowchart <- renderPlot({
        
    input$go_button
    
    df <- data
    
    if(input$playerID != "All"){
      df <- df %>% 
        filter(BAT_ID == input$playerID)
    }
    
    if(!("All" %in% input$inning)){
      df <- df %>% 
        filter(INN_CT %in% input$inning)
    }
    
    props <- find_proportions(df)
    
    plot_sequences(props)[1][[1]]
        
  })
  
  output$freq_table <- render_gt({
    
    input$go_button
    
    df <- data
    
    if(input$playerID != "All"){
      df <- df %>% 
        filter(BAT_ID == input$playerID)
    }
    
    if(!("All" %in% input$inning)){
      df <- df %>% 
        filter(INN_CT %in% input$inning)
    }
    
    props <- find_proportions(df)
    
    plot_sequences(props)[2][[1]]
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
