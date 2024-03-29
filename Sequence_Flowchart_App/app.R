#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#setwd("~/Desktop/Projects/Pitch Sequence Project/MLB Pitch Sequence Flowchart")
setwd("~/Projects/MLB-Pitch-Sequence-Flowchart")


source("create_line_chart.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MLB Pitch Sequence Flowchart Generator"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(2,
          
            selectizeInput(
              inputId = "playerID_1",
              label = "Player ID",
              choices = c("All",sort(unique(data$BAT_ID))),
              selected = NULL
            ),
            
            selectizeInput(
              inputId = "outs_1",
              label = "Outs",
              choices = c("All",0:2),
              selected = "All",
              multiple = FALSE
            ),
            
            selectizeInput(
              inputId = "runners_1",
              label = "Runners on Base",
              choices = c("Any", "000", "100", "010", "001", "110", "011", "111"),
              selected = "Any",
              multiple = TRUE
            ),
            
            checkboxGroupInput(
              inputId = "pit_hand_1",
              label = "Pitcher Handedness",
              choices = c("L","R"),
              selected = c("L","R")
            ),
            
            checkboxGroupInput(
              inputId = "bat_hand_1",
              label = "Batter Handedness",
              choices = c("L","R"),
              selected = c("L","R")
            )
            
        ),

        column(4,
           plotOutput("flowchart_1"),
           gt_output("table_1")
        ),
        
        column(4,
           plotOutput("flowchart_2"),
           gt_output("table_2")
        ),
        
        column(2,
               
               selectizeInput(
                 inputId = "playerID_2",
                 label = "Player ID",
                 choices = c("All",sort(unique(data$BAT_ID))),
                 selected = NULL
               ),
               
               selectizeInput(
                 inputId = "outs_2",
                 label = "Outs",
                 choices = c("All",0:2),
                 selected = "All",
                 multiple = FALSE
               ),
               
               selectizeInput(
                 inputId = "runners_2",
                 label = "Runners on Base",
                 choices = c("Any", "000", "100", "010", "001", "110", "011", "111"),
                 selected = "Any",
                 multiple = TRUE
               ),
               
               checkboxGroupInput(
                 inputId = "pit_hand_2",
                 label = "Pitcher Handedness",
                 choices = c("L","R"),
                 selected = c("L","R")
               ),
               
               checkboxGroupInput(
                 inputId = "bat_hand_2",
                 label = "Batter Handedness",
                 choices = c("L","R"),
                 selected = c("L","R")
               )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$flowchart_1 <- renderPlot({
    
    df <- data
    
    if(input$playerID_1 != "All"){
      df <- df %>% 
        filter(BAT_ID == input$playerID_1)
    }
    
    if(!("All" %in% input$outs_1)){
      df <- df %>% 
        filter(OUTS_CT %in% input$outs_1)
    }
    
    if(!("Any" %in% input$runners_1)){
      first <- as.logical(as.numeric(substr(input$runners_1,1,1)))
      second <- as.logical(as.numeric(substr(input$runners_1,2,2)))
      third <- as.logical(as.numeric(substr(input$runners_1,3,3)))
      
      df <- df %>% 
        filter(runner_on_first == first,
               runner_on_second == second,
               runner_on_third == third)
    }
    
    df <- df %>% 
      filter(PIT_HAND_CD %in% input$pit_hand_1 &
               BAT_HAND_CD %in% input$bat_hand_1)
    
    props <- find_proportions(df)
    
    plot_sequences(props)[1][[1]]
        
  })
  
  output$table_1 <- render_gt({
    
    df <- data
    
    if(input$playerID_1 != "All"){
      df <- df %>% 
        filter(BAT_ID == input$playerID_1)
    }
    
    if(!("All" %in% input$outs_1)){
      df <- df %>% 
        filter(OUTS_CT == input$outs_1)
    }
    
    if(!("Any" %in% input$runners_1)){
      first <- as.logical(as.numeric(substr(input$runners_1,1,1)))
      second <- as.logical(as.numeric(substr(input$runners_1,2,2)))
      third <- as.logical(as.numeric(substr(input$runners_1,3,3)))
      
      df <- df %>% 
        filter(runner_on_first == first,
               runner_on_second == second,
               runner_on_third == third)
    }
    
    df <- df %>% 
      filter(PIT_HAND_CD %in% input$pit_hand_1 &
               BAT_HAND_CD %in% input$bat_hand_1)
    
    props <- find_proportions(df)
    
    plot_sequences(props)[2][[1]]
    
  })
  
  output$flowchart_2 <- renderPlot({
    
    df <- data
    
    if(input$playerID_2 != "All"){
      df <- df %>% 
        filter(BAT_ID == input$playerID_2)
    }
    
    if(!("All" %in% input$inning_2)){
      df <- df %>% 
        filter(OUTS_CT == input$outs_2)
    }
    
    if(!("Any" %in% input$runners_2)){
      first <- as.logical(as.numeric(substr(input$runners_2,1,1)))
      second <- as.logical(as.numeric(substr(input$runners_2,2,2)))
      third <- as.logical(as.numeric(substr(input$runners_2,3,3)))
      
      df <- df %>% 
        filter(runner_on_first == first,
               runner_on_second == second,
               runner_on_third == third)
    }
    
    df <- df %>% 
      filter(PIT_HAND_CD %in% input$pit_hand_2 &
               BAT_HAND_CD %in% input$bat_hand_2)
    
    props <- find_proportions(df)
    
    plot_sequences(props)[1][[1]]
    
  })
  
  output$table_2 <- render_gt({
    
    df <- data
    
    if(input$playerID_2 != "All"){
      df <- df %>% 
        filter(BAT_ID == input$playerID_2)
    }
    
    if(!("All" %in% input$outs_2)){
      df <- df %>% 
        filter(OUTS_CT %in% input$outs_2)
    }
    
    if(!("Any" %in% input$runners_2)){
      first <- as.logical(as.numeric(substr(input$runners_2,1,1)))
      second <- as.logical(as.numeric(substr(input$runners_2,2,2)))
      third <- as.logical(as.numeric(substr(input$runners_2,3,3)))
      
      df <- df %>% 
        filter(runner_on_first == first,
               runner_on_second == second,
               runner_on_third == third)
    }
    
    df <- df %>% 
      filter(PIT_HAND_CD %in% input$pit_hand_2 &
               BAT_HAND_CD %in% input$bat_hand_2)
    
    props <- find_proportions(df)
    
    plot_sequences(props)[2][[1]]
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
