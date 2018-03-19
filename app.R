#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(data.table)

options(shiny.maxRequestSize=300*1024^2) 

# Prepare template path to the data
DIR_DATA <- "./data/"
DIR_TRAIN <- "%s%s/train/"
DIR_TEST <- "%s%s/test/"

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("M4 visualization"),
   
   sidebarPanel(
     
   
     # fileInput("file1", "Choose CSV File",
     #           multiple = TRUE,
     #           accept = c("text/csv",
     #                      "text/comma-separated-values,text/plain",
     #                      ".csv")),
     uiOutput("selectSizeUi"),
     uiOutput("selectTsUi"),
     uiOutput("selectRowUi")
   ),
      
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("tsPlot")
    )
 )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({
    
    req(input$selectTs)
    
    filepath <- paste0(sprintf(DIR_TRAIN, DIR_DATA, input$selectSize), input$selectTs)
    cat(filepath)
    
    if (input$selectSize == "full") {
      data <- fread(filepath, header = T, sep = ",")
    } else {
      data <- fread(filepath, header = T, sep = ",", drop=c(1))
    }
    
  })
  
  df_filtered <- reactive({
    
    req(input$selectRow)
    
    df() %>% 
      filter(V1 == input$selectRow) %>% 
      select(-V1) %>% 
      as.matrix %>% 
      t %>% 
      data.frame %>% 
      filter(!is.na(.))
  })
   
   output$tsPlot <- renderPlot({
     
     req(df_filtered())

     plot.ts(df_filtered())
     
   })
   
   # Select size of the dataset
   output$selectSizeUi <- renderUI({
     selectInput("selectSize", "Select dataset size", 
                 choices = c("", list.dirs(DIR_DATA, recursive = F, full.names = F)))
   })
   
   # Select timeseries
   output$selectTsUi <- renderUI({
     
     req(input$selectSize)
     filepath <- sprintf(DIR_TRAIN, DIR_DATA, input$selectSize)
     
     selectInput("selectTs", "Select timeseries", 
                 choices = c("", list.files(filepath, recursive = F, full.names = F)))
   })
   
   
   # Select row of selected dataset
   output$selectRowUi <- renderUI({
     
     req(input$selectTs)
     
     selectInput("selectRow", "Select row", choices = df()$V1)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

