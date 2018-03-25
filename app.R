library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(data.table)
library(forecast)

# TODO
# Generate reports
# Histo

options(shiny.maxRequestSize=300*1024^2) 

# Prepare template path to the data
DIR_DATA <- "./data/"
DIR_TRAIN <- "%s%s/train/"
DIR_TEST <- "%s%s/test/"

# Load benchmarking and default predictions code
source("./benchmark.R")
# Load helpers
source("./helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("M4 visualization"),
   
   sidebarPanel(
     
   
     fileInput("selectSubmission", "Choose submission file",
               multiple = F,
               accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")),
     
     br(),
     uiOutput("selectSizeUi"),
     uiOutput("selectTsUi"),
     uiOutput("selectRowUi"),
     uiOutput("selectMetricsUi")
   ),
      
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("tsPlot")
    )
 )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive ######
  
  # Read and update train dataframe
  df_train <- reactive({
    
    req(input$selectTs)
    
    filepath <- paste0(sprintf(DIR_TRAIN, DIR_DATA, input$selectSize), input$selectTs)
    print(filepath)
    
    read_df(filepath, input$selectSize)
  })
  
  # Read and update test dataframe
  df_test <- reactive({
    
    req(input$selectTs)
    
    filepath <- paste0(sprintf(DIR_TEST, DIR_DATA, input$selectSize), input$selectTs)
    print(filepath)
    
    read_df(filepath, input$selectSize)
  })
  
  # Make selection of the row
  df_selected_index <- reactive({
    
    req(input$selectRow)
    
    which(df_train()[, 1] == input$selectRow)
  })

  
  # Filter train dataset to show one row
  df_train_filtered <- reactive({
    
    req(df_selected_index())
    filter_df(df_train(), df_selected_index())
  })

  # Filter test dataset to show one row
  df_test_filtered <- reactive({
    
    req(df_selected_index())
    filter_df(df_test(), df_selected_index())
  })
  
  # Compute default forecasts
  forecasts_default <- reactive({
    
    req(df_train())
    req(df_test())
    
    get_forecasts(df_train(), df_test())
  })
  
  # Filter forecasts for rendering
  forecasts_filtered <- reactive({
    
    req(df_selected_index())
    req(input$selectMetrics)
    
    forecasts_default()[[df_selected_index()]] %>% 
      data.frame %>% 
      select(input$selectMetrics)
  })
  
  
  # Merge dataframe for plotting
  df_merged <- reactive({
    
    # Enable progress bar
    withProgress(message = "Merge dataframes", value = 0, {
      
      incProgress(1/3, detail = "Format train data")
      df_train <- df_train_filtered() %>% 
        mutate(n = 1:(nrow(.))) %>% 
        mutate(cl = rep("Train", nrow(.)))
      
      incProgress(2/3, detail = "Format test data")
      df_test <- df_test_filtered() %>% 
        mutate(n = nrow(df_train):(nrow(.) + nrow(df_train) - 1)) %>% 
        mutate(cl = rep("Test", nrow(.)))
      
      incProgress(3/3, detail = "Format forecasts data")
      df_forecast <- forecasts_filtered() %>% 
        mutate(n = nrow(df_train):(nrow(.) + nrow(df_train) - 1)) %>% 
        gather(cl, x, -n)
      
      df <- rbind(df_train, df_test, df_forecast)
    })
    return(df)
  })
  
  # Compute smape for all forecasts
  smape <- reactive({
    
    req(df_test())
    req(forecasts_default())
    
    get_smape(df_test(), forecasts_default())
  })
  
  # Compute mase for all forecasts
  smape <- reactive({
    
    req(df_test())
    req(df_train())
    req(forecasts_default())
    
    get_mase(df_train(), df_test(), forecasts_default())
  })
  
  # UI ########
  
  # Render timeseries plot
  output$tsPlot <- renderPlotly({
    
    # req(df_train_filtered())
    
    p <- ggplot(df_merged(), aes(x = n, y = x, col = cl))+
      geom_line()+
      theme_bw()
    
    ggplotly(p)
    # plot.ts(df_train_filtered())
    
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
    
    selectInput("selectRow", "Select row", 
                choices = df_train()[, 1])
  })
  
  # Select metrics to show
  output$selectMetricsUi <- renderUI({
    
    req(input$selectRow)
    
    # used_metrics <- unique(df_merged()$cl)
    # selectInput("selectMetrics", "Select metrics",
    #             choices = c("", used_metrics, input$selectSubmission$name), 
    #             multiple = T,
    #             selected = c("Train", "Test"))
    
    selectInput("selectMetrics", "Select metrics",
                choices = c("", names_benchmarks, input$selectSubmission$name), 
                multiple = T,
                selected = names_benchmarks[1])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

