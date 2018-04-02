library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(data.table)
library(DT)
library(shinycssloaders)

# TODO
# Generate reports
# Histo

options(shiny.maxRequestSize=30*1024^2) 

# Prepare template path to the data
DIR_DATA <- "./data/"
DIR_TRAIN <- "%s%s/train/"
DIR_TEST <- "%s%s/test/"

# Load benchmarking and default predictions code
source("./benchmark.R")
# Load helpers
source("./helper.R")

# Load all naive predictions
naive_forecast <- NULL # For syntax highlighting
naive_smape <- NULL
naive_mase <- NULL
merged_train <- NULL
merged_test <- NULL
load("./data/naive_forecast.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("M4 visualization"),
   
   sidebarPanel(
     
   
     fileInput("selectSubmission", "Choose submission file",
               multiple = T,
               accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")),
     
     br(),
     uiOutput("checkboxUi"),
     uiOutput("selectSizeUi"),
     uiOutput("selectTsUi"),
     uiOutput("selectRowUi"),
     uiOutput("selectMetricsUi"),
     uiOutput("btnRunBenchmarkUi")
   ),
      
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("tsPlot") %>% withSpinner,
       br(),
       DT::DTOutput("metricsRowUi"),
       br(),
       DT::DTOutput("metricsMeanUi"),
       br(),
       DT::DTOutput("benchmarkResultsUI")
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
    
    filter_df_test(df_test(), df_selected_index())
  })
  
  # Compute default forecasts
  forecasts_default <- reactive({
    
    # req(df_train())
    # req(df_test())
    
    # Default forecasting is deprecacted
    # withProgress(value = 1, message = "Get forecasts", {
    #   get_forecasts(df_train(), df_test())
    # })
    
    naive_forecast
    
  })
  
  # External predictions
  forecasts_raw <- reactive({
    
    req(input$selectSubmission)
    
    fc.dfs <- apply(input$selectSubmission, 1, function(f) {
      path <- f["datapath"][[1]]
      name <- f["name"][[1]]
      
      fread(path, header = T, sep = ",")
    })
    
    names(fc.dfs) <- input$selectSubmission$name
    fc.dfs
  })
  
  forecasts_external <- reactive({
    
    fe <- forecasts_default()
    
    if (!plyr::empty(input$selectSubmission)) {
      # For each selected file read forecasts 
      apply(input$selectSubmission, 1, function(f) {

        path <- f["datapath"][[1]]
        name <- f["name"][[1]]
        
        df <- forecasts_raw()[[name]]
        
        # df <- fread(path, header = T, sep = ",")
        # print(df)
        # Order by naive predictions
        df <- df[match(df$V1, fe$Naive$V1) %>% order, ]
        
        # Subset by intersection with the train data
        df <- df[df$V1 %in% df_train()$V1, ]
        # TODO make forecast invariant to sorting
        
        # Update default forecast
        # fe <<- lapply(1:length(fe), function(j) {
        #   fc <- fe[[j]]
        #   fc[[name]] <- as.numeric(df[j, ])
        #   fc
        # })
        
        # Update in new format: list of dfs
        fe[[name]] <<- df
      })
    }
    
    # print(names(fe[[1]]))
    
    return(fe)
    
  })
  
  # Filter forecasts for rendering
  forecasts_filtered <- reactive({
    
    req(df_selected_index())
    req(df_train())
    req(input$selectMetrics)
    
    # For each forecast df extract a particular row
    fc.fil <- lapply(forecasts_external(), function(fc) {
      # Check, where df_train row is in the forecast df
      sname <- df_train()[df_selected_index(), 1] %>% unlist
      print(paste("Forecast names", fc[1:5, 1]))
      idx <- which(fc[, 1] == sname)
      print(paste(sname, idx))
      fc[idx, ] %>% as.numeric %>% na.omit
    }) 
    names(fc.fil) <- names(forecasts_external())
    
    fc.fil %>% 
      data.frame %>% 
      select(input$selectMetrics)
    
    # Deprecated
    # forecasts_external()[[df_selected_index()]] %>%
    # # forecasts_default()[[df_selected_index()]] %>% 
    #   data.frame %>% 
    #   select(input$selectMetrics)
  })
  
  
  # Merge dataframe for plotting
  df_merged <- reactive({
    
    req(forecasts_filtered())
    
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
      
      # print(df_forecast)
      
      df <- rbind(df_train, df_test, df_forecast)
    })
    
    df
  })
  
  # Compute smape for all forecasts
  smape <- reactive({
    
    req(df_test())
    req(df_train())
    req(forecasts_external())
    
    idx <- match(unlist(df_train()[, 1]), unlist(naive_smape[, 1]))
    smape <- naive_smape[idx, -1]
    
    # Check for methods in forecasts_external others than naive and naive2
    methods <- names(forecasts_external())
    new_methods <- methods[!(methods %in% names(smape))]
    
    withProgress(message = "Calculate SMAPE", value = 1, {
      if (identical(new_methods, character(0))) {
        return(smape)
      } else {
        new_smape <- get_smape(df_test(), df_train(), forecasts_external()[new_methods])
        return(cbind(smape, new_smape))
      }
    })
  })
  
  # Compute mase for all forecasts
  mase <- reactive({
    
    req(df_test())
    req(df_train())
    req(forecasts_external())
    
    idx <- match(unlist(df_train()[, 1]), unlist(naive_mase[, 1]))
    mase <- naive_mase[idx, -1]    
    
    # Check for methods in forecasts_external others than naive and naive2
    methods <- names(forecasts_external())
    new_methods <- methods[!(methods %in% names(mase))]
    
    withProgress(message = "Calculate MASE", value = 1, {
      if (identical(new_methods, character(0))) {
        return(mase)
      } else {
        new_mase <- get_mase(df_test(), df_train(), forecasts_external()[new_methods])
        return(cbind(mase, new_mase))
      }
    })
    
    
    
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
                choices = c("", names(forecasts_external()), input$selectSubmission$name), 
                multiple = T,
                selected = c(names(forecasts_external())[1],
                             input$selectSubmission$name))
  })
  
  output$metricsRowUi <- DT::renderDT({
    req(input$selectSize)
    req(input$selectTs)
    req(input$chkMetrics)
    
    row_smape <- smape()[df_selected_index(), ] %>% round(3)
    row_mase <- mase()[df_selected_index(), ] %>% round(3)
    owa <- ((row_mase/row_mase[["Naive2"]] + row_smape/row_smape[["Naive2"]]) / 2) %>% round(3)
    
    row_metrics <- rbind(row_smape, row_mase, owa) %>% 
      DT::datatable(rownames = c("SMAPE", "MASE", "OWA"),
                    options = list(dom = 't'),
                    caption = paste("Row metrics for the row:", input$selectRow))
  })
  
  output$metricsMeanUi <- DT::renderDT({
    
    req(input$selectSize)
    req(input$chkMetrics)
    
    mean_mase <- apply(mase(), 2, mean) %>% round(3)
    mean_smape <- apply(smape(), 2, mean) %>% round(3)
    owa <- ((mean_mase/mean_mase["Naive2"] + mean_smape/mean_smape["Naive2"]) / 2) %>% round(3)
    
    row_metrics <- rbind(mean_smape, mean_mase, owa) %>% 
      DT::datatable(rownames = c("SMAPE", "MASE", "OWA"),
                    options = list(dom = 't'),
                    caption = paste("Mean metrics for the set:", input$selectTs, input$selectSize))
  })
  
  output$checkboxUi <- renderUI({
    checkboxInput("chkMetrics", "Calculate MASE and SMAPE", 
                  value = T)
  })
  
  output$btnRunBenchmarkUi <- renderUI({
    
    # Allow tests only with uploaded submission
    req(input$selectSubmission)
    shiny::actionButton("btnRunBenchmark", "Run benchmarks")
  })
  
  observeEvent(input$btnRunBenchmark, {
    # Read raw forecasts
    forecasts <- forecasts_raw()
    new_methods <- names(forecasts) # All methods in forecasts_raw are new
    
    # IF you want lo load domain sequentially, use code below
    # Open and process train and test files sequentially
    
    # Create empty dataframes to store calculated MASE and SMAPE
    mase <- data.frame()
    smape <- data.frame()
    
    bmarks <- sapply(DIR_TRAIN %>% sprintf(DIR_DATA, input$selectSize) %>% dir,
           function(file.name) {
             
             withProgress(value = 0, message = paste("Process", file.name), {
               # Read domain files
               incProgress(1/3, detail = "Read data")
               train.path <- paste0(sprintf(DIR_TRAIN, DIR_DATA, input$selectSize), file.name)
               test.path <- paste0(sprintf(DIR_TEST, DIR_DATA, input$selectSize), file.name)
               df_train <- read_df(train.path, input$selectSize)
               df_test <- read_df(test.path, input$selectSize)
               
               # Check indices that are matched from forecast to the current loaded set
               idx <- match(unlist(forecasts[[1]][, 1]), unlist(df_train[, 1])) %>% 
                 na.omit %>% as.numeric
               
               # If all indices are NA, omit this dataset
               if (identical(idx, numeric(0))) {
                 print("Indices not found!")
                 return(FALSE)
               }
               
               # Calculate benchmarks
               incProgress(2/3, detail = "Get SMAPE")
               new_smape <- get_smape(df_test, df_train, forecasts[c(new_methods)])
               incProgress(3/3, detail = "Get MASE")
               new_mase <- get_mase(df_test, df_train, forecasts[c(new_methods)])
               
               # Write outside of the cycle
               smape <<- rbind(smape, new_smape)
               mase <<- rbind(mase, new_mase) 
               
               return(TRUE)
             })
           })
    
    # Show status for different domains
    print(bmarks)
    
    # Subset precalculated naive predictions
    idx <- match(unlist(forecasts[[1]][, 1]), unlist(naive_smape[, 1]))
    mase <- data.frame(naive_mase[idx, -1], mase)
    smape <- data.frame(naive_smape[idx, -1], smape)
    
    mean_mase <- apply(mase, 2, mean) %>% round(3)
    mean_smape <- apply(smape, 2, mean) %>% round(3)
    owa <- ((mean_mase/mean_mase["Naive2"] + mean_smape/mean_smape["Naive2"]) / 2) %>% round(3)
    
    print(mean_mase)
    print(mean_smape)
    print(owa)

    output$benchmarkResultsUI <- DT::renderDT({
      rbind(mean_smape, mean_mase, owa) %>%
          DT::datatable(rownames = c("SMAPE", "MASE", "OWA"),
                        options = list(dom = 't'),
                        caption = paste("Benchmark for", input$selectSubmission$name))
    })
    
    # ELSE IF you want to load merged dataset, use code below
    # Load merged data
    # withProgress(value = 0, message = "Benchmark", {
    #   
    #   incProgress(1/4, detail = "Loading merged data")
    #   load("./data/merged.RData")
    # 
    #   # Match indices over the whole dataset
    #   idx <- match(unlist(forecasts[[1]][, 1]), unlist(naive_smape[, 1]))
    #   # Remove first column in the forecasts df
    #   # forecasts <- lapply(forecasts, function(fc) fc[, -1])
    #   # names(forecasts) <- new_methods
    #   
    #   # print(forecasts[c(new_methods)])
    #   
    #   mase <- naive_mase[idx, -1]  
    #   smape <- naive_smape[idx, -1]
    #   df_train <- merged_train[idx, ]
    #   df_test <- merged_test[idx, ]
    #   
    #   # Remove merged sets immediately
    #   merged_train <- NULL
    #   merged_test <- NULL
    #   
    #   # Calculate SMAPE and MASE
    #   incProgress(2/4, detail = "Calculate SMAPE")
    #   new_smape <- get_smape(df_test, df_train, forecasts[c(new_methods)])
    #   # print(new_smape)
    #   
    #   smape <- cbind(smape, new_smape)
    #   
    #   incProgress(3/4, detail = "Calculate MASE")
    #   new_mase <- get_mase(df_test, df_train, forecasts[c(new_methods)])
    #   # print(new_mase)
    # 
    #   mase <- cbind(mase, new_mase)
    #   
    #   # Prepare tables
    #   mean_mase <- apply(mase, 2, mean) %>% round(3)
    #   mean_smape <- apply(smape, 2, mean) %>% round(3)
    #   owa <- ((mean_mase/mean_mase["Naive2"] + mean_smape/mean_smape["Naive2"]) / 2) %>% round(3)
    #   
    #   print(mean_mase)
    #   print(mean_smape)
    #   print(owa)
    #   
    #   incProgress(4/4, detail = "Render DT")
    #   output$benchmarkResultsUI <- DT::renderDT({
    #     
    #     row_metrics <- rbind(mean_smape, mean_mase, owa) %>% 
    #       DT::datatable(rownames = c("SMAPE", "MASE", "OWA"),
    #                     options = list(dom = 't'),
    #                     caption = paste("Benchmark for", input$selectSubmission$name))
    #   })
    # })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

