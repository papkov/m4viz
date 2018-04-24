require(sparkline)
require(DT)
require(shiny)

forecasts <- read.csv("data/owa10k2.csv", stringsAsFactors = F)
df_test <- read.csv("data/10000/test/Daily.csv", stringsAsFactors = F)
df_train <- read.csv("data/10000/train/Daily.csv", stringsAsFactors = F)
forecasts <- forecasts[forecasts[,1] %in% df_train[, 2], ]

# create data with sparklines
spark_data <- data.frame(
  id = c('spark1', 'spark2', 'spark3'),
  spark = c(
    spk_chr(values = 1:3, elementId = 'spark1'),
    spk_chr(values = 3:1, elementId = 'spark2'),
    spk_composite(
      sparkline(1:3, fillColor = FALSE), sparkline(3:1, fillColor = FALSE)
    ) %>% htmltools::as.tags() %>% as.character()
  )
)

spark_data <- data.frame(
  id = forecasts[,1],
  spark = sapply(1:nrow(df_test), function(i) {
      sparkline::spk_composite(
        sparkline::sparkline(
          round(as.vector(na.omit(unlist(df_test[i, ]))), 2),
          # 3:1,
          type="line",
          fillColor = FALSE,
          width = 80,
          height = 60,
          lineColor ='blue'),
        sparkline::sparkline(
          round(as.vector(na.omit(unlist(forecasts[i, -1]))), 2),
          # 1:3,
          type="line",
          fillColor = FALSE,
          width = 80,
          height = 60,
          lineColor ='red')
      ) %>% # spk_chr()
        # htmlwidgets::as.tags.htmlwidget() %>% 
        # htmltools::as.character.shiny.tag.list() %>% 
        # htmltools::HTML()
        htmltools::as.tags() %>% as.character()
    })
)

###  adding this <------------
cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')

ui <- fluidPage(
  ###  and this <------------
  htmlwidgets::getDependency('sparkline'),
  DT::dataTableOutput("tbl")
)

server <- function(input, output) {
  
  output$tbl <- DT::renderDataTable(
    expr = spark_data,
    escape = FALSE,
    ###  and this <------------
    selection = list(mode = "single"),
    options = list(dom = 'pt',
                   fnDrawCallback = htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}'),
                   scrollY = '400px',
                   paging = FALSE
    )
  )
}

shinyApp(ui = ui, server = server)
