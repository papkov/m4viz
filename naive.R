load("./data/M4.RData")

library(data.table)
library(janitor)
library(dplyr)
library(forecast)

source("./benchmark.R")

train.path <- "./data/full/train/"
test.path <- "./data/full/test/"
naive.path <- "./data/naive/"
naive2.path <- "./data/naive2/"
#file.name <- "Hourly.csv"


# Create separate files for naive predictions
res <- sapply(list.files(train.path, full.names = F), function(file.name) {
  print(file.name)
  train <- fread(paste0(train.path, file.name))
  test <- fread(paste0(test.path, file.name)) 
  
  horizon <- ncol(test) %>% as.numeric
  # Naive
  naive.predictions <- apply(train, 1, function(row) {
    row <- na.omit(row)
    c(row[1], row %>% last %>% as.numeric %>% rep(horizon))
  }) %>% t() %>% as_tibble()
  
  # Naive 2
  naive2.predictions <- apply(train, 1, function(row) {
    row <- row %>% na.omit
    des <- compute_deseason(as.numeric(row[-1]), horizon)
    f3 <- naive(des$input, h = horizon)$mean * des$SIout
    c(row[1], f3)
  }) %>% t() %>% as_tibble()
  
  
  # Save naive predictions
  write.csv(naive.predictions, 
            paste0(naive.path, file.name),
            row.names = F)
  
  write.csv(naive2.predictions, 
            paste0(naive2.path, file.name),
            row.names = F)
  
  # Return
  TRUE
})


# Merge files
naive <- NULL
naive2 <- NULL

res <- sapply(list.files(naive.path, full.names = F), function(file.name) {
  print(file.name)
  fn <- fread(paste0(naive.path, file.name))
  fn2 <- fread(paste0(naive2.path, file.name))
  
  if(is.null(naive)) {
    naive <<- fn
  } else {
    naive <<- rbind(naive, fn, fill = T)
  }
  
  
  if(is.null(naive2)) {
    naive2 <<- fn2
  } else {
    naive2 <<- rbind(naive2, fn2, fill = T)
  }
  
  TRUE
})

write.csv(naive, 
          paste0("./data/naive_merged.csv"),
          row.names = F)

write.csv(naive2, 
          paste0("./data/naive2_merged.csv"),
          row.names = F)
