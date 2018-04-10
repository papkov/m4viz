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
    # c(row[1], row %>% last %>% as.numeric %>% rep(horizon))
    naive(as.numeric(row[-1]), h = horizon)$mean
  }) %>% 
    t() %>% 
    as.data.frame() %>% 
    cbind(id = unlist(train[, 1]), ., stringsAsFactors = F) %>% 
    as_tibble()
  
  # Naive 2
  naive2.predictions <- apply(train, 1, function(row) {
    row <- na.omit(row)
    des <- compute_deseason(as.numeric(row[-1]), horizon)
    naive(des$input, h = horizon)$mean * des$SIout
  }) %>% 
    t() %>% 
    as.data.frame() %>% 
    cbind(id = unlist(train[, 1]), ., stringsAsFactors = F) %>% 
    as_tibble()
  
  identical(naive.predictions, naive2.predictions) %>% paste("Identical:", .) %>% print()
  
  
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
naive <- data.frame()
naive2 <- data.frame()

res <- sapply(list.files(naive.path, full.names = F), function(file.name) {
  print(file.name)
  fn <- fread(paste0(naive.path, file.name))
  fn2 <- fread(paste0(naive2.path, file.name))
  
  naive <<- rbind(naive, fn, fill = T)
  naive2 <<- rbind(naive2, fn2, fill = T)
  
  TRUE
})

# Make numeric
naive <- apply(naive[, -1], 2, as.numeric) %>% 
  cbind(naive[, 1], .) %>% 
  as.data.frame()

naive2 <- apply(naive2[, -1], 2, as.numeric) %>% 
  cbind(naive2[, 1], .) %>% 
  as.data.frame()

# Save as csv
write.csv(naive, 
          paste0("./data/naive_merged.csv"),
          row.names = F)

write.csv(naive2, 
          paste0("./data/naive2_merged.csv"),
          row.names = F)

# Create forecast list

# naive_forecasts <- lapply(naive[, 1] %>% unlist %>% as.vector, 
#                           function(rname) { # Iterate over timeseries name
#   list("Naive" = naive[which(naive$V1 == rname), -1] %>% na.omit(),
#        "Naive2" = naive2[which(naive2$V1 == rname), -1] %>% na.omit())
# })

naive_forecast <- list(
  "Naive" = naive,
  "Naive2" = naive2
)



# Save as RData
save(naive, naive2, file = "./data/naive.RData")
save(naive_forecast, file = "./data/naive_forecast.RData")

# Load RData object
load("./data/naive.RData")


### Pre-calculate MASE and SMAPE for naive predictions

# Open and merge all test files
test_dfs <- lapply(dir(test.path, full.names = T), function(file.name) {
  fread(file.name, header = T)
})
merged_test <- rbindlist(test_dfs, fill = T)

train_dfs <- lapply(dir(train.path, full.names = T), function(file.name) {
  fread(file.name, header = T)
})
merged_train <- rbindlist(train_dfs, fill = T)


# Calculate MASE and SMAPE for naive predictions for the whole dataset
naive_mase <- get_mase(merged_test, merged_train, naive_forecast)
naive_mase$name <- merged_train$V1

naive_smape <- get_smape(merged_test, merged_train, naive_forecast)
naive_smape$name <- merged_train$V1

# Read pytohn-generated
naive_mase <- read.csv("./data/naive_mase.csv", header = T,
                       stringsAsFactors = F)
names(naive_mase)[1] = "name"

scaling <- read.csv("./data/scaling.csv", header = T,
                    stringsAsFactors = F)

write.csv(naive_smape, 
          paste0("./data/naive_smape.csv"),
          row.names = F)

fwrite(merged_train, 
       paste0("./data/merged_train.csv"),
       row.names = F)

fwrite(merged_test, 
       paste0("./data/merged_test.csv"),
       row.names = F)

naive_smape <- naive_smape[, c(3, 1, 2)]

load("./data/naive_forecast.RData")
save(naive_forecast, 
     naive_smape, 
     naive_mase, 
     scaling,
     # merged_train,
     # merged_test,
     file = "./data/naive_forecast.RData")

save(merged_train,
     merged_test,
     file = "./data/merged.RData")


train_dfs <- NULL
test_dfs <- NULL


########
library(rbenchmark)
train <- read.csv("data/10000/train/Daily.csv")
test <- read.csv("data/10000/test/Daily.csv")

benchmark(
  # "MASE cpp" = sapply(1:100, function(i) mase_cal_cpp(as.numeric(train[i, -1]), 
  #                                                     as.numeric(test[i, -1]), 
  #                                                     as.numeric(test[i, -1]))),
  "MASE ord" = sapply(1:10000, function(i) mase_cal(as.numeric(train[i, -1]), 
                                                  as.numeric(test[i, -1]), 
                                                  as.numeric(test[i, -1]),
                                                  as.numeric(scaling[i, -1]))),
  # "SMAPE cpp" = sapply(1:100, function(i) smape_cal_cpp(as.numeric(test[i, -1]), 
  #                                                       as.numeric(test[i, -1]))),
  "SMAPE ord" = sapply(1:10000, function(i) smape_cal(as.numeric(test[i, -1]), 
                                                    as.numeric(test[i, -1]))),
  replications = 1
)
