filter_df <- function(df, index) {
  df_f <- df[index, ] %>% 
    select(-c(1)) %>% 
    as.matrix %>% 
    t %>% 
    data.frame %>% 
    filter(!is.na(.))
  
  colnames(df_f) <- c("x")
  df_f
}

filter_df_test <- function(df, index) {
  df_f <- df[index, ] %>%
    as.matrix %>% 
    t %>% 
    data.frame %>% 
    filter(!is.na(.))
  
  colnames(df_f) <- c("x")
  df_f
}

read_df <- function(filepath, size) {
  if (size == "full") {
    data <- fread(filepath, header = T, sep = ",")
  } else {
    data <- fread(filepath, header = T, sep = ",", drop=c(1))
  }
}