names_benchmarks <- c("Naive", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Com")

## Metrics code are provided by organizers, I won't change them

smape_cal <- function(outsample, forecasts){
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)
  smape <- (abs(outsample - forecasts) * 200) / (abs(outsample) + abs(forecasts))
  
  return(smape)
}

mase_cal <- function(insample, outsample, forecasts){
  #Used to estimate MASE
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA, frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep <- mean(abs(insample-forecastsNaiveSD), na.rm = TRUE)
  
  outsample <- as.numeric(outsample)
  forecasts <- as.numeric(forecasts)
  mase <- abs(outsample-forecasts) / masep
  
  return(mase)
}

naive_seasonal <- function(input, fh){
  #Used to estimate Seasonal Naive
  frcy <- frequency(input)
  frcst <- naive(input, h=fh)$mean 
  
  if (frcy > 1){ 
    frcst <- head(rep(as.numeric(tail(input,frcy)), fh), fh) + frcst - frcst
  } 
  
  return(frcst)
}

Theta.classic <- function(input, fh){
  #Used to estimate Theta classic
  
  #Set parameters
  wses <- wlrl<-0.5
  theta <- 2
  #Estimate theta line (0)
  observations <- length(input)
  xt <- 1:observations
  xf <- (observations+1):(observations+fh)
  train <- data.frame(input=input, xt=xt)
  test <- data.frame(xt = xf)
  
  estimate <- lm(input ~ poly(xt, 1, raw=TRUE))
  thetaline0In <- as.numeric(predict(estimate))
  thetaline0Out <- as.numeric(predict(estimate,test))
  
  #Estimate theta line (2)
  thetalineT <- theta*input+(1-theta)*thetaline0In
  sesmodel <- ses(thetalineT, h=fh)
  thetaline2In <- sesmodel$fitted
  thetaline2Out <- sesmodel$mean
  
  #Theta forecasts
  forecastsIn <- (thetaline2In*wses)+(thetaline0In*wlrl)
  forecastsOut <- (thetaline2Out*wses)+(thetaline0Out*wlrl)
  
  #Zero forecasts become positive
  for (i in 1:length(forecastsOut)){
    if (forecastsOut[i] <0){ forecastsOut[i]<-0 }
  }
  
  output=list(fitted = forecastsIn, mean = forecastsOut,
              fitted0 = thetaline0In, mean0 = thetaline0Out,
              fitted2 = thetaline2In, mean2 = thetaline2Out)
  
  return(output)
}

SeasonalityTest <- function(input, ppy){
  #Used to determine whether a time series is seasonal
  tcrit <- 1.645 # No idea, what it is
  if (length(input) < 3*ppy){
    test_seasonal <- FALSE
  } else {
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    
    if (is.na(test_seasonal)) {
      test_seasonal <- FALSE 
    }
  }
  
  return(test_seasonal)
}

compute_deseason <- function(input, fh) {
  #Estimate seasonaly adjusted time series
  ppy <- frequency(input) 
  ST <- F
  if (ppy > 1) { 
    ST <- SeasonalityTest(input,ppy) 
  }
  
  if (ST){
    Dec <- decompose(input, type = "multiplicative")
    des_input <- input/Dec$seasonal
    # What the fuck is going on here?
    SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
  } else {
    des_input <- input
    SIout <- rep(1, fh)
  }
  
  return(list(SIout = SIout, input = des_input))
}

# Don't compute extra values, just repeat the last one
simple_naive <- function(input, fh) {
  rep(input[length(input)], fh)
}

default_forecasting <- function(input, fh){
  #Used to estimate the statistical benchmarks of the M4 competition
  
  # des <- compute_deseason(input, fh)
  
  # print("Naive")
  # f1 <- naive(input, h=fh)$mean #Naive
  f1 <- simple_naive(input, fh)
  # print("SNaive")
  # f2 <- naive_seasonal(input, fh = fh) #Seasonal Naive
  # print("Naive2")
  # f3 <- naive(des$input, h = fh)$mean * des$SIout #Naive2
  # print("Ses")
  # f4 <- ses(des$input, h = fh)$mean * des$SIout #Ses
  # # print("Holt")
  # f5 <- holt(des$input, h = fh, damped = F)$mean * des$SIout #Holt
  # # print("Damped")
  # f6 <- holt(des$input, h = fh, damped = T)$mean * des$SIout #Damped
  # # print("Theta")
  # f7 <- Theta.classic(input=des$input, fh=fh)$mean * des$SIout #Theta
  # # print("Comb")
  # f8 <- (f4+f5+f6)/3 #Comb
  # 
  # res <- list(f1,f2,f3,f4,f5,f6,f7,f8)
  res <- list(f1)
  names(res) <- names_benchmarks[1]
  
  return(res)
}

# Predictions for our data
get_forecasts <- function(data_train, data_test) { 
  forecasts <- lapply(1:nrow(data_train), function(i) {
    # Predict as many data points as there are in the test set
    fh <- length(data_test[i, ])
    print(paste(i, "/", nrow(data_train)))
    
    data_train[i, ] %>% 
      select_if(!is.na(.)) %>% # Filter NA columns
      select(-c(1)) %>%  # First train col contains names, skip it 
      as.numeric() %>% 
      default_forecasting(fh)
  })
}

# Predictions for example data
get_forecasts_example <- function(data_train, data_test) {  
  forecasts <- lapply(1:length(data_train), function(i) {
    fn <- length(data_test[[i]]) # Predict as many data points as there are in the test set
    # print(data_train[[i]])
    # print(fh)
    default_forecasting(data_train[[i]], fh)
  })
}

## How to update forecasts table with custom predictions

# updated_forecast <- lapply(forecasts, function(fc) {
#   fc$new_var = c(1:6)
#   fc
# })


get_smape <- function(data_test, forecasts) {
  smape <- sapply(1:length(data_test), function(i) {
    sapply(forecasts[[i]], function(fc) smape_cal(data_test[[i]], fc) %>% mean)
  }) %>% t %>% data.frame
}

get_mase <- function(data_train, data_test, forecasts) {
  mase <-  sapply(1:length(data_test), function(i) {
    sapply(forecasts[[i]], function(fc) mase_cal(data_train[[i]], data_test[[i]], fc) %>% mean)
  }) %>% t %>% data.frame
}


## How to calculate mean parameters

#mean_mase <- apply(mase, 2, mean)
#mean_smape <- apply(smape, 2, mean)
#owa <- (mean_mase/mean_mase["Naive2"] + mean_smape/mean_smape["Naive2"]) / 2
