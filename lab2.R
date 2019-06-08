library(MASS)
library(ISLR)
library(dplyr)
library(ggplot2)


know_data <- Boston %>% 
  dplyr::select(lstat,medv)

know_data

names(know_data)[1] <- "input"
names(know_data)[2] <- "output"


know_data.rows <- nrow(know_data)

get_boot <- function(x){
  index<-sample(1:know_data.rows, 
                size = know_data.rows, 
                replace = TRUE)
  return(know_data[index,])
}

get_formula <- function(dataset, degree = 2) {
  formula <- paste0("I(","input^",1:degree,")",collapse = '+')
  formula <- paste0("output ~ ",formula)
  return(formula)
}

fit_lm <- function(dataset,degree=2){
  formula <- paste0("I(","input^",1:degree,")",collapse = '+')
  formula <- paste0("output ~ ",formula)
  fit <- lm(formula,data = dataset)
  return(fit)
}

model_plot_data <- function(fit){
  xaxis<-
    seq(
      min(know_data$input),
      max(know_data$input),
      by=0.01)
  yaxis<-predict(fit,tibble(input=xaxis))
  return(tibble(input=xaxis,output=yaxis))
}

varianza <- function(df, reg, boots, n) {
  # fix randomization seed, make sample() reproducible
  set.seed(123)
  
  sampleSize <- n
  bRows <- df[sample(nrow(df), sampleSize), ]
  # do 7 bootstrap replications
  bSamples <- boots
  # make container for results
  bResults <- rep(NA, bSamples) 
  
  var_df <- data.frame('VAR' = 1:1)
  
  # loop over bootstraps
  for (b in seq_len(bSamples)) { 
    # make bootstrap draw from bRows
    bData <- bRows[sample(sampleSize, size = sampleSize, replace = TRUE), ]
    # compute your statistic of interest
    y <- lm(formula = reg, data = bData)
    y
    y_hat <- predict(y)
    esperanza <- mean(y_hat)
    #print(esperanza)
    #print(y_hat)
    
    var <- (1 / (boots - 1)) * sum((y_hat - esperanza)**2)
    var_df <- rbind(var_df, VAR = c(var))
  }
  var_df = var_df[-1,]
  mean_var <- mean(var_df)
  return(mean_var)
}

varianza(df = know_data, boots = 7, n = 10, reg = get_formula(dataset = know_data, degree = 2))
