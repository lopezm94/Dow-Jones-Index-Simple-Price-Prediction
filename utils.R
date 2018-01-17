### Preprocess Data

data <- read.csv("dow_jones_index.data", header = TRUE, quote = "\"", dec = ".", check.names=TRUE)

## Encode columns
data$quarter    <- as.factor(data$quarter) #1
data$stock    <- as.factor(data$stock) #2
data$date   <- as.factor(data$date) #3
data$open <- as.numeric(sub("\\$","", data$open)) #4
data$high <- as.numeric(sub("\\$","", data$high)) #5
data$low <- as.numeric(sub("\\$","", data$low)) #6
data$close <- as.numeric(sub("\\$","", data$close)) #7
data$volume <- as.numeric(data$volume) #8
data$percent_change_price <- as.numeric(data$percent_change_price) #9
data$percent_change_volume_over_last_wk <- as.numeric(data$percent_change_volume_over_last_wk) #10
data$previous_weeks_volume <- as.numeric(data$previous_weeks_volume) #11
data$next_weeks_open <- as.numeric(sub("\\$","", data$next_weeks_open)) #12
data$next_weeks_close <- as.numeric(sub("\\$","", data$next_weeks_close)) #13
data$percent_change_next_weeks_price <- as.numeric(data$percent_change_next_weeks_price) #14
data$days_to_next_dividend <- as.numeric(data$days_to_next_dividend) #15
data$percent_return_next_dividend <- as.numeric(data$percent_return_next_dividend) #16

## Remove useless rows (rows containing NA)
data <- data[which(!is.na(data$previous_weeks_volume)),]

### Auxiliar Functions

## Make new dataset only with desired rows
getData = function(
        stock_name=FALSE,
        percent_change_price=TRUE, percent_change_volume_over_last_wk=TRUE,
        days_to_next_dividend=TRUE, percent_return_next_dividend=TRUE
    ) {
    #Copy Data
    processed_data = data

    #Remove column if not specified
    undesired_columns = c(1,3,4,5,6,7,8,11,12,13,14)
    if (!stock_name) {
        undesired_columns <- c(undesired_columns,2)
    }
    if (!percent_change_price) {
        undesired_columns <- c(undesired_columns,9)
    }
    if (!percent_change_volume_over_last_wk) {
        undesired_columns <- c(undesired_columns,10)
    }
    if (!days_to_next_dividend) {
        undesired_columns <- c(undesired_columns,15)
    }
    if (!percent_return_next_dividend) {
        undesired_columns <- c(undesired_columns,16)
    }

    processed_data <- processed_data[-undesired_columns]
    processed_data[["percent_change_next_weeks_price"]] <- data$percent_change_next_weeks_price

    return(processed_data)
}

extractNormalizedTraining = function(x, p=0.8) {
    rows = dim(x)[1]
    columns = dim(x)[2]
    training <- x[1:(rows*p),]
    for (c in 1:columns){
        column_data <- training[,c]
        training[,c] <- (column_data - mean(column_data)) / sd(column_data)
    }
    return(training)
}

extractNormalizedTest = function(x, p=0.8) {
    rows = dim(x)[1]
    columns = dim(x)[2]
    test <- x[(rows*p+1):rows,]
    training <- x[1:(rows*p),]
    for (c in 1:columns){
        column_data <- training[,c]
        test[,c] <- (test[,c] - mean(column_data)) / sd(column_data)
    }
    return(test)
}

#Estimates Root Mean Square Error given a model or two vectors
rmse = function(model, test_data) {
    result <- NA
    if (missing(test_data)) {
        result <- mean(model$residuals^2, na.rm=TRUE)
    } else {
        real_values <- test_data$percent_change_next_weeks_price
        prediction <- predict(model, test_data)
        result <- mean((prediction-real_values)^2, na.rm=TRUE)
    }
    return(sqrt(result))
}

MASE <- function(f,y) { # f = vector with forecasts, y = vector with actuals
    if(length(f)!=length(y)){ stop("Vector length is not equal") }
    n <- length(f)
    return(mean(abs((y - f) / ((1/(n-1)) * sum(abs(y[2:n]-y[1:n-1]))))))
}

#Estimates Mean Absolute Scaled Error given a model or two vectors
mase = function(model, test_data) {
    result <- NA
    if (missing(test_data)) {
        result <- MASE(model$fitted, model$residuals+model$fitted)
    } else {
        real_values <- test_data$percent_change_next_weeks_price
        prediction <- predict(model, test_data)
        result <- MASE(prediction, real_values)
    }
    return(sqrt(result))
}

#Time series cross validation. k: number of partitions
cross_validate = function(model_builder, data_set, k=5, ...) {
    cum_error = 0
    for (i in 1:(k-1)) {
        partition <- i/k
        test <- extractNormalizedTest(data_set, p=partition)
        training <- extractNormalizedTraining(data_set, p=partition)
        model <- model_builder(training, ...)
        cum_error = cum_error + mase(model, test)
    }
    average = cum_error/k
    return(average)
}

normalize = function(x) {
  data.min = min(x)
  data.max = max(x)
  return((x - data.min)/(data.max - data.min))
}

log10_f = function(x) {
  return(log10(x))
}

checkError = function(predicted, real, type) {
  tab = table(factor(predicted, levels = levels(real)), real)
  print(paste("Error", type))
  print(tab)

  error = 100 - (sum(diag(tab)))/length(predicted) * 100
  print(error)
}

hist.with.normal <- function (x, main="", xlabel=deparse(substitute(x)),path, ...)
{
  h <- hist(x,plot=F, ...)
  s <- sd(x)
  m <- mean(x)
  ylim <- range(0,h$density,dnorm(0,sd=s))
  p <- ggplot(data.frame(f = x),aes(x)) +
    geom_histogram(aes(y = ..density..)) +
    stat_function(fun=function(x) dnorm(x,m,s), colour = "red") +
    xlab(xlabel) +
    ggtitle(main)
  ggsave(file=path,plot = p)
}

save.boxplot <- function(x,main="",ylabel=deparse(substitute(x)),path, ...){
  p <- ggplot(data.frame(f = x),aes(x="",y=x)) +
    geom_boxplot() +
    theme(axis.title.x = element_blank()) +
    ggtitle(main) +
    ylab(ylabel)
  ggsave(file=path,plot = p)
}

barplot.percent <- function(df,df.x,df.y,main="",xlabel=deparse(substitute(df.x)),ylabel=deparse(substitute(df.y)),col=2){
  library(reshape2)
  library(scales)
  DF1 <- melt(prop.table(table(df.x,df.y), col), id.var=xlabel)
  p <- 0
  if(col == 2){
    p <- ggplot(DF1, aes(x=df.x,y=value,fill=df.y)) +
      geom_bar(position = "fill",stat = "identity") +
      xlab(xlabel) +
      ylab("percent") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_discrete(name=ylabel) +
      ggtitle(main)
  }
  else{
    p <- ggplot(DF1, aes(x=df.y,y=value,fill=df.x)) +
      geom_bar(position = "fill",stat = "identity") +
      xlab(ylabel) +
      ylab("percent") +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_discrete(name=xlabel) +
      ggtitle(main) +
      coord_flip()
  }
  print(p)

}

barplot.facet <- function(df,df.x,df.y,main="",xlabel=deparse(substitute(df.x)),ylabel=deparse(substitute(df.y))){
  library(reshape2)
  DF1 <- melt(prop.table(table(df.x,df.y), 2), id.var=xlabel)
  p <- ggplot(DF1, aes(df.x,y=value)) + geom_bar(stat="identity") +
    facet_wrap(~ df.y) +
    ggtitle(main)
  print(p)
}
