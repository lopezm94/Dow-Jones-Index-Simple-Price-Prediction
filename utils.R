
## Open data

data <- read.csv("dow_jones_index.data", header = TRUE, quote = "\"", dec = ".", check.names=TRUE)


## Pre proceso de los datos tal y como los vamos a tratar---------------------------------------------
data$f.quarter    <- as.factor(data$quarter) #1
data$f.quarter <- factor(c("Primero", "Segundo")) #2
data$f.stock    <- as.factor(data$stock) #3
data$f.data   <- as.factor(data$date) #4

data$open <- as.numeric(sub("\\$","", data$open)) #5
data$high <- as.numeric(sub("\\$","", data$high)) #6
data$low <- as.numeric(sub("\\$","", data$low)) #7
data$close <- as.numeric(sub("\\$","", data$close)) #8
data$percent_change_price <- as.numeric(data$percent_change_price) #9
data$percent_change_volume_over_last_wk <- as.numeric(data$percent_change_volume_over_last_wk) #10
data$previous_weeks_volume <- as.numeric(data$previous_weeks_volume) #11
data$next_weeks_open <- as.numeric(sub("\\$","", data$next_weeks_open)) #12
data$next_weeks_close <- as.numeric(sub("\\$","", data$next_weeks_close)) #13
data$percent_change_next_weeks_price <- as.numeric(data$percent_change_volume_over_last_wk) #14
data$days_to_next_dividend <- as.numeric(data$days_to_next_dividend) #15
data$percent_return_next_dividend <- as.numeric(data$percent_change_price) #16

#Create training and test data
training <-  data[which(data$quarter ==  1),]
training$f.data <- factor(c(1,2,3,4,5,6,7,8,9,10,11,12))
##training$f.data <- factor(c('Sem1','Sem2','Sem3','Sem4','Sem5','Sem6','Sem7','Sem8','Sem9','Sem10','Sem11','Sem12'))
training <- training[c(-1,-2,-3,-10,-11,-12,-13,-15)]
test <-  data[which(data$quarter ==  2),]
test$f.data <- factor(c(1,2,3,4,5,6,7,8,9,10,11,12))
##test$f.data <- factor(c('Sem1','Sem2','Sem3','Sem4','Sem5','Sem6','Sem7','Sem8','Sem9','Sem10','Sem11','Sem12','Sem13'))
test <- test[c(-1,-2,-3,-10,-11,-12,-13,-15)]



## FIN-----------------------------------------------------------------------------------------------------

#getFeatureTable = function(window_size=1, )

### FUNCIONES AUXILIARES QUE HE ENCONTRADO

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
