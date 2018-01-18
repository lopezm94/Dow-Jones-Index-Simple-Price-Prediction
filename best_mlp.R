library(plotly)
source("utils.R")
source("regression_models.R")

#Collect data
data_set <- getData(
    stock=TRUE,
    percent_change_price=TRUE,
    percent_change_volume_over_last_wk=TRUE,
    days_to_next_dividend=TRUE,
    percent_return_next_dividend=TRUE)

#Extract normalized training and test sets
training <- extractNormalizedTraining(data_set)
test <- extractNormalizedTest(data_set)

min_error <- Inf
final_model <- NA
final_cross_validation_error <- NA

exp_seq = function(a,b,step) {
    seq <- c(a)
    while (a < b) {
        a <- a*step
        seq <- c(seq,a)
    }
    return(seq)
}

find_best = function(model_builder, ...) {
    model <- model_builder(training, ...)
    error <- mase(model, test)
    if (min_error > error) {
        min_error <<- error
        final_model <<- model
        final_cross_validation_error <<- cross_validate(model_builder, data_set, ...)
    }
}

message("\nStart looking for optimal fit\n")

message("One Hidden Layer MLP...")
for (size in seq(2,20,6)) {
    for (decay in seq(0.001,0.5,0.004)) {
        find_best(model.mlp, maxit=500, size=size, decay=decay, trace=FALSE)
    }
}

message("\nDone!\n\n")

message("Results\n")
show(final_model)
training_error <- mase(final_model)
message(sprintf("Training MASE: %s", training_error))
test_error <- mase(final_model, test)
message(sprintf("Test MASE: %s", test_error))
message(sprintf("Cross-Validation MASE error: %s\n", final_cross_validation_error))

real_values <- test$percent_change_next_weeks_price
prediction <- predict(final_model, test)
xdata <- seq(1:length(real_values))

#plot(xdata, real_values, col="blue", pch="*", lty=1,
#    ylim=c(min(real_values, prediction),max(real_values, prediction)) )
#points(xdata, prediction, col="red", pch="*")

plot_ly(x = xdata, y = real_values, name = 'Real values', type = 'scatter', mode = 'markers') %>%
  add_trace(y = prediction, name = 'Predictions', mode = 'markers') %>%
  layout(title = 'Difference between real and predicted',
         yaxis = list(title="Result"),
         xaxis = list(title="Input"))
