source("utils.R")
source("regression_models.R")

#Collect data
data_set <- getData(percent_change_price=TRUE,
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

message("Ridge Regression...")
for (lambda in seq(0.001,0.5,0.002)) {
    find_best(model.ridge, lambda=lambda)
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

plot(xdata, real_values, col="blue", pch="*", lty=1,
    ylim=c(min(real_values, prediction),max(real_values, prediction)) )
points(xdata, prediction, col="red", pch="*")
