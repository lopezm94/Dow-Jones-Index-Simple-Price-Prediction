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

message("Linear Regression...")
find_best(model.linear)

message("Ridge Regression...")
for (lambda in seq(0.001,0.5,0.002)) {
    find_best(model.ridge, lambda=lambda)
}

message("Linear SVM...")
find_best(model.linear_svm)

message("SVM with RBF...")
for (cost in exp_seq(2^-5,2^11,2^2)) {
    for (gamma in exp_seq(2^-15,2^3,2^2)) {
        find_best(model.svm_rbf, cost=cost, gamma=gamma)
    }
}

message("One Hidden Layer MLP...")
for (size in seq(2,20,6)) {
    for (decay in seq(0.001,0.5,0.004)) {
        find_best(model.mlp, maxit=500, size=size, decay=decay, trace=FALSE)
    }
}
message("\nTraining Done!\n\n")

message("Results\n")
show(final_model)
training_error <- mase(final_model)
message(sprintf("Training MASE: %s", training_error))
test_error <- mase(final_model, test)
message(sprintf("Test MASE: %s", test_error))
message(sprintf("Cross-Validation MASE error: %s\n", final_cross_validation_error))
