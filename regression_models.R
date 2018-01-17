source("utils.R")
library(MASS) #Ridge
library(e1071) #SVM
library(nnet) #MLP

model.linear = function(training, ...) {
    real_training_values <- training$percent_change_next_weeks_price
    model <- glm (real_training_values ~  percent_change_price +
        percent_change_volume_over_last_wk +
        days_to_next_dividend +
        percent_return_next_dividend,
        data=training, family=gaussian, ...)
    return(model)
}

model.ridge = function(training, lambda=0.5, ...) {
    real_training_values <- training$percent_change_next_weeks_price
    model <- glm (real_training_values ~  percent_change_price +
        percent_change_volume_over_last_wk +
        days_to_next_dividend +
        percent_return_next_dividend,
        data=training, family=gaussian, ...)
    model.final <- lm.ridge (model,lambda=lambda)
    #Force coefficients into model
    model$coefficients[2:length(model$coefficients)] <- model.final$coef
    return(model)
}

model.linear_svm = function(training, ...) {
    real_training_values <- training$percent_change_next_weeks_price
    model <- svm(real_training_values ~  percent_change_price +
        percent_change_volume_over_last_wk +
        days_to_next_dividend +
        percent_return_next_dividend,
        data=training, kernel="linear", ...)
    return(model)
}


model.svm_rbf = function(training, ...) {
    real_training_values <- training$percent_change_next_weeks_price
    model <- svm(real_training_values ~  percent_change_price +
        percent_change_volume_over_last_wk +
        days_to_next_dividend +
        percent_return_next_dividend,
        data=training, kernel="radial", ...)
    return(model)
}


model.mlp = function(training, ...) {
    real_training_values <- training$percent_change_next_weeks_price
    model <- nnet(real_training_values ~  percent_change_price +
        percent_change_volume_over_last_wk +
        days_to_next_dividend +
        percent_return_next_dividend,
        data=training, ...)
    return(model)
}
