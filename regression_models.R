source("utils.R")
library(MASS) #Ridge
library(e1071) #SVM
library(nnet) #MLP

model.linear = function(training, ...) {
    model <- glm (percent_change_next_weeks_price ~  .,
        data=training, family=gaussian, ...)
    return(model)
}

model.ridge = function(training, lambda=0.5, ...) {
    model <- glm (percent_change_next_weeks_price ~  .,
        data=training, family=gaussian, ...)
    model.final <- lm.ridge (model,lambda=lambda)
    #Force coefficients into model
    model$coefficients[2:length(model$coefficients)] <- model.final$coef
    return(model)
}

model.linear_svm = function(training, ...) {
    model <- svm(percent_change_next_weeks_price ~  .,
        data=training, kernel="linear", ...)
    return(model)
}

model.svm_rbf = function(training, ...) {
    model <- svm(percent_change_next_weeks_price ~  .,
        data=training, kernel="radial", ...)
    return(model)
}

model.mlp = function(training, ...) {
    model <- nnet(percent_change_next_weeks_price ~  .,
        data=training, ...)
    return(model)
}
