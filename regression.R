source("utils.R")
library(MASS) #Ridge
library(e1071) #SVM
library(nnet) #MLP

#Collect data
data_set <- getData()

#Extract normalized training and test sets
training <- extractNormalizedTraining(data_set)
test <- extractNormalizedTest(data_set)

#Real values
real_training_values <- training$percent_change_next_weeks_price
real_test_values <- test$percent_change_next_weeks_price

#Linear Regression
model <- glm (real_training_values ~  percent_change_price +
                        percent_change_volume_over_last_wk +
                        days_to_next_dividend +
                        percent_return_next_dividend,
                data=training, family=gaussian)

prediction <- predict(model,test)

message("\n")
message("Results for linear regression\n\n")
message(sprintf("Training MASE: %s\n", mase(model)))
message(sprintf("Test MASE: %s\n", mase(prediction, real_test_values)))


#Ridge Regression
lambdes <- seq(0.001,0.5,0.001)
model <- glm (real_training_values ~  percent_change_price +
                        percent_change_volume_over_last_wk +
                        days_to_next_dividend +
                        percent_return_next_dividend,
                data=training, family=gaussian)

model.ridge <- lm.ridge (model, lambda = lambdes)
#select( lm.ridge(model, lambda = lambdes) ) -> 0.5 GCV
model.final <- lm.ridge (model,lambda=0.5)
#Force coefficients into model
model$coefficients[2:length(model$coefficients)] <- model.final$coef

prediction <- predict(model,test)

message("\n")
message("Results for ridge regression\n\n")
message(sprintf("Training MASE: %s\n", mase(model)))
message(sprintf("Test MASE: %s\n", mase(prediction, real_test_values)))


#Support Vector Machine with linear kernel
model <- svm(real_training_values ~  percent_change_price +
                        percent_change_volume_over_last_wk +
                        days_to_next_dividend +
                        percent_return_next_dividend,
                data=training, kernel="linear")

prediction <- predict(model,test)

message("\n")
message("Results for SVM with linear kernel regression\n\n")
message(sprintf("Training MASE: %s\n", mase(model)))
message(sprintf("Test MASE: %s\n", mase(prediction, real_test_values)))


#Support Vector Machine with RBF kernel
model <- svm(real_training_values ~  percent_change_price +
                        percent_change_volume_over_last_wk +
                        days_to_next_dividend +
                        percent_return_next_dividend,
                data=training, kernel="radial")

prediction <- predict(model,test)

message("\n")
message("Results for SVM with RBF kernel regression\n\n")
message(sprintf("Training MASE: %s\n", mase(model)))
message(sprintf("Test MASE: %s\n", mase(prediction, real_test_values)))


#MLP with only one hidden layer
model <- nnet(real_training_values ~  percent_change_price +
                        percent_change_volume_over_last_wk +
                        days_to_next_dividend +
                        percent_return_next_dividend,
                data=training, , size=20, maxit=1000, decay=0.01)

prediction <- predict(model,test)

message("\n")
message("Results for MLP single hidden layer regression\n\n")
message(sprintf("Training MASE: %s\n", mase(model)))
message(sprintf("Test MASE: %s\n", mase(prediction, real_test_values)))
