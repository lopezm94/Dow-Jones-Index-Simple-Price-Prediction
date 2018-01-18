# Dow-Jones-Index-Simple-Price-Prediction

This repository contains the application of five common machine learning models to forecast prices of the stocks from the [UCI Dow Jones Index Repository](https://archive.ics.uci.edu/ml/datasets/dow+jones+index#). The models are the following :

- Linear SVM
- RBF SVM
- Linear Regression
- Ridge Regression
- One Hidden Layer MLP

To see the process of finding the best model from all the types of models, execute the following file in R:

- `find_best_model.R`

You can check for each individual method what are the best parameters for them executing:


- `best_svm.R`
- `best_svm_rbf.R`
- `best_linear_regression.R`
- `best_ridge_regression.R`
- `best_mlp.R`

For each one of these also a visualization of the difference between the forecast and the real price will appear. All models are generated in `regression_models.R`.
