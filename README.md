
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DairyPredictR

<!-- badges: start -->

<!-- badges: end -->

## Overview

**DairyPredictR** is an R package for machine learning-based prediction
and comparative evaluation of dairy milk yield.

The package provides a complete workflow for:

- importing dairy production data
- data cleaning and preparation
- selecting target and predictor variables
- creating training and testing datasets
- fitting machine learning models
- evaluating model performance
- comparing multiple models
- identifying the best-performing model
- explaining model variable importance
- generating predictions for new observations
- saving and loading fitted models
- visualizing model performance

The supported machine learning methods are:

1.  Linear Regression
2.  Random Forest
3.  Gradient Boosting Machine (GBM)
4.  Support Vector Machine (SVM)
5.  Artificial Neural Network (ANN)

## Installation

### From a local source package

A locally built source package can be installed using:

``` r
install.packages(
  "DairyPredictR_1.0.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

### Development version

Once the GitHub repository is available, the development version can be
installed using:

``` r
remotes::install_github("rakeshnehra1698/DairyPredictR")
```

Replace `YOUR-GITHUB-USERNAME` with the actual GitHub repository owner.

## Basic workflow

``` text
Dairy data
    |
    v
read_dairy()
    |
    v
clean_data()
    |
    v
split_data()
    |
    v
fit_model()
    |
    v
evaluate()
    |
    v
compare_models()
    |
    v
Best model
    |
    +------------------+
    |                  |
    v                  v
explain()          predict()
    |                  |
    v                  v
Variable          New milk-yield
importance         predictions
```

## Read dairy data

DairyPredictR supports CSV and Excel files.

``` r
library(DairyPredictR)

data <- read_dairy("milk_data.csv")
```

For Excel files:

``` r
data <- read_dairy("milk_data.xlsx")
```

## Clean data

The data can be cleaned before modeling:

``` r
cleaned_data <- clean_data(data)
```

## Split the data

Create training and testing datasets:

``` r
split <- split_data(
  data = cleaned_data,
  target = "MilkYield305",
  train_size = 0.80,
  seed = 123
)
```

The resulting object contains:

``` r
split$train
split$test
```

## Fit a machine learning model

Specify the target variable and predictor variables:

``` r
predictors <- c(
  "TD1",
  "TD2",
  "TD3"
)
```

### Linear Regression

``` r
linear_model <- fit_model(
  data = split$train,
  target = "MilkYield305",
  predictors = predictors,
  method = "linear"
)
```

### Random Forest

``` r
rf_model <- fit_model(
  data = split$train,
  target = "MilkYield305",
  predictors = predictors,
  method = "rf"
)
```

### Gradient Boosting Machine

``` r
gbm_model <- fit_model(
  data = split$train,
  target = "MilkYield305",
  predictors = predictors,
  method = "gbm"
)
```

### Support Vector Machine

``` r
svm_model <- fit_model(
  data = split$train,
  target = "MilkYield305",
  predictors = predictors,
  method = "svm"
)
```

### Artificial Neural Network

``` r
ann_model <- fit_model(
  data = split$train,
  target = "MilkYield305",
  predictors = predictors,
  method = "ann"
)
```

## Evaluate model performance

A fitted model can be evaluated using the testing dataset:

``` r
evaluate(
  object = rf_model,
  newdata = split$test
)
```

The evaluation provides:

- R²
- RMSE
- MAE
- MAPE
- Bias

## Compare multiple models

DairyPredictR can train and evaluate all supported models using the same
training/testing split and selected predictors:

``` r
comparison <- compare_models(
  data = split,
  target = "MilkYield305",
  predictors = predictors
)

comparison
```

The comparison table contains:

``` text
Model
R2
RMSE
MAE
MAPE
Bias
```

## Model performance visualization

Performance metrics can be visualized using:

``` r
plot_performance(
  comparison,
  metric = "R2"
)
```

Other available metrics are:

``` r
plot_performance(comparison, metric = "RMSE")
plot_performance(comparison, metric = "MAE")
plot_performance(comparison, metric = "MAPE")
plot_performance(comparison, metric = "Bias")
```

All available metrics can be generated using:

``` r
plots <- plot_performance(
  comparison,
  metric = "all"
)
```

## Model explainability

Variable importance can be obtained for supported fitted models using:

``` r
explain(rf_model)
```

The resulting table identifies the importance of predictor variables
according to the fitted model’s explainability method.

## Generate predictions

Predictions for new observations can be generated using the standard R
`predict()` interface:

``` r
predictions <- predict(
  rf_model,
  split$test
)

predictions
```

## Save and load models

A fitted model can be saved:

``` r
save_model(
  rf_model,
  "rf_model.rds"
)
```

The saved model can subsequently be loaded:

``` r
loaded_model <- load_model(
  "rf_model.rds"
)
```

Predictions can then be generated from the loaded model:

``` r
predict(
  loaded_model,
  split$test
)
```

## Main functions

| Function             | Purpose                              |
|:---------------------|:-------------------------------------|
| `read_dairy()`       | Import CSV or Excel dairy data       |
| `clean_data()`       | Clean and prepare dairy data         |
| `split_data()`       | Create training and testing datasets |
| `fit_model()`        | Fit a machine learning model         |
| `evaluate()`         | Evaluate model performance           |
| `compare_models()`   | Compare supported models             |
| `plot_performance()` | Visualize model performance          |
| `explain()`          | Obtain model variable importance     |
| `predict()`          | Generate predictions                 |
| `save_model()`       | Save a fitted model                  |
| `load_model()`       | Load a saved model                   |

## Supported models

| Method                    | Code       |
|:--------------------------|:-----------|
| Linear Regression         | `"linear"` |
| Random Forest             | `"rf"`     |
| Gradient Boosting Machine | `"gbm"`    |
| Support Vector Machine    | `"svm"`    |
| Artificial Neural Network | `"ann"`    |

## Shiny application

DairyPredictR is accompanied by a Shiny-based web application,
**DairyPredictR-Web**, which provides a graphical interface for the
modeling workflow.

The application allows users to:

- upload dairy datasets
- select target and predictor variables
- configure training/testing data
- fit machine learning models
- compare model performance
- identify the recommended model
- generate predictions
- examine model explainability
- visualize model performance

The Shiny application uses the `DairyPredictR` package for the
underlying modeling functions.

## Reproducibility

For reproducible model comparisons, specify a random seed when creating
the training/testing split:

``` r
split <- split_data(
  data = cleaned_data,
  target = "MilkYield305",
  train_size = 0.80,
  seed = 123
)
```

Using the same data, predictors, training proportion, and seed allows
the analysis to be reproduced under the same computational environment.

## Citation

If you use DairyPredictR in research, please cite the package and its
associated publication when available.

Citation information will be provided with the release documentation.

## Authors

**Rakesh Nehra** — Author and maintainer

**Pallavi Choudhary** — Author

## License

DairyPredictR is released under the **GPL (\>= 3)** license.
