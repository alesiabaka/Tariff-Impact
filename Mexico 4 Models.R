# Load packages
library(readxl)
library(tibble)
library(tsibble)
library(forecast)
library(dplyr)
library(ggplot2)
library(forecastHybrid)
library(Metrics)
library(tsDyn)
library(caret)
library(lavaan)
library(randomForest)
library(xgboost)
library(e1071)
library(glmnet)

# Define global paths and constants
file_path <- "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Mexico/Quarterly Analysis/MexicoDataQ Final.xlsx"
macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Exchange_Rate")
tariff_vars <- c("Mexico_to_US_Tariff", "USA_to_Mexico_Tariff")
model_vars <- c(macro_vars, tariff_vars)
split_point <- NULL

library(zoo)

# Load and preprocess data
load_data <- function(file_path) {
  data <- readxl::read_excel(file_path)
  data <- data[, -1]  # Drop YQ
  names(data) <- gsub("[ /]", "_", names(data))
  split_point <<- floor(0.9 * nrow(data))
  list(
    train = data[1:split_point, ],
    test = data[(split_point + 1):nrow(data), ],
    full = data
  )
}

# Accuracy evaluation function
eval_accuracy <- function(actual, predicted) {
  binary_actual <- ifelse(actual > mean(actual, na.rm = TRUE), 1, 0)
  binary_pred <- ifelse(predicted > mean(actual, na.rm = TRUE), 1, 0)
  TP <- sum(binary_pred == 1 & binary_actual == 1)
  FP <- sum(binary_pred == 1 & binary_actual == 0)
  FN <- sum(binary_pred == 0 & binary_actual == 1)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  accuracy <- mean(binary_pred == binary_actual)
  c(
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    MAPE = mean(abs((actual - predicted) / actual)) * 100,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall
  )
}

# Plotting function for all models
plot_all_models <- function(data, models, title_prefix) {
  for (var in macro_vars) {
    df <- data.frame(
      Quarter = 1:nrow(data),
      Historical = data[[var]]
    )
    
    for (name in names(models)) {
      forecast_col <- rep(NA, nrow(data))
      shock_col <- rep(NA, nrow(data))
      forecast_col[(split_point + 1):nrow(data)] <- models[[name]]$forecast[[var]]
      shock_col[(split_point + 1):nrow(data)] <- models[[name]]$shock[[var]]
      df[[paste0(name, "_Forecast")]] <- forecast_col
      df[[paste0(name, "_Shock")]] <- shock_col
    }
    
    df_long <- tidyr::pivot_longer(df, -Quarter, names_to = "Series", values_to = "Value")
    print(
      ggplot2::ggplot(df_long, ggplot2::aes(x = Quarter, y = Value, color = Series)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste0(title_prefix, var), y = var, x = "Quarter") +
        ggplot2::theme_minimal()
    )
  }
}

# Plotting function for individual models
plot_individual_model <- function(data, model_output, model_name) {
  quarters <- seq(as.Date("1995-01-01"), by = "quarter", length.out = nrow(data) + nrow(model_output$forecast))
  for (var in macro_vars) {
    df <- data.frame(
      Date = zoo::as.yearqtr(quarters),
      Historical = c(data[[var]], rep(NA, nrow(model_output$forecast))),
      Forecast = c(rep(NA, nrow(data)), model_output$forecast[[var]]),
      Shocked = c(rep(NA, nrow(data)), model_output$shock[[var]])
    )
    df_long <- tidyr::pivot_longer(df, -Date, names_to = "Series", values_to = "Value")
    print(
      ggplot2::ggplot(df_long, ggplot2::aes(x = Date, y = Value, color = Series)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste(model_name, "Mexico Forecast vs Shock for", var), y = var, x = "Quarter") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::theme_minimal()
    )
  }
}






# Evaluation summary
summarize_accuracy <- function(test_data, model_outputs) {
  metrics <- list()
  for (name in names(model_outputs)) {
    acc <- sapply(macro_vars, function(var) {
      pred <- model_outputs[[name]]$forecast[[var]]
      actual <- test_data[[var]]
      eval_accuracy(actual, pred)
    })
    metrics[[name]] <- round(t(acc), 3)
  }
  metrics
}

# DFM model
run_dfm <- function(data_list) {
  macro_ts <- ts(data_list$train[, macro_vars], start = c(1995, 1), frequency = 4)
  pca <- prcomp(macro_ts, scale. = TRUE)
  factors <- ts(pca$x[, 1:2], start = start(macro_ts), frequency = 4)
  
  # Regress factors on tariffs
  tariff_train <- scale(data_list$train[, tariff_vars])
  factor_models <- lapply(1:2, function(i) lm(factors[, i] ~ tariff_train))
  
  # Predict future factors for both baseline and shock
  tariff_test <- scale(data_list$test[, tariff_vars], 
                       center = attr(tariff_train, "scaled:center"), 
                       scale = attr(tariff_train, "scaled:scale"))
  shock_test <- tariff_test * 1.10
  
  factor_preds <- sapply(factor_models, function(model) predict(model, newdata = as.data.frame(tariff_test)))[1:nrow(data_list$test), ]
  factor_shocks <- sapply(factor_models, function(model) predict(model, newdata = as.data.frame(shock_test)))[1:nrow(data_list$test), ]
  
  # Reconstruct forecasts
  dfm_models <- lapply(macro_vars, function(var) {
    lm(ts(data_list$train[[var]], start = c(1995, 1), frequency = 4) ~ factors)
  })
  names(dfm_models) <- macro_vars
  
  forecast_df <- as.data.frame(lapply(dfm_models, function(model) {
    coefs <- coef(model)
    intercept <- coefs[1]
    slopes <- coefs[-1]
    intercept + as.matrix(factor_preds) %*% slopes
  }))
  
  shock_df <- as.data.frame(lapply(dfm_models, function(model) {
    coefs <- coef(model)
    intercept <- coefs[1]
    slopes <- coefs[-1]
    intercept + as.matrix(factor_shocks) %*% slopes
  }))
  
  colnames(forecast_df) <- macro_vars
  colnames(shock_df) <- macro_vars
  list(forecast = forecast_df, shock = shock_df)
}
# XGBoost model
run_xgboost <- function(data_list) {
  library(xgboost)
  models <- lapply(macro_vars, function(var) {
    dtrain <- xgb.DMatrix(data = as.matrix(data_list$train[, tariff_vars]), label = data_list$train[[var]])
    xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
  })
  names(models) <- macro_vars
  forecast_df <- as.data.frame(sapply(macro_vars, function(var) {
    predict(models[[var]], as.matrix(data_list$test[, tariff_vars]))
  }))
  colnames(forecast_df) <- macro_vars
  shocked_test <- data_list$test
  shocked_test[, tariff_vars] <- shocked_test[, tariff_vars] * 1.10
  shock_df <- as.data.frame(sapply(macro_vars, function(var) {
    predict(models[[var]], as.matrix(shocked_test[, tariff_vars]))
  }))
  colnames(shock_df) <- macro_vars
  list(forecast = forecast_df, shock = shock_df)
}

# DSGE model
run_dsge <- function(data_list, tariff_shock = 0.10) {
  simulate_dsge <- function(initial_values, shock) {
    gdp <- inflation <- unemployment <- exports <- exchange_rate <- numeric(nrow(data_list$test))
    gdp[1] <- initial_values$GDP
    inflation[1] <- initial_values$Inflation
    unemployment[1] <- initial_values$Unemployment
    exports[1] <- initial_values$Exports
    exchange_rate[1] <- initial_values$Exchange_Rate
    for (t in 2:length(gdp)) {
      gdp[t] <- gdp[t - 1] * (1 - 0.03 * 2 * shock)
      output_gap <- (gdp[t] - gdp[t - 1]) / gdp[t - 1]
      inflation[t] <- inflation[t - 1] + 0.5 * output_gap
      unemployment[t] <- unemployment[t - 1] - 0.4 * output_gap
      exports[t] <- exports[t - 1] * (1 - 0.1 * 2 * shock)
      exchange_rate[t] <- exchange_rate[t - 1] * (1 - 0.02 * 2 * shock)
    }
    data.frame(GDP = gdp, Inflation = inflation, Unemployment = unemployment, Exports = exports, Exchange_Rate = exchange_rate)
  }
  
  initial_values <- as.list(data_list$test[1, macro_vars])
  initial_values <- lapply(initial_values, as.numeric)
  baseline_df <- simulate_dsge(initial_values, shock = 0.00)
  shock_df <- simulate_dsge(initial_values, shock = tariff_shock)
  list(forecast = baseline_df, shock = shock_df)
}


# BVAR model
run_bvar <- function(data_list) {
  library(vars)
  base_Y <- ts(data_list$train[, model_vars], start = c(1995, 1), frequency = 4)
  model <- VAR(base_Y, p = 2, type = "const")
  forecast <- predict(model, n.ahead = nrow(data_list$test))
  baseline_df <- as.data.frame(sapply(macro_vars, function(var) forecast$fcst[[var]][, "fcst"]))
  colnames(baseline_df) <- macro_vars
  shocked_test <- data_list$test
  shocked_test[, tariff_vars] <- shocked_test[, tariff_vars] * 1.10
  combined_shock <- rbind(data_list$train, shocked_test)
  model_shock <- VAR(ts(combined_shock[, model_vars], start = c(1995, 1), frequency = 4), p = 2, type = "const")
  forecast_shock <- predict(model_shock, n.ahead = nrow(data_list$test))
  shock_df <- as.data.frame(sapply(macro_vars, function(var) forecast_shock$fcst[[var]][, "fcst"]))
  colnames(shock_df) <- macro_vars
  list(forecast = baseline_df, shock = shock_df)
}

# Run all models and summarize
run_all_models <- function() {
  data_list <- load_data(file_path)
  results <- list(
    DFM = run_dfm(data_list),
    XGB = run_xgboost(data_list),
    DSGE = run_dsge(data_list),
    BVAR = run_bvar(data_list)
  )
  accuracy_metrics <- summarize_accuracy(data_list$test, results)
  print(accuracy_metrics)
  
  # Explicitly print grouped plots
  plot_all_models(data_list$full, results, title_prefix = "Mexico Forecast Comparison for ")
  
  # Explicitly print individual plots
  for (name in names(results)) {
    plot_individual_model(data_list$full, results[[name]], model_name = name)
  }
}



# Call run_all_models() to execute everything
run_all_models()

######################################Tunning ##########################################################
# Load and preprocess data
load_data <- function(file_path) {
  data <- readxl::read_excel(file_path)
  data <- data[, -1]  # Drop YQ
  names(data) <- gsub("[ /]", "_", names(data))
  split_point <<- floor(0.9 * nrow(data))
  list(
    train = data[1:split_point, ],
    test = data[(split_point + 1):nrow(data), ],
    full = data
  )
}

# Accuracy evaluation function
eval_accuracy <- function(actual, predicted) {
  binary_actual <- ifelse(actual > mean(actual, na.rm = TRUE), 1, 0)
  binary_pred <- ifelse(predicted > mean(actual, na.rm = TRUE), 1, 0)
  TP <- sum(binary_pred == 1 & binary_actual == 1)
  FP <- sum(binary_pred == 1 & binary_actual == 0)
  FN <- sum(binary_pred == 0 & binary_actual == 1)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  accuracy <- mean(binary_pred == binary_actual)
  c(
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    MAPE = mean(abs((actual - predicted) / actual)) * 100,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall
  )
}

# Plotting function for all models
plot_all_models <- function(data, models, title_prefix) {
  for (var in macro_vars) {
    df <- data.frame(
      Quarter = 1:nrow(data),
      Historical = data[[var]]
    )
    
    for (name in names(models)) {
      forecast_col <- rep(NA, nrow(data))
      shock_col <- rep(NA, nrow(data))
      forecast_col[(split_point + 1):nrow(data)] <- models[[name]]$forecast[[var]]
      shock_col[(split_point + 1):nrow(data)] <- models[[name]]$shock[[var]]
      df[[paste0(name, "_Forecast")]] <- forecast_col
      df[[paste0(name, "_Shock")]] <- shock_col
    }
    
    df_long <- tidyr::pivot_longer(df, -Quarter, names_to = "Series", values_to = "Value")
    print(
      ggplot2::ggplot(df_long, ggplot2::aes(x = Quarter, y = Value, color = Series)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste0(title_prefix, var), y = var, x = "Quarter") +
        ggplot2::theme_minimal()
    )
  }
}

# Plotting function for individual models
plot_individual_model <- function(data, model_output, model_name) {
  quarters <- seq(as.Date("1995-01-01"), by = "quarter", length.out = nrow(data) + nrow(model_output$forecast))
  for (var in macro_vars) {
    df <- data.frame(
      Date = zoo::as.yearqtr(quarters),
      Historical = c(data[[var]], rep(NA, nrow(model_output$forecast))),
      Forecast = c(rep(NA, nrow(data)), model_output$forecast[[var]]),
      Shocked = c(rep(NA, nrow(data)), model_output$shock[[var]])
    )
    df_long <- tidyr::pivot_longer(df, -Date, names_to = "Series", values_to = "Value")
    print(
      ggplot2::ggplot(df_long, ggplot2::aes(x = Date, y = Value, color = Series)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste(model_name, "Mexico Forecast vs Shock for", var), y = var, x = "Quarter") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        ggplot2::theme_minimal()
    )
  }
}






# Evaluation summary
summarize_accuracy <- function(test_data, model_outputs) {
  metrics <- list()
  for (name in names(model_outputs)) {
    acc <- sapply(macro_vars, function(var) {
      pred <- model_outputs[[name]]$forecast[[var]]
      actual <- test_data[[var]]
      eval_accuracy(actual, pred)
    })
    metrics[[name]] <- round(t(acc), 3)
  }
  metrics
}

# DFM model
# DFM model
run_dfm <- function(data_list) {
  macro_ts <- ts(data_list$train[, macro_vars], start = c(1995, 1), frequency = 4)
  pca <- prcomp(macro_ts, scale. = TRUE)
  factors <- ts(pca$x[, 1:5], start = start(macro_ts), frequency = 4)
  
  # Regress factors on tariffs
  tariff_train <- scale(data_list$train[, tariff_vars])
  factor_models <- lapply(1:5, function(i) lm(factors[, i] ~ tariff_train))
  
  # Predict future factors for both baseline and shock (fixed)
  tariff_test <- scale(data_list$test[, tariff_vars], 
                       center = attr(tariff_train, "scaled:center"), 
                       scale = attr(tariff_train, "scaled:scale"))
  shock_test <- tariff_test * 1.10
  factor_preds <- sapply(factor_models, function(model) predict(model, newdata = as.data.frame(tariff_test)))[1:nrow(data_list$test), ]
  factor_shocks <- sapply(factor_models, function(model) predict(model, newdata = as.data.frame(shock_test)))[1:nrow(data_list$test), ]
  
  # Reconstruct forecasts
  dfm_models <- lapply(macro_vars, function(var) {
    lm(ts(data_list$train[[var]], start = c(1995, 1), frequency = 4) ~ factors)
  })
  names(dfm_models) <- macro_vars
  
  forecast_df <- as.data.frame(lapply(dfm_models, function(model) {
    coefs <- coef(model)
    intercept <- coefs[1]
    slopes <- coefs[-1]
    intercept + as.matrix(factor_preds) %*% slopes
  }))
  
  shock_df <- as.data.frame(lapply(dfm_models, function(model) {
    coefs <- coef(model)
    intercept <- coefs[1]
    slopes <- coefs[-1]
    intercept + as.matrix(factor_shocks) %*% slopes
  }))
  
  colnames(forecast_df) <- macro_vars
  colnames(shock_df) <- macro_vars
  list(forecast = forecast_df, shock = shock_df)
}

# XGBoost model
run_xgboost <- function(data_list) {
  library(xgboost)
  models <- lapply(macro_vars, function(var) {
    dtrain <- xgb.DMatrix(data = as.matrix(data_list$train[, tariff_vars]), label = data_list$train[[var]])
    xgboost(data = dtrain, nrounds = 125, objective = "reg:squarederror", verbose = 0)
  })
  names(models) <- macro_vars
  forecast_df <- as.data.frame(sapply(macro_vars, function(var) {
    predict(models[[var]], as.matrix(data_list$test[, tariff_vars]))
  }))
  colnames(forecast_df) <- macro_vars
  shocked_test <- data_list$test
  shocked_test[, tariff_vars] <- shocked_test[, tariff_vars] * 1.10
  shock_df <- as.data.frame(sapply(macro_vars, function(var) {
    predict(models[[var]], as.matrix(shocked_test[, tariff_vars]))
  }))
  colnames(shock_df) <- macro_vars
  list(forecast = forecast_df, shock = shock_df)
}

# DSGE model
run_dsge <- function(data_list, elasticity_params_1, elasticity_params_2, us_to_mexico_shock = 0.10, mexico_to_us_shock = 0.10) {
  macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Exchange_Rate")
  
  # Prepare initial values from the first row of the test set
  initial_values <- as.list(data_list$test[1, macro_vars])
  initial_values <- lapply(initial_values, as.numeric)
  forecast_horizon <- nrow(data_list$test)
  
  # Simulate without and with shock
  simulate_dsge <- function(initial_values, elasticity_params_1, elasticity_params_2, us_to_mexico_shock, mexico_to_us_shock, forecast_horizon) {
    gdp <- inflation <- unemployment <- exports <- exchange_rate <- numeric(forecast_horizon)
    gdp[1] <- initial_values$GDP
    inflation[1] <- initial_values$Inflation
    unemployment[1] <- initial_values$Unemployment
    exports[1] <- initial_values$Exports
    exchange_rate[1] <- initial_values$Exchange_Rate
    
    for (t in 2:forecast_horizon) {
      # Apply shocks with custom elasticities
      gdp[t] <- gdp[t - 1] * (1 + elasticity_params_1$GDP * us_to_mexico_shock + elasticity_params_2$GDP * mexico_to_us_shock)
      
      # Calculate output gap
      output_gap <- (gdp[t] - gdp[t - 1]) / gdp[t - 1]
      
      # Use custom elasticities for inflation and unemployment
      inflation[t] <- inflation[t - 1] + (elasticity_params_1$Inflation * us_to_mexico_shock + elasticity_params_2$Inflation * mexico_to_us_shock) * output_gap
      unemployment[t] <- unemployment[t - 1] - (elasticity_params_1$Unemployment * us_to_mexico_shock + elasticity_params_2$Unemployment * mexico_to_us_shock) * output_gap
      
      # Use custom elasticities for exports
      exports[t] <- exports[t - 1] * (1 + elasticity_params_1$Exports * us_to_mexico_shock + elasticity_params_2$Exports * mexico_to_us_shock)
      
      # Use custom elasticities for the exchange rate
      exchange_rate[t] <- exchange_rate[t - 1] / (1 + elasticity_params_1$Exchange_Rate * us_to_mexico_shock + elasticity_params_2$Exchange_Rate * mexico_to_us_shock)
    }
    
    data.frame(GDP = gdp, Inflation = inflation, Unemployment = unemployment, Exports = exports, Exchange_Rate = exchange_rate)
  }
  
  # Generate forecasts
  baseline_df <- simulate_dsge(initial_values, list(GDP = 1, Inflation = 1, Unemployment = 1, Exports = 1, Exchange_Rate = 1), list(GDP = 1, Inflation = 1, Unemployment = 1, Exports = 1, Exchange_Rate = 1), 0, 0, forecast_horizon)
  shock <- simulate_dsge(initial_values, elasticity_params_1, elasticity_params_2, us_to_mexico_shock, mexico_to_us_shock, forecast_horizon)
  
  list(
    forecast = baseline_df,
    shock = shock
  )
}

# BVAR model
run_bvar <- function(data_list) {
  library(vars)
  base_Y <- ts(data_list$train[, model_vars], start = c(1995, 1), frequency = 4)
  model <- VAR(base_Y, p = 4, type = "const")
  forecast <- predict(model, n.ahead = nrow(data_list$test))
  baseline_df <- as.data.frame(sapply(macro_vars, function(var) forecast$fcst[[var]][, "fcst"]))
  colnames(baseline_df) <- macro_vars
  shocked_test <- data_list$test
  shocked_test[, tariff_vars] <- shocked_test[, tariff_vars] * 1.10
  combined_shock <- rbind(data_list$train, shocked_test)
  model_shock <- VAR(ts(combined_shock[, model_vars], start = c(1995, 1), frequency = 4), p = 4, type = "const")
  forecast_shock <- predict(model_shock, n.ahead = nrow(data_list$test))
  shock_df <- as.data.frame(sapply(macro_vars, function(var) forecast_shock$fcst[[var]][, "fcst"]))
  colnames(shock_df) <- macro_vars
  list(forecast = baseline_df, shock = shock_df)
}


# Run all models and summarize
run_all_models <- function(file_path) {
  data_list <- load_data(file_path)
  macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Exchange_Rate")
  
  # Define elasticity parameters
  elasticity_params_1 <- list(
    GDP = -0.28,
    Inflation = 1.50,
    Unemployment = 0.02,
    Exports = -0.63,
    Exchange_Rate = -0.45
  )
  
  elasticity_params_2 <- list(
    GDP = -0.25,
    Inflation = 0.56,
    Unemployment = -0.08,
    Exports = - 0.66,
    Exchange_Rate = -0.55
  )
  
  results <- list(
    DFM = run_dfm(data_list),
    XGB = run_xgboost(data_list),
    DSGE = run_dsge(data_list, 
                    elasticity_params_1 = elasticity_params_1, 
                    elasticity_params_2 = elasticity_params_2, 
                    us_to_mexico_shock = 0.10, 
                    mexico_to_us_shock = 0.10),
    BVAR = run_bvar(data_list)
  )
  
  # Print accuracy metrics
  accuracy_metrics <- summarize_accuracy(data_list$test, results)
  print(accuracy_metrics)
  
  # Plot grouped models
  plot_all_models(data_list$full, results, title_prefix = "Mexico Forecast Comparison for ")
  
  # Plot individual models
  for (name in names(results)) {
    plot_individual_model(data_list$full, results[[name]], model_name = name)
  }
}

# Call run_all_models() to execute everything
run_all_models(file_path)




