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
file_path <- "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/EU/Quarterly Analysis/EU DataQ Final.xlsx"
macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Euro_Dollar")
tariff_vars <- c("EU_to_USA_Tariff", "USA_to_EU_Tariff")
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
      ggplot2::ggplot(df_long, ggplot2::aes(x = Quarter, y = Value, color = Series, linetype)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste0(title_prefix, var), y = var, x = "Quarter") +
        ggplot2::theme_minimal()
    )
  }
}

# Plotting function for individual models
plot_individual_model <- function(data, model_output, model_name) {
  # Set the full historical range
  historical_length <- nrow(data)
  forecast_length <- nrow(model_output$forecast)
  
  # Generate the correct date range
  full_quarters <- seq(as.Date("1995-01-01"), by = "quarter", length.out = historical_length + forecast_length)
  
  # Create the data frame for plotting
  df <- data.frame(
    Date = zoo::as.yearqtr(full_quarters),
    Historical = c(as.numeric(data[[model_name]]), rep(NA, forecast_length)),
    Forecast = c(rep(NA, historical_length), as.numeric(model_output$forecast[[model_name]])),
    Shocked = c(rep(NA, historical_length), as.numeric(model_output$shock[[model_name]]))
  )
  
  # Ensure the data frame lengths match
  if (nrow(df) != (historical_length + forecast_length)) {
    stop("Mismatch in row counts for Date, Historical, Forecast, and Shocked data")
  }
  
  # Convert to long format
  df_long <- tidyr::pivot_longer(df, -Date, names_to = "Series", values_to = "Value")
  
  # Plot the data
  print(
    ggplot2::ggplot(df_long, ggplot2::aes(x = Date, y = Value, color = Series, linetype = Series)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = paste(model_name, "Forecast vs Shock for", model_name), y = model_name, x = "Quarter") +
      ggplot2::scale_x_yearqtr(breaks = zoo::as.yearqtr(seq(as.Date("1995-01-01"), as.Date("2029-01-01"), by = "2 years"))) +
      ggplot2::theme_minimal()
  )
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
    gdp <- inflation <- unemployment <- exports <- euro_dollar <- numeric(nrow(data_list$test))
    gdp[1] <- initial_values$GDP
    inflation[1] <- initial_values$Inflation
    unemployment[1] <- initial_values$Unemployment
    exports[1] <- initial_values$Exports
    euro_dollar[1] <- initial_values$Euro_Dollar
    for (t in 2:length(gdp)) {
      gdp[t] <- gdp[t - 1] * (1 - 0.03 * 2 * shock)
      output_gap <- (gdp[t] - gdp[t - 1]) / gdp[t - 1]
      inflation[t] <- inflation[t - 1] + 0.5 * output_gap
      unemployment[t] <- unemployment[t - 1] - 0.4 * output_gap
      exports[t] <- exports[t - 1] * (1 - 0.1 * 2 * shock)
      euro_dollar[t] <- euro_dollar[t - 1] * (1 - 0.02 * 2 * shock)
    }
    data.frame(GDP = gdp, Inflation = inflation, Unemployment = unemployment, Exports = exports, Euro_Dollar = euro_dollar)
  }
  
  initial_values <- data_list$test[1, macro_vars]
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
  
  message("Running DFM...")
  dfm_result <- tryCatch(run_dfm(data_list), error = function(e) { message("DFM error:", e); NULL })
  
  message("Running XGBoost...")
  xgb_result <- tryCatch(run_xgboost(data_list), error = function(e) { message("XGBoost error:", e); NULL })
  
  message("Running DSGE...")
  dsge_result <- tryCatch(run_dsge(data_list), error = function(e) { message("DSGE error:", e); NULL })
  
  message("Running BVAR...")
  bvar_result <- tryCatch(run_bvar(data_list), error = function(e) { message("BVAR error:", e); NULL })
  
  results <- list(
    DFM = dfm_result,
    XGB = xgb_result,
    DSGE = dsge_result,
    BVAR = bvar_result
  )
  
  results <- Filter(Negate(is.null), results)
  
  if (length(results) == 0) {
    stop("No models were successfully run.")
  }
  
  accuracy_metrics <- summarize_accuracy(data_list$test, results)
  print(accuracy_metrics)
  
  # Grouped plot
  plot_all_models(data_list$full, results, title_prefix = "Forecast Comparison for ")
  
  # Individual plots
  for (name in names(results)) {
    plot_individual_model(data_list$full, results[[name]], model_name = name)
  }
}




# Call run_all_models() to execute everything
run_all_models()

###############################Tunning #######################################################

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
  # Ensure actual and predicted are numeric
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
  # Remove NA values
  valid_indices <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid_indices]
  predicted <- predicted[valid_indices]
  
  # Binary conversion for precision and recall
  binary_actual <- ifelse(actual > mean(actual, na.rm = TRUE), 1, 0)
  binary_pred <- ifelse(predicted > mean(actual, na.rm = TRUE), 1, 0)
  
  TP <- sum(binary_pred == 1 & binary_actual == 1)
  FP <- sum(binary_pred == 1 & binary_actual == 0)
  FN <- sum(binary_pred == 0 & binary_actual == 1)
  
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  accuracy <- mean(binary_pred == binary_actual)
  
  # Calculate metrics
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
        ggplot2::labs(title = paste(model_name, "EU Forecast vs Shock for", var), y = var, x = "Quarter") +
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
  factors <- ts(pca$x[, 1:5], start = start(macro_ts), frequency = 4)
  
  # Regress factors on tariffs
  tariff_train <- scale(data_list$train[, tariff_vars])
  factor_models <- lapply(1:5, function(i) lm(factors[, i] ~ tariff_train))
  
  # Predict future factors for both baseline and shock
  tariff_test <- scale(data_list$test[, tariff_vars], 
                       center = attr(tariff_train, "scaled:center"), 
                       scale = attr(tariff_train, "scaled:scale"))
  shock_test <- tariff_test * 1.10
  
  # Regress factors on tariffs
  factor_models <- lapply(1:5, function(i) lm(factors[, i] ~ tariff_train))
  
  # Predict future factors for both baseline and shock
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


# DSGE model
run_dsge <- function(data_list, elasticity_params_1, elasticity_params_2, us_to_eu_shock = 0.10, eu_to_us_shock = 0.10) {
  macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Euro_Dollar")
  
  # Prepare initial values from the first row of the test set
  initial_values <- as.list(data_list$test[1, macro_vars])
  initial_values <- lapply(initial_values, as.numeric)
  forecast_horizon <- nrow(data_list$test)
  
  # Simulate without and with shock
  simulate_dsge <- function(initial_values, elasticity_params_1, elasticity_params_2, us_to_eu_shock, eu_to_us_shock, forecast_horizon) {
    gdp <- inflation <- unemployment <- exports <- euro_dollar <- numeric(forecast_horizon)
    gdp[1] <- initial_values$GDP
    inflation[1] <- initial_values$Inflation
    unemployment[1] <- initial_values$Unemployment
    exports[1] <- initial_values$Exports
    euro_dollar[1] <- initial_values$Euro_Dollar
    
    for (t in 2:forecast_horizon) {
      # Apply shocks with custom elasticities
      gdp[t] <- gdp[t - 1] * (1 + elasticity_params_1$GDP * eu_to_us_shock + elasticity_params_2$GDP * us_to_eu_shock)
      
      # Calculate output gap
      output_gap <- (gdp[t] - gdp[t - 1]) / gdp[t - 1]
      
      # Use custom elasticities for inflation and unemployment
      inflation[t] <- inflation[t - 1] + (elasticity_params_1$Inflation * us_to_eu_shock + elasticity_params_2$Inflation * eu_to_us_shock) * output_gap
      unemployment[t] <- unemployment[t - 1] - (elasticity_params_1$Unemployment * us_to_eu_shock + elasticity_params_2$Unemployment * eu_to_us_shock) * output_gap
      
      # Use custom elasticities for exports
      exports[t] <- exports[t - 1] * (1 + elasticity_params_1$Exports * us_to_eu_shock + elasticity_params_2$Exports * eu_to_us_shock)
      
      # Use custom elasticities for the exchange rate
      euro_dollar[t] <-  euro_dollar[t - 1] / (1 + elasticity_params_1$ Euro_Dollar * us_to_eu_shock + elasticity_params_2$ Euro_Dollar * eu_to_us_shock)
    }
    
    data.frame(GDP = gdp, Inflation = inflation, Unemployment = unemployment, Exports = exports, Euro_Dollar = euro_dollar)
  }
  
  
  
  # Generate forecasts
  baseline_df <- simulate_dsge(initial_values, list(GDP = 1, Inflation = 1, Unemployment = 1, Exports = 1, Euro_Dollar = 1), list(GDP = 1, Inflation = 1, Unemployment = 1, Exports = 1, Euro_Dollar = 1), 0, 0, forecast_horizon)
  shock <- simulate_dsge(initial_values, elasticity_params_1, elasticity_params_2, us_to_eu_shock, eu_to_us_shock, forecast_horizon)
  
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
  macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Euro_Dollar")
  
  # Define elasticity parameters
  elasticity_params_1 <- list(
    GDP = -0.37,
    Inflation = -2.71,
    Unemployment = 0.638,
    Exports = 1.28,
    Euro_Dollar = -0.01
  )
  
  elasticity_params_2 <- list(
    GDP = 0.06,
    Inflation = 2.11,
    Unemployment = -0.53,
    Exports = 0.23,
    Euro_Dollar = -0.11
  )
  
  results <- list(
    DFM = run_dfm(data_list),
    XGB = run_xgboost(data_list),
    DSGE = run_dsge(data_list, 
                    elasticity_params_1 = elasticity_params_1, 
                    elasticity_params_2 = elasticity_params_2, 
                    us_to_eu_shock = 0.10, 
                    eu_to_us_shock = 0.10),
    BVAR = run_bvar(data_list)
  )
  
  # Print accuracy metrics
  accuracy_metrics <- summarize_accuracy(data_list$test, results)
  print(accuracy_metrics)
  
  # Plot grouped models
  plot_all_models(data_list$full, results, title_prefix = "EU Forecast Comparison for ")
  
  # Plot individual models
  for (name in names(results)) {
    plot_individual_model(data_list$full, results[[name]], model_name = name)
  }
}

# Call run_all_models() to execute everything
run_all_models(file_path)



