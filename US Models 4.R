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
library(scales)

# Define global paths and constants
file_path <- "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/USA/Quarterly Analysis/US DataQ Final.xlsx"
macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Imports")
tariff_vars <- c("US_to_Canada",	"US_to_China",	"US_to_Mexico",	"China_to_US_Tariff",	"Canada_to_US_Tariff",	"Mexico_to_US_Tariff","EU_to_US_Tariff",	"US_to_EU")
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
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
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
      Historical = as.numeric(data[[var]])
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
        ggplot2::labs(title = paste(title_prefix, var), y = var, x = "Quarter") +
        ggplot2::scale_x_continuous(breaks = seq(1, nrow(data), by = 8)) +
        ggplot2::theme_minimal()
    )
  }
}



# Plotting function for individual models
plot_individual_model <- function(data, model_output, model_name, model_type) {
  # Ensure the variable exists in the dataset
  if (!(model_name %in% names(data))) {
    stop(paste("Variable", model_name, "not found in the data. Please check the column names."))
  }
  
  # Extract the split point and calculate the correct date ranges
  historical_length <- split_point
  forecast_length <- nrow(model_output$forecast)
  
  # Create the correct date sequences
  start_date <- as.Date("1995-01-01")
  end_date <- as.Date("2025-03-31")
  historical_dates <- seq(start_date, by = "quarter", length.out = historical_length)
  forecast_dates <- seq(end_date + 1, by = "quarter", length.out = forecast_length)
  
  # Combine historical and forecast dates
  full_dates <- c(historical_dates, forecast_dates)
  
  # Create the data frame for plotting
  df <- data.frame(
    Date = full_dates,
    Historical = c(as.numeric(data[[model_name]][1:historical_length]), rep(NA, forecast_length)),
    Forecast = c(rep(NA, historical_length), as.numeric(model_output$forecast[[model_name]])),
    Shocked = c(rep(NA, historical_length), as.numeric(model_output$shock[[model_name]]))
  )
  
  # Convert to long format for easier plotting
  df_long <- tidyr::pivot_longer(df, -Date, names_to = "Series", values_to = "Value")
  
  # Add model type to the series names for clarity
  df_long$Series <- paste0(model_type, " - ", df_long$Series)
  
  # Plot the data
  print(
    ggplot2::ggplot(df_long, ggplot2::aes(x = Date, y = Value, color = Series)) +
      ggplot2::geom_line(size = 0.8) +
      ggplot2::labs(title = paste(model_type, model_name, "Forecast vs Shock"), 
                    y = model_name, 
                    x = "Year") +
      ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
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
  # Extract the training data
  macro_ts <- ts(data_list$train[, macro_vars], start = c(1995, 1), frequency = 4)
  
  # Perform PCA for dimensionality reduction
  pca <- prcomp(macro_ts, scale. = TRUE)
  factors <- ts(pca$x[, 1:2], start = start(macro_ts), frequency = 4)
  
  # Regress factors on tariffs
  tariff_train <- scale(data_list$train[, tariff_vars])
  factor_models <- lapply(1:2, function(i) lm(factors[, i] ~ tariff_train))
  
  # Predict future factors for the exact test length
  tariff_test <- scale(data_list$test[, tariff_vars], center = attr(tariff_train, "scaled:center"), scale = attr(tariff_train, "scaled:scale"))
  n_test_periods <- nrow(data_list$test)
  
  # Limit the forecast horizon to the same length as the test set
  factor_preds <- sapply(factor_models, function(model) predict(model, newdata = as.data.frame(tariff_test)))
  shock_test <- tariff_test * 1.10
  factor_shocks <- sapply(factor_models, function(model) predict(model, newdata = as.data.frame(shock_test)))
  
  # Reconstruct forecasts
  dfm_models <- lapply(macro_vars, function(var) {
    lm(ts(data_list$train[[var]], start = c(1995, 1), frequency = 4) ~ factors)
  })
  names(dfm_models) <- macro_vars
  
  # Create forecast and shock dataframes with correct length
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
  
  # Set column names to match the macro variables
  colnames(forecast_df) <- macro_vars
  colnames(shock_df) <- macro_vars
  
  # Return as a list for consistency with other models
  list(forecast = forecast_df[1:n_test_periods, ], shock = shock_df[1:n_test_periods, ])
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
  macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Imports")
  
  simulate_dsge <- function(initial_values, shock) {
    n_quarters <- nrow(data_list$test)
    gdp <- inflation <- unemployment <- exports <- imports <- numeric(n_quarters)
    
    gdp[1] <- initial_values["GDP"]
    inflation[1] <- initial_values["Inflation"]
    unemployment[1] <- initial_values["Unemployment"]
    exports[1] <- initial_values["Exports"]
    imports [1] <- initial_values["Imports"]
    
    for (t in 2:n_quarters) {
      gdp[t] <- gdp[t - 1] * (1 - 0.03 * 2 * shock)
      output_gap <- (gdp[t] - gdp[t - 1]) / gdp[t - 1]
      inflation[t] <- inflation[t - 1] + 0.5 * output_gap
      unemployment[t] <- unemployment[t - 1] - 0.4 * output_gap
      exports[t] <- exports[t - 1] * (1 - 0.1 * 2 * shock)
      imports[t] <- imports[t - 1] * (1 - 0.02 * 2 * shock)
    }
    
    data.frame(
      GDP = gdp,
      Inflation = inflation,
      Unemployment = unemployment,
      Exports = exports,
      Imports=imports
      
    )
  }
  
  initial_row <- data_list$test[1, macro_vars, drop = FALSE]
  initial_values <- as.numeric(initial_row[1, ])
  names(initial_values) <- macro_vars
  
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
  plot_all_models(data_list$full, results, title_prefix = "United States Forecast Comparison for ")
  
  # Explicitly print individual plots
  for (var in macro_vars) {
    for (model_type in names(results)) {
      plot_individual_model(data_list$full, results[[model_type]], model_name = var, model_type = model_type)
    }
  }
}

# Call run_all_models() to execute everything
run_all_models()

######################################### Tunning #####################################################

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

# Accuracy Evaluation
eval_accuracy <- function(actual, predicted) {
  # Convert to numeric, removing any non-numeric characters
  actual <- as.numeric(as.character(actual))
  predicted <- as.numeric(as.character(predicted))
  
  # Remove NA values to prevent errors
  valid_indices <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid_indices]
  predicted <- predicted[valid_indices]
  
  # Handle case where no valid indices are left
  if (length(actual) == 0 || length(predicted) == 0) {
    warning("No valid numeric data for accuracy evaluation")
    return(rep(NA, 7))  # Return NA for each metric if data is empty
  }
  
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
    # Convert Historical to numeric, force NA on non-convertible values
    historical_data <- as.numeric(as.character(data[[var]]))
    
    # Handle NA conversion issues
    if (any(is.na(historical_data))) {
      warning(paste("Non-numeric values found in", var, "replaced with NA"))
    }
    
    # Create the base dataframe for plotting
    df <- data.frame(
      Quarter = 1:nrow(data),
      Historical = historical_data
    )
    
    # Add forecast and shock columns
    for (name in names(models)) {
      forecast_len <- nrow(models[[name]]$forecast)
      shock_len <- nrow(models[[name]]$shock)
      
      # Ensure correct lengths for forecast and shock columns
      if (forecast_len != (nrow(data) - split_point)) {
        stop(paste("Mismatch in forecast length for model", name, "and variable", var))
      }
      
      forecast_col <- rep(NA, nrow(data))
      shock_col <- rep(NA, nrow(data))
      forecast_col[(split_point + 1):nrow(data)] <- models[[name]]$forecast[[var]]
      shock_col[(split_point + 1):nrow(data)] <- models[[name]]$shock[[var]]
      
      df[[paste0(name, "_Forecast")]] <- forecast_col
      df[[paste0(name, "_Shock")]] <- shock_col
    }
    
    # Ensure all columns are numeric before pivoting
    df[] <- lapply(df, function(col) as.numeric(as.character(col)))
    
    # Pivot longer for ggplot
    df_long <- tidyr::pivot_longer(df, -Quarter, names_to = "Series", values_to = "Value")
    
    # Plot the data
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
        ggplot2::labs(title = paste(model_name, "United States Forecast vs Shock for", var), y = var, x = "Quarter") +
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


#DFM Model
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

# DSGE model with extracted elasticities and full tariff shocks
run_dsge <- function(data_list, 
                     us_to_china_shock = 0.10, 
                     china_to_us_shock = 0.10, 
                     us_to_eu_shock = 0.10, 
                     us_to_canada_shock = 0.10, 
                     us_to_mexico_shock = 0.10,
                     eu_to_us_shock = 0.10,
                     canada_to_us_shock = 0.10,
                     mexico_to_us_shock = 0.10) {
  
  macro_vars <- c("GDP", "Inflation", "Unemployment", "Exports", "Imports")
  
  # Prepare initial values from the first row of the test set
  initial_values <- as.list(data_list$test[1, macro_vars])
  initial_values <- lapply(initial_values, as.numeric)
  forecast_horizon <- nrow(data_list$test)
  
  # Elasticities based on OLS results
  elasticities <- list(
    GDP = list(
      US_to_EU = 0.76240,
      US_to_China = -0.95541,
      US_to_Canada = 0.14289,
      US_to_Mexico = -0.22324,
      China_to_US = 0.02155,
      Canada_to_US = -0.11660,
      EU_to_US = -0.58571,
      Mexico_to_US = -0.43444
    ),
    Unemployment = list(
      US_to_EU = -0.86345,
      US_to_China = 1.33480,
      US_to_Canada = -1.33479,
      US_to_Mexico = 1.17243,
      China_to_US = -0.48858,
      Canada_to_US = -0.21291,
      EU_to_US = 0.63068,
      Mexico_to_US = 0.28416
    ),
    Exports = list(
      US_to_EU = 0.41691,
      US_to_China = -0.88495,
      US_to_Canada = -0.04775,
      US_to_Mexico = -0.23453,
      China_to_US = 0.07289,
      Canada_to_US = -0.18390,
      EU_to_US = -0.24929,
      Mexico_to_US = -0.52189
    ),
    Imports = list(
      US_to_EU = 0.56075,
      US_to_China = -0.98538,
      US_to_Canada = 0.12306,
      US_to_Mexico = -0.31112,
      China_to_US = -0.01274,
      Canada_to_US = -0.17300,
      EU_to_US = -0.55539,
      Mexico_to_US = -0.37781
    ),
    Inflation = list(
      US_to_EU = 1.20493,
      US_to_China = -0.31125,
      US_to_Canada = 0.63554,
      US_to_Mexico = -0.91671,
      China_to_US = -0.24748,
      Canada_to_US = -0.32704,
      EU_to_US = -0.38766,
      Mexico_to_US = 0.04395
    )
  )
  
  # Simulate without and with shocks
  simulate_dsge <- function(initial_values, elasticities, 
                            us_to_china_shock, china_to_us_shock, 
                            us_to_eu_shock, us_to_canada_shock, us_to_mexico_shock,
                            eu_to_us_shock, canada_to_us_shock, mexico_to_us_shock,
                            forecast_horizon) {
    
    # Initialize time series
    gdp <- inflation <- unemployment <- exports <- imports <- numeric(forecast_horizon)
    gdp[1] <- initial_values$GDP
    inflation[1] <- initial_values$Inflation
    unemployment[1] <- initial_values$Unemployment
    exports[1] <- initial_values$Exports
    imports[1] <- initial_values$Imports
    
    for (t in 2:forecast_horizon) {
      # Apply shocks to GDP
      gdp[t] <- gdp[t - 1] * (1 + 
                              elasticities$GDP$US_to_EU * us_to_eu_shock + 
                              elasticities$GDP$US_to_China * us_to_china_shock + 
                              elasticities$GDP$US_to_Canada * us_to_canada_shock + 
                              elasticities$GDP$US_to_Mexico * us_to_mexico_shock +
                              elasticities$GDP$China_to_US * china_to_us_shock +
                              elasticities$GDP$Canada_to_US * canada_to_us_shock +
                              elasticities$GDP$EU_to_US * eu_to_us_shock +
                              elasticities$GDP$Mexico_to_US * mexico_to_us_shock)
      
      # Calculate output gap
      output_gap <- (gdp[t] - gdp[t - 1]) / gdp[t - 1]
      
      # Apply shocks to Inflation
      inflation[t] <- inflation[t - 1] + 
                      (elasticities$Inflation$US_to_EU * us_to_eu_shock + 
                       elasticities$Inflation$US_to_China * us_to_china_shock + 
                       elasticities$Inflation$US_to_Canada * us_to_canada_shock + 
                       elasticities$Inflation$US_to_Mexico * us_to_mexico_shock +
                       elasticities$Inflation$China_to_US * china_to_us_shock +
                       elasticities$Inflation$Canada_to_US * canada_to_us_shock +
                       elasticities$Inflation$EU_to_US * eu_to_us_shock +
                       elasticities$Inflation$Mexico_to_US * mexico_to_us_shock) * output_gap
      
      # Apply shocks to Unemployment
      unemployment[t] <- unemployment[t - 1] - 
                         (elasticities$Unemployment$US_to_EU * us_to_eu_shock + 
                          elasticities$Unemployment$US_to_China * us_to_china_shock + 
                          elasticities$Unemployment$US_to_Canada * us_to_canada_shock + 
                          elasticities$Unemployment$US_to_Mexico * us_to_mexico_shock +
                          elasticities$Unemployment$China_to_US * china_to_us_shock +
                          elasticities$Unemployment$Canada_to_US * canada_to_us_shock +
                          elasticities$Unemployment$EU_to_US * eu_to_us_shock +
                          elasticities$Unemployment$Mexico_to_US * mexico_to_us_shock) * output_gap
      
      # Apply shocks to Exports and Imports
      exports[t] <- exports[t - 1] * (1 + elasticities$Exports$US_to_China * us_to_china_shock)
      imports[t] <- imports[t - 1] * (1 + elasticities$Imports$US_to_China * us_to_china_shock)
    }
    
    data.frame(GDP = gdp, Inflation = inflation, Unemployment = unemployment, Exports = exports, Imports = imports)
  }
  
  # Generate forecasts
  baseline_df <- simulate_dsge(initial_values, elasticities, 0, 0, 0, 0, 0, 0, 0, 0, forecast_horizon)
  shock_df <- simulate_dsge(initial_values, elasticities, us_to_china_shock, china_to_us_shock, us_to_eu_shock, us_to_canada_shock, us_to_mexico_shock, eu_to_us_shock, canada_to_us_shock, mexico_to_us_shock, forecast_horizon)
  
  list(
    forecast = baseline_df,
    shock = shock_df
  )
}


# BVAR model
run_bvar <- function(data_list) {
  library(vars)
  
  # Convert training data to time series
  base_Y <- ts(data_list$train[, macro_vars], start = c(1995, 1), frequency = 4)
  
  # Train BVAR model
  model <- VAR(base_Y, p = 4, type = "const")
  
  # Generate baseline forecasts
  forecast <- predict(model, n.ahead = nrow(data_list$test))
  baseline_df <- as.data.frame(sapply(macro_vars, function(var) forecast$fcst[[var]][, "fcst"]))
  colnames(baseline_df) <- macro_vars
  
  # Apply shock to test data
  shocked_test <- data_list$test
  shocked_test[, tariff_vars] <- shocked_test[, tariff_vars] * 1.10
  
  # Combine training and shocked test data
  combined_data <- rbind(data_list$train, shocked_test)
  combined_Y <- ts(combined_data[, macro_vars], start = c(1995, 1), frequency = 4)
  
  # Train shocked BVAR model
  model_shock <- VAR(combined_Y, p = 4, type = "const")
  forecast_shock <- predict(model_shock, n.ahead = nrow(data_list$test))
  shock_df <- as.data.frame(sapply(macro_vars, function(var) forecast_shock$fcst[[var]][, "fcst"]))
  colnames(shock_df) <- macro_vars
  
  # Return the forecast and shock data
  list(forecast = baseline_df, shock = shock_df)
}


# Run all models and summarize
run_all_models <- function(file_path) {
  data_list <- load_data(file_path)
  
  # Define the macro variables
  macro_vars <<- c("GDP", "Inflation", "Unemployment", "Exports", "Imports")
  
  # Define elasticity parameters based on OLS results
  elasticity_params <- list(
    GDP = list(
      US_to_EU = 0.76240,
      US_to_China = -0.95541,
      US_to_Canada = 0.14289,
      US_to_Mexico = -0.22324,
      China_to_US = 0.02155,
      Canada_to_US = -0.11660,
      EU_to_US = -0.58571,
      Mexico_to_US = -0.43444
    ),
    Unemployment = list(
      US_to_EU = -0.86345,
      US_to_China = 1.33480,
      US_to_Canada = -1.33479,
      US_to_Mexico = 1.17243,
      China_to_US = -0.48858,
      Canada_to_US = -0.21291,
      EU_to_US = 0.63068,
      Mexico_to_US = 0.28416
    ),
    Exports = list(
      US_to_EU = 0.41691,
      US_to_China = -0.88495,
      US_to_Canada = -0.04775,
      US_to_Mexico = -0.23453,
      China_to_US = 0.07289,
      Canada_to_US = -0.18390,
      EU_to_US = -0.24929,
      Mexico_to_US = -0.52189
    ),
    Imports = list(
      US_to_EU = 0.56075,
      US_to_China = -0.98538,
      US_to_Canada = 0.12306,
      US_to_Mexico = -0.31112,
      China_to_US = -0.01274,
      Canada_to_US = -0.17300,
      EU_to_US = -0.55539,
      Mexico_to_US = -0.37781
    ),
    Inflation = list(
      US_to_EU = 1.20493,
      US_to_China = -0.31125,
      US_to_Canada = 0.63554,
      US_to_Mexico = -0.91671,
      China_to_US = -0.24748,
      Canada_to_US = -0.32704,
      EU_to_US = -0.38766,
      Mexico_to_US = 0.04395
    )
  )
  
  # Run all models
  results <- list(
    DFM = run_dfm(data_list),
    XGB = run_xgboost(data_list),
    DSGE = run_dsge(
      data_list,
      us_to_china_shock = 0.10,
      china_to_us_shock = 0.10,
      us_to_eu_shock = 0.10,
      us_to_canada_shock = 0.10,
      us_to_mexico_shock = 0.10,
      eu_to_us_shock = 0.10,
      canada_to_us_shock = 0.10,
      mexico_to_us_shock = 0.10
    ),
    BVAR = run_bvar(data_list)
  )
  
  # Print accuracy metrics
  accuracy_metrics <- summarize_accuracy(data_list$test, results)
  print(accuracy_metrics)
  
  # Plot grouped models
  plot_all_models(data_list$full, results, title_prefix = "Forecast Comparison for ")
  
  # Plot individual models
  for (name in names(results)) {
    plot_individual_model(data_list$full, results[[name]], model_name = name)
  }
}

# Call run_all_models() to execute everything
run_all_models(file_path)
