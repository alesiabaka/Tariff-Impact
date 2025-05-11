#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(vars)

# Load and preprocess data
data_list <- load_data("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Canada/Quarterly Analysis/Canada DataQ Final.xlsx")

# Train initial BVAR model
base_Y <- ts(data_list$train[, model_vars], start = c(1995, 1), frequency = 4)
model <- VAR(base_Y, p = 2, type = "const")

ui <- fluidPage(
  titlePanel("Interactive Tariff Shock Forecast: Canada"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_select", "Choose a macro variable:", choices = macro_vars, selected = "GDP"),
      sliderInput("can_to_us", "Canada to US Tariff Shock (% Increase):",
                  min = 0, max = 200, value = 10, step = 5),
      sliderInput("us_to_can", "US to Canada Tariff Shock (% Increase):",
                  min = 0, max = 200, value = 10, step = 5)
    ),
    mainPanel(
      plotOutput("forecastPlot", click = "plot_click"),
      DT::dataTableOutput("forecastTable")
    )
  )
)

server <- function(input, output) {
  output$forecastPlot <- renderPlot({
    # Convert percent to multiplier
    can_to_us_mult <- 1 + input$can_to_us / 100
    us_to_can_mult <- 1 + input$us_to_can / 100
    
    # Apply tariff shocks
    shocked_test <- data_list$test
    shocked_test$Canada_to_US_Tariff <- shocked_test$Canada_to_US_Tariff * can_to_us_mult
    shocked_test$US_to_Canada_Tariff <- shocked_test$US_to_Canada_Tariff * us_to_can_mult
    
    combined_data <- rbind(data_list$train, shocked_test)
    model_shock <- VAR(ts(combined_data[, model_vars], start = c(1995, 1), frequency = 4), p = 2, type = "const")
    forecast_shock <- predict(model_shock, n.ahead = nrow(data_list$test))
    
    baseline <- predict(model, n.ahead = nrow(data_list$test))
    
    var_name <- input$var_select
    hist <- data_list$full[[var_name]]
    fcst <- baseline$fcst[[var_name]][, "fcst"]
    shck <- forecast_shock$fcst[[var_name]][, "fcst"]
    
    start_year <- 1995
    start_quarter <- 1
    total_quarters <- length(hist) + length(fcst)
    quarters <- zoo::as.yearqtr(seq(from = as.Date("1995-01-01"), by = "quarter", length.out = total_quarters))
    
    df <- data.frame(
      Quarter = quarters,
      Historical = c(hist, rep(NA, length(fcst))),
      Forecast = c(rep(NA, length(hist)), fcst),
      Shocked = c(rep(NA, length(hist)), shck)
    )
    
    df_long <- tidyr::pivot_longer(df, -Quarter, names_to = "Series", values_to = "Value")
    
    ggplot(df_long, aes(x = Quarter, y = Value, color = Series)) +
      geom_line(linewidth = 1) +
      labs(title = paste("Forecast vs Shock for", var_name), y = var_name, x = "Quarter") +
      theme_minimal()
  })
  
  output$forecastTable <- DT::renderDataTable({
    var_name <- input$var_select
    baseline <- predict(model, n.ahead = nrow(data_list$test))
    shocked_test <- data_list$test
    shocked_test$Canada_to_US_Tariff <- shocked_test$Canada_to_US_Tariff * (1 + input$can_to_us / 100)
    shocked_test$US_to_Canada_Tariff <- shocked_test$US_to_Canada_Tariff * (1 + input$us_to_can / 100)
    
    combined_data <- rbind(data_list$train, shocked_test)
    model_shock <- VAR(ts(combined_data[, model_vars], start = c(1995, 1), frequency = 4), p = 2, type = "const")
    forecast_shock <- predict(model_shock, n.ahead = nrow(data_list$test))
    
    baseline_vals <- baseline$fcst[[var_name]][, "fcst"]
    shock_vals <- forecast_shock$fcst[[var_name]][, "fcst"]
    
    forecast_quarters <- zoo::as.yearqtr(seq(from = as.Date("2025-01-01"), by = "quarter", length.out = nrow(data_list$test)))
    
    data.frame(
      Quarter = forecast_quarters,
      Baseline_Forecast = baseline_vals,
      Shocked_Forecast = shock_vals
    )
  })
}

shinyApp(ui, server)