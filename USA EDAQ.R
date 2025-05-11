library(readxl)
library(psych)
library(ggplot2)
library(tidyr)
library(corrplot)
library(reshape)
library(mice)
library(zoo)
library(writexl)
library(lubridate)
library(dplyr)

#Open File
USA <- read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/USA/Quarterly Analysis/US DataQ.xlsx")
head(USA)


# Count outliers as a percentage for each variable
outlier_counts <- sapply(numeric_data, function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  num_outliers <- sum(column < lower_bound | column > upper_bound, na.rm = TRUE)
  total_non_na <- sum(!is.na(column))
  (num_outliers / total_non_na) * 100  # Convert to percentage
})

# Convert to data frame for plotting
outlier_df <- data.frame(Variable = names(outlier_counts), Outliers_Percentage = outlier_counts)

# Plot all outliers as a percentage in a single grouped plot
ggplot(outlier_df, aes(x = reorder(Variable, -Outliers_Percentage), y = Outliers_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Percentage of Outliers in US Data",
       x = "Variable",
       y = "Percentage of Outliers (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))



#Summary Stats
summary(USA)

#Descriptive Stats 
describe(USA)

names(USA)

#Columns 
selected_columns<- c("GDP", "Unemployment", "Export", "Imports", "Total Manfacturing", "Inflation", "USA to World", "China to US Tariff", "Canada to US Tariff",
                     "Mexico to US Tariff", "EU to US Tariff")


# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(USA[, selected_columns], use = "pairwise.complete.obs")


# Convert correlation matrix to long format
cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Var1", "Var2", "value")

# Plot heatmap with no gray tiles
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation", na.value = "white"  # fill NAs white, though they shouldn’t occur now
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "USA Correlation Heatmap", x = "", y = "")

# Convert data to long format for ggplot
USA_long <- pivot_longer(USA_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(USA_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "USA Histograms of Variables", x = "Value", y = "Count")


library(ggplot2)

# Calculate the percentage of missing data for each variable
missing_percentages <- sapply(USA, function(column) {
  missing_count <- sum(is.na(column))
  total_count <- length(column)
  (missing_count / total_count) * 100
})

# Convert to data frame for plotting
missing_df <- data.frame(Variable = names(missing_percentages), Missing_Percentage = missing_percentages)

# Plot the missing percentages
ggplot(missing_df, aes(x = reorder(Variable, -Missing_Percentage), y = Missing_Percentage)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Percentage of Missing Data in US Data",
       x = "Variable",
       y = "Percentage of Missing Data (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))



#Fill
USA <- na.locf(USA, na.rm = FALSE) 
USA <- na.locf(USA, fromLast = TRUE, na.rm = FALSE)
sum(is.na(USA))
missing_percentage <- colMeans(is.na(USA)) * 100
print(missing_percentage)
write_xlsx(USA, "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/USA/Quarterly Analysis/US DataQ Final.xlsx")


#Describe & Summary Stats for USA Cleaned 
USA_clean<-read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/USA/Quarterly Analysis/US DataQ Final.xlsx")
names(USA_clean)


# Clean column names to remove spaces and special characters
names(USA_clean) <- gsub(" ", "_", names(USA_clean))
names(USA_clean) <- gsub("-", "_", names(USA_clean))
names(USA_clean) <- gsub("/", "_", names(USA_clean))
names(USA_clean) <- gsub("\\.", "_", names(USA_clean))

# Print the cleaned column names
print(names(USA_clean))



summary_stats<-summary(USA_clean)
print(summary_stats)
describe(USA_clean)


# Remove NA values from the selected columns
selected_columns <- names(USA_clean)[sapply(USA_clean, is.numeric)]
selected_columns <- selected_columns[!is.na(selected_columns)]

# Print the cleaned column names
print(selected_columns)

# Compute the correlation matrix
cor_matrix <- cor(USA_clean[, selected_columns], use = "pairwise.complete.obs")

# Convert to long format for ggplot
library(reshape2)
cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Var1", "Var2", "value")

# Plot the correlation heatmap
library(ggplot2)
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation", na.value = "white"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "USA Correlation Heatmap", x = "", y = "")




USA_clean <- USA_clean %>%
  mutate(Date = as.Date(as.yearqtr(YQ, format = "%Y Q%q")))

USA_long <- USA_clean %>%
  pivot_longer(cols = -c(Date, YQ), names_to = "Variable", values_to = "Value")

# Plot time series trends
ggplot(USA_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "USA Macroeconomic Indicators Trends Over Time",
       x = "Date",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


