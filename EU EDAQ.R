library(readxl)
library(psych)
library(ggplot2)
library(tidyr)
library(corrplot)
library(reshape)
library(mice)
library(zoo)
library(writexl)
library(tsibble)

#Open File
EU<- read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/EU/Quarterly Analysis/EU DataQ.xlsx")
head(EU)

# Identify numeric columns
numeric_cols <- sapply(EU, is.numeric)
numeric_data <- EU[, numeric_cols]


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
  labs(title = "Percentage of Outliers in EU Data",
       x = "Variable",
       y = "Percentage of Outliers (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))




#Summary Stats
summary(EU)

#Descriptive Stats 
describe(EU)

names(EU)

#Columns 
selected_columns<- c("GDP", "Unemployment", "Exports",  "USA to EU Tariff", "EU to USA Tariff",
                     "Exchange Rate", "Inflation")


# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(EU[, selected_columns], use = "pairwise.complete.obs")


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
  labs(title = "EU Correlation Heatmap", x = "", y = "")

# Convert data to long format for ggplot
EU_long <- pivot_longer(EU_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(EU_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "EU Histograms of Variables", x = "Value", y = "Count")


#Missing Values
missing_percentages <- sapply(EU, function(column) {
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
  labs(title = "Percentage of Missing Data in EU Data",
       x = "Variable",
       y = "Percentage of Missing Data (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))




#Fill
EU <- na.locf(EU, na.rm = FALSE) 
EU<- na.locf(EU, fromLast = TRUE, na.rm = FALSE)
sum(is.na(EU))
missing_percentage <- colMeans(is.na(EU)) * 100
print(missing_percentage)
write_xlsx(EU, "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/EU/Quarterly Analysis/EU DataQ Final.xlsx")


#Describe & Summary Stats for Canada Cleaned 
EU_clean<-read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/EU/Quarterly Analysis/EU DataQ Final.xlsx")

summary_stats<-summary(EU_clean)
print(summary_stats)
describe(EU_clean)

# Clean column names to remove spaces and special characters
names(EU_clean) <- gsub(" ", "_", names(EU_clean))
names(EU_clean) <- gsub("-", "_", names(EU_clean))
names(EU_clean) <- gsub("/", "_", names(EU_clean))
names(EU_clean) <- gsub("\\.", "_", names(EU_clean))

# Print the cleaned column names
print(names(EU_clean))



summary_stats<-summary(USA_clean)
print(summary_stats)
describe(USA_clean)


# Remove NA values from the selected columns
selected_columns <- names(EU_clean)[sapply(EU_clean, is.numeric)]
selected_columns <- selected_columns[!is.na(selected_columns)]

# Print the cleaned column names
print(selected_columns)

# Compute the correlation matrix
cor_matrix <- cor(EU_clean[, selected_columns], use = "pairwise.complete.obs")

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
  labs(title = "EU Correlation Heatmap", x = "", y = "")



# Convert the YQ column to Date
EU_clean <- EU_clean %>%
  mutate(YQ = yearquarter(YQ))  # Convert to yearquarter format

# Print the cleaned data
print(head(EU_clean))

# Convert YQ to Date
EU_clean <- EU_clean %>%
  mutate(Date = as.Date(as.yearqtr(YQ, format = "%Y Q%q")))

# Reshape the data to long format
EU_long <- EU_clean %>%
  pivot_longer(cols = -c(Date, YQ), names_to = "Variable", values_to = "Value")

# Plot time series trends
ggplot(EU_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "EU Macroeconomic Indicators Trends Over Time",
       x = "Date",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))