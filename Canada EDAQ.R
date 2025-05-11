library(readxl)
library(psych)
library(ggplot2)
library(tidyr)
library(corrplot)
library(reshape)
library(mice)
library(zoo)
library(writexl)

#Open File
Canada<- read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Canada/Quarterly Analysis/Canada DataQ.xlsx")
head(Canada)


#Summary Stats
summary(Canada)

#Descriptive Stats 
describe(Canada)

names(Canada)

#Columns 
selected_columns<- c("GDP", "Unemployment", "Exports",  "US to Canada Tariff", "Canada to US Tariff",
                   "Exchange Rate")


# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(Canada[, selected_columns], use = "pairwise.complete.obs")


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
Canada_long <- pivot_longer(Canada_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(Canada_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "USA Histograms of Variables", x = "Value", y = "Count")


#Missing Values
missing_percentage <- colMeans(is.na(Canada)) * 100
print(missing_percentage)


#Fill
Canada <- na.locf(Canada, na.rm = FALSE) 
Canada<- na.locf(Canada, fromLast = TRUE, na.rm = FALSE)
sum(is.na(Canada))
missing_percentage <- colMeans(is.na(Canada)) * 100
print(missing_percentage)
write_xlsx(Canada, "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Canada/Quarterly Analysis/Canada DataQ Final.xlsx")

# Load the data
Canada_clean <- read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Canada/Quarterly Analysis/Canada DataQ Final.xlsx")

# Clean column names
names(Canada_clean) <- make.names(names(Canada_clean))

# Select relevant columns
selected_columns <- c("GDP", "Unemployment", "Exports", "US.to.Canada.Tariff", "Canada.to.US.Tariff", "Exchange.Rate")

# Check for missing columns
missing_cols <- setdiff(selected_columns, names(Canada_clean))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# Convert to numeric if necessary
Canada_clean[selected_columns] <- lapply(Canada_clean[selected_columns], as.numeric)

# Summary Statistics
summary_stats <- summary(Canada_clean[selected_columns])
print(summary_stats)

# Compute correlation matrix
cor_matrix <- cor(Canada_clean[selected_columns], use = "pairwise.complete.obs")

# Convert correlation matrix to long format for heatmap
cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Var1", "Var2", "value")

# Plot correlation heatmap
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation", na.value = "white"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Canada Correlation Heatmap", x = "", y = "")
