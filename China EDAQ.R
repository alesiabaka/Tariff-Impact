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
China<- read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/China/Quarterly Analysis/China DataQ.xlsx")
head(China)


#Summary Stats
summary(China)

#Descriptive Stats 
describe(China)

names(China)

#Columns 
selected_columns<- c("GDP", "Unemployment", "Export",  "USA to China Tariff", "China to USA Tariff",
                     "Exchange Rate", "Inflation")


# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(China[, selected_columns], use = "pairwise.complete.obs")


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
  labs(title = "China Correlation Heatmap", x = "", y = "")

# Convert data to long format for ggplot
China_long <- pivot_longer(China_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(China_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "USA Histograms of Variables", x = "Value", y = "Count")


#Missing Values
missing_percentage <- colMeans(is.na(China)) * 100
print(missing_percentage)


#Fill
China <- na.locf(China, na.rm = FALSE) 
China<- na.locf(China, fromLast = TRUE, na.rm = FALSE)
sum(is.na(China))
missing_percentage <- colMeans(is.na(China)) * 100
print(missing_percentage)
write_xlsx(China, "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/China/Quarterly Analysis/China DataQ Final.xlsx")


#Describe & Summary Stats for Canada Cleaned 
China_clean<-read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/China/Quarterly Analysis/China DataQ Final.xlsx")

summary_stats<-summary(China_clean)
print(summary_stats)
describe(China_clean)


#Columns 
selected_columns<- c("GDP", "Unemployment", "Exports",  "US to Canada Tariff", "Canada to US Tariff",
                     "Exchange Rate", "Inflation")

# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(China[, selected_columns], use = "pairwise.complete.obs")


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
  labs(title = "China Correlation Heatmap", x = "", y = "")

# Convert data to long format for ggplot
China_long <- pivot_longer(China_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(China_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "China Cleaned Histograms of Variables", x = "Value", y = "Count")


