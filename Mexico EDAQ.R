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
Mexico<- read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Mexico/Quarterly Analysis/Mexico DataQ.xlsx")
head(Mexico)


#Summary Stats
summary(Mexico)

#Descriptive Stats 
describe(Mexico)

names(Mexico)

#Columns 
selected_columns<- c("GDP", "Unemployment", "Exports",  "USA to Mexico Tariff", "Mexico to US Tariff",
                     "Exchange Rate", "Inflation")


# Compute correlation matrix using pairwise complete observations
cor_matrix <- cor(Mexico[, selected_columns], use = "pairwise.complete.obs")


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
  labs(title = "Mexico Correlation Heatmap", x = "", y = "")

# Convert data to long format for ggplot
Mexico_long <- pivot_longer(Mexico_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(Mexico_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "Mexico Histograms of Variables", x = "Value", y = "Count")


#Missing Values
missing_percentage <- colMeans(is.na(Mexico)) * 100
print(missing_percentage)


#Fill
Mexico <- na.locf(Mexico, na.rm = FALSE) 
Mexico<- na.locf(Mexico, fromLast = TRUE, na.rm = FALSE)
sum(is.na(Mexico))
missing_percentage <- colMeans(is.na(Mexico)) * 100
print(missing_percentage)
write_xlsx(Mexico, "C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Mexico/Quarterly Analysis/MexicoDataQ Final.xlsx")


#Describe & Summary Stats for Canada Cleaned 
Mexico_clean<-read_xlsx("C:/Users/alesi/OneDrive/Desktop/Predictive Analytics & Forecasting/Final Project/Mexico/Quarterly Analysis/MexicoDataQ Final.xlsx")

summary_stats<-summary(Mexico_clean)
print(summary_stats)
describe(Mexico_clean)


#Columns 
selected_columns<- c("GDP", "Unemployment", "Exports",  "USA to Mexico Tariff", "Mexico to US Tariff",
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
  labs(title = "Mexico Correlation Heatmap", x = "", y = "")

# Convert data to long format for ggplot
Mexico_long <- pivot_longer(Mexico_numeric, cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms using facet_wrap()
ggplot(Mexico_long, aes(x = Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +  # Creates separate histograms for each variable
  theme_minimal() +
  labs(title = "Mexico Cleaned Histograms of Variables", x = "Value", y = "Count")
