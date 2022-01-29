#----------------------------- FIFA SOCCER PLAYERS ANALYSIS -------------------#
# Analysis made for final project for Big Data Analysis at CDV.
# Analysis based on "www.sofifa.com" dataset.
# Author: Adrian Stodolski
# Year: 2022


#---------------------------- INITIAL REQUIREMENTS ----------------------------#
# 1. Put both files data and .R script code into the same location (folder).
# 2. Set working directory.
setwd("./")
# 3. Install require package with dependencies (comment after finished installation).
install.packages("ggplot2", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("gridExtra", dependencies = TRUE)
# 4. Load require package.

library(ggplot2)
library(dplyr)
library(gridExtra)
# 5. Load data into "df" variable
df = read.csv(file = "CompleteDataset.csv", stringsAsFactors = FALSE)
df = tibble(df)
df <- select(df, ID, X, Name, Age, Nationality, 
             Overall, Club, Value, Wage, Preferred.Positions)


#---------------------------- ANALYSIS ----------------------------------------#
# 1. Show the first ten records with player statistics 
head(df, 10)
# 2. Convert the "Value" and the "Wage" columns to actual currency values. 
# This function took a vector as an input and removed the “€” sign 
# from the columns and multiplied it with appropriate number to convert it 
# into thousand(K) and million(M)
toNumberCurrency <- function(vector) {
  vector <- as.character(vector)
  vector <- gsub("(€|,)","", vector)
  result <- as.numeric(vector)
  
  k_positions <- grep("K", vector)
  result[k_positions] <- as.numeric(gsub("K","",        vector[k_positions])) * 1000
  
  m_positions <- grep("M", vector)
  result[m_positions] <- as.numeric(gsub("M","", 
                                         vector[m_positions])) * 1000000
  
  return(result)
}

df$Wage <- toNumberCurrency(df$Wage) 
df$Value <- toNumberCurrency(df$Value)
# 3. Edition of the "Preferred.Positions" column
df$Preferred.Positions <- gsub(" ", "", substr(df$Preferred.Positions, 1, 3))
# 4. Edition of the "Preferred.Positions" column <- more specific values
# GK - goalkeeper
# DEF - defender
# MID - winger
# FWD - offensive
x <- as.factor(df$Preferred.Positions)
levels(x) <- list(GK  = c("GK"), 
                  DEF = c("LWB", "LB", "CB", "RB", "RWB"), 
                  MID = c("LW","LM","CDM","CM","CAM","RM","RW"), 
                  FWD = c("CF", "ST"))
df <- mutate(df, Position = x)
head(df)
# 5. Distribution of players based on the age
g_age <- ggplot(data = df, aes(Age))
g_age + 
  geom_histogram(col="orange", aes(fill = ..count..)) + ggtitle("Distribution based on Age")
# 6. Relation between the Age of the players and their general playing position.
g_age + 
  geom_density(col="orange", aes(fill = Position), alpha=0.5) + facet_grid(.~Position) + 
  ggtitle("Distribution based on Age and Position")
# 6. Distribution of players based on their overall rating
g_overall <- ggplot(data = df, aes(Overall))
g_overall + 
  geom_histogram(col="orange", aes(fill = ..count..)) + ggtitle("Distribution based on Overall Rating")
# 7. Number of players from different (top 10) countries
countries_count <- count(df, Nationality)
top_10_countries <- top_n(countries_count, 10, n)
top_10_country_names <- top_10_countries$Nationality

country <- filter(df, Nationality == top_10_country_names)
ggplot(country, aes(x = Nationality)) + 
  geom_bar(col = "orange", aes(fill = ..count..)) + ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")
# 8. Top 1 % count of the player "Value" and "Wage"
# Quantile function
top_1_percent_wage   <- quantile(df$Wage, probs=0.99)
filtered_wage <- filter(df, Wage > top_1_percent_wage)

g_value <- ggplot(filtered_wage, aes(Wage))
g_value + 
  geom_histogram(aes(fill=..count..)) + 
  ggtitle("Distribution of top 1% value")

top_1_percent_value   <- quantile(df$Value, probs=0.99)
filtered_value <- filter(df, Value > top_1_percent_value)
# Second chart
g_wage <- ggplot(filtered_value, aes(Value))
g_wage + 
  geom_histogram(aes(fill=..count..)) + 
  ggtitle("Distribution of top 1% Value")
