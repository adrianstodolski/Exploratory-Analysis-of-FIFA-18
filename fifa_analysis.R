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
# into thousand(K) and million(M).
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
