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


#---------------------------- ANALYSIS ----------------------------#
# 1. Show the first ten records with player statistics 
head(df, 10)
