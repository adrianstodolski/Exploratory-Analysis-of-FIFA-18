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
# 9. Create "wage_brackets" and "value_brackets"
# For further analyze the "wage" and "value"
# The wage_brackets are: 0–100k 100k-200k 200k-300k 300k-400k 400k-500k 500k+
# The value_brackets are : 0–10M 10–20M 20–30M 30–40M 40–50M 50–60M 60–70M 70–80M 80–90M 90–100M 100M+ 
# Create wage brackets
wage_breaks <- c(0, 100000, 200000, 300000, 400000, 500000, Inf)
wage_labels <- c("0-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k+")
wage_brackets <- cut(x=df$Wage, breaks=wage_breaks, 
                     labels=wage_labels, include.lowest = TRUE)
df <- mutate(df, wage_brackets)
# Create value brackets

value_breaks <- c(0, 10000000, 20000000, 30000000, 40000000, 50000000, 60000000, 70000000, 80000000, 90000000, 100000000, Inf)
value_labels <- c("0-10M", "10-20M", "20-30M", "30-40M", "40-50M","50-60M", "60-70M", "70-80M", "80-90M","90-100M","100M+")
value_brackets <- cut(x=df$Value, breaks=value_breaks, 
                      labels=value_labels, include.lowest = TRUE)
df <-mutate(df, value_brackets)
head(df)
# 10. Wage and values plot
not0To100K <- filter(df, wage_brackets != "0-100k") 
ggplot(not0To100K, aes(x = wage_brackets)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Distribution of top Wage between 100K-500K+")
# Plot
moreThan50M <- filter(df, Value>50000000)
ggplot(moreThan50M, aes(x = value_brackets)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Distribution of value between 50M-100M+")
# 11. Age vs Overall of players divided amongst wage brackets
g_age_overall <- ggplot(df, aes(Age, Overall))
g_age_overall + 
  geom_point(aes(color=wage_brackets)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Overall of players based  on Wage bracket")
# 12. Age vs Overall of players divided amongst value brackets
g_age_overall <- ggplot(df, aes(Age, Overall))
g_age_overall + geom_point(aes(color=value_brackets)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Overall of players based on Value bracket")
# 13. Number of players as per their general playing positions
ggplot(df, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Distribution based on General Playing Position")
# 14. Number of players as per their preferred playing positions
ggplot(df, aes(Preferred.Positions)) + geom_bar(aes(fill=..count..)) + 
  ggtitle("Distribution of players based on preferred position")
# 15. Players paid as wages and their valuation based on their preferred positions.
# Position based on value
gf1 <- filter(df, Value<30000000)
g1 <- ggplot(gf1, aes(Preferred.Positions)) + geom_bar(aes(fill=value_brackets)) + 
  ggtitle("Position based on Value (0-50M)")
gf2 <- filter(df,Value>30000000)
g2 <- ggplot(gf2, aes(Preferred.Positions)) + geom_bar(aes(fill=value_brackets)) + 
  ggtitle("Position based on Value (50M +)")
grid.arrange(g1, g2, ncol=1)
# Position based on wage
gw1 <- filter(df, Wage > 100000, Wage<300000)
g1 <- ggplot(gw1, aes(Preferred.Positions)) + geom_bar(aes(fill=wage_brackets)) + 
  ggtitle("Position based on Wage (0-100k)") 
gw2 <- filter(df,Wage>300000) 
g2 <- ggplot(gw2, aes(Preferred.Positions)) + geom_bar(aes(fill=wage_brackets)) + 
  ggtitle("Position based on Wage (100k+)")
grid.arrange(g1, g2, ncol=1)
# 16. The top ten valuable clubs
# The club value is calculated by summing up the player valuation for each club
group_clubs <- group_by(df, Club)
club_value <- summarise(group_clubs, total_val = sum(Value))
top_10_valuable_clubs <- top_n(club_value, 10, total_val)

top_10_valuable_clubs$Club <-as.factor(top_10_valuable_clubs$Club)

ggplot(top_10_valuable_clubs, aes(x = Club, y = total_val)) + geom_bar(stat = "identity", aes(fill=total_val)) + coord_flip() + ggtitle("Top 10 valuable clubs")
#---------------------------- CONCLUSIONS -------------------------------------#
# There is a high number of players around 25 years of age
# Most of the goalkeepers are around 22 years old
# Most of the defenders are around 25 years old
# Most of the wingers are around 22/25 years old
# Most of the offensives are around 25 years old
# Majority number of players have an overall rating of around 65
# Most of the players are from England and also a lot of the players come from Germany, France and Spain
# A large number of players earn a weekly wage of €100000
# A large number of players are worth €25,000,000 with the count decreasing sharply as the price increases
# A very large number of players have wages which lie between 0–100k and valuation between 0–50M
# The highest wages are commanded by players of overall 85+ and age around 30 years. Cristiano Ronaldo is number one
# Number of players as per their general playing positions. Number of center-backs is the highest followed by the number of strikers
# Attackers and midfielders are the most expensive and highest paid players
# Real Madrid, Bayern Munich and FC Barcelona are the most valuable clubs