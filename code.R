## Title: Predicting the outcome of a Premier League soccer match
## Author: Brennan Reamer
## Date: 04/15/2023
## Description: This script will predict the outcome of a Premier League soccer match using the data from the 2019-2023 season
## The data is from https://www.football-data.co.uk/englandm.php.

# Data Column Name Definitions
## HomeTeam: Home Team Name
## AwayTeam: Away Team Name
## FTHG: Full Time Home Team Goals
## FTAG: Full Time Away Team Goals
## Referee: Referee Name
## B365H: Bet365 Home Win Odds
## B365D: Bet365 Draw Odds
## B365A: Bet365 Away Win Odds
## BWH: Bet&Win Home Win Odds
## BWD: Bet&Win Draw Odds
## BWA: Bet&Win Away Win Odds
## IWH: Interwetten Home Win Odds
## IWD: Interwetten Draw Odds
## IWA: Interwetten Away Win Odds
## PSH: Pinnacle Home Win Odds
## PSD: Pinnacle Draw Odds
## PSA: Pinnacle Away Win Odds
## PSCH: Pinnacle Home Win Odds
## PSCD: Pinnacle Draw Odds
## PSCA: Pinnacle Away Win Odds
## MaxH: Maximum Home Win Odds
## MaxD: Maximum Draw Odds
## MaxA: Maximum Away Win Odds

# Load the libraries
library(dplyr)
library(caret)
library(randomForest)

# Seed the random number generator
#set.seed(123)

# Read the data
data_2223 <- read.csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv", header = TRUE, sep = ",")
data_2122 <- read.csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv", header = TRUE, sep = ",")
data_2021 <- read.csv("https://www.football-data.co.uk/mmz4281/2021/E0.csv", header = TRUE, sep = ",")
data_1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/E0.csv", header = TRUE, sep = ",")

# Make all columns the same
data_2223 <- subset(data_2223, select = c(HomeTeam,AwayTeam,FTHG,FTAG,Referee,B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,WHH,WHD,WHA,VCH,VCD,VCA,MaxH,MaxD,MaxA,AvgH,AvgD,AvgA,AHh,B365AHH,B365AHA,PAHH,PAHA,MaxAHH,MaxAHA,AvgAHH,AvgAHA,B365CH,B365CD,B365CA,BWCH,BWCD,BWCA,IWCH,IWCD,IWCA,PSCH,PSCD,PSCA,WHCH,WHCD,WHCA,VCCH,VCCD,VCCA,MaxCH,MaxCD,MaxCA,AvgCH,AvgCD,AvgCA))
data_2122 <- subset(data_2122, select = c(HomeTeam,AwayTeam,FTHG,FTAG,Referee,B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,WHH,WHD,WHA,VCH,VCD,VCA,MaxH,MaxD,MaxA,AvgH,AvgD,AvgA,AHh,B365AHH,B365AHA,PAHH,PAHA,MaxAHH,MaxAHA,AvgAHH,AvgAHA,B365CH,B365CD,B365CA,BWCH,BWCD,BWCA,IWCH,IWCD,IWCA,PSCH,PSCD,PSCA,WHCH,WHCD,WHCA,VCCH,VCCD,VCCA,MaxCH,MaxCD,MaxCA,AvgCH,AvgCD,AvgCA))
data_2021 <- subset(data_2021, select = c(HomeTeam,AwayTeam,FTHG,FTAG,Referee,B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,WHH,WHD,WHA,VCH,VCD,VCA,MaxH,MaxD,MaxA,AvgH,AvgD,AvgA,AHh,B365AHH,B365AHA,PAHH,PAHA,MaxAHH,MaxAHA,AvgAHH,AvgAHA,B365CH,B365CD,B365CA,BWCH,BWCD,BWCA,IWCH,IWCD,IWCA,PSCH,PSCD,PSCA,WHCH,WHCD,WHCA,VCCH,VCCD,VCCA,MaxCH,MaxCD,MaxCA,AvgCH,AvgCD,AvgCA))
data_1920 <- subset(data_1920, select = c(HomeTeam,AwayTeam,FTHG,FTAG,Referee,B365H,B365D,B365A,BWH,BWD,BWA,IWH,IWD,IWA,PSH,PSD,PSA,WHH,WHD,WHA,VCH,VCD,VCA,MaxH,MaxD,MaxA,AvgH,AvgD,AvgA,AHh,B365AHH,B365AHA,PAHH,PAHA,MaxAHH,MaxAHA,AvgAHH,AvgAHA,B365CH,B365CD,B365CA,BWCH,BWCD,BWCA,IWCH,IWCD,IWCA,PSCH,PSCD,PSCA,WHCH,WHCD,WHCA,VCCH,VCCD,VCCA,MaxCH,MaxCD,MaxCA,AvgCH,AvgCD,AvgCA))

# Append old datasets onto data_2223
data <- rbind(data_2223, data_2122)
data <- rbind(data, data_2021)
data <- rbind(data, data_1920)

# Split data randomly into training and testing
test <- data[sample(nrow(data), 0.2*nrow(data)),]
train <- data[-sample(nrow(data), 0.2*nrow(data)),]

#Prepare training data
train <- subset(train, select = c(HomeTeam, AwayTeam, FTHG, FTAG, Referee, B365H, B365D, B365A, BWH, BWD, BWA, IWH, IWD, IWA, PSCA, PSCD, PSCH, MaxA, MaxD, MaxH))
train_home <- subset(train, select = c(HomeTeam, AwayTeam, FTHG, Referee, B365H, B365D, B365A, BWH, BWD, BWA, IWH, IWD, IWA, PSCA, PSCD, PSCH, MaxA, MaxD, MaxH))
train_away <- subset(train, select = c(HomeTeam, AwayTeam, FTAG, Referee, B365H, B365D, B365A, BWH, BWD, BWA, IWH, IWD, IWA, PSCA, PSCD, PSCH, MaxA, MaxD, MaxH))

#Prepare testing data
result_FTHG <- test$FTHG
result_FTAG <- test$FTAG
test <- subset(test, select = c(HomeTeam, AwayTeam, Referee, B365H, B365D, B365A, BWH, BWD, BWA, IWH, IWD, IWA, PSCA, PSCD, PSCH, MaxA, MaxD, MaxH))

#Use Poisson Regression to predict FTHG and FTAG
p_FTHG <- glm(FTHG ~ ., data = train_home, family = poisson(link = "log"))
p_FTAG <- glm(FTAG ~ ., data = train_away, family = poisson(link = "log"))

#Use Random Forest Regression to predict FTHG and FTAG
rf_reg_FTHG <- randomForest(FTHG ~ ., data = train_home, importance = TRUE, proximity = TRUE)
rf_reg_FTAG <- randomForest(FTAG ~ ., data = train_away, importance = TRUE, proximity = TRUE)

# Print the summary of the models
print(summary(rf_reg_FTHG))
print(summary(rf_reg_FTAG))
print(summary(p_FTHG))
print(summary(p_FTAG))

# Predict the values
predict_rf_reg_FTHG <- predict(rf_reg_FTHG, test)
predict_rf_reg_FTAG <- predict(rf_reg_FTAG, test)
predict_p_FTHG <- predict(p_FTHG, test)
predict_p_FTAG <- predict(p_FTAG, test)

# Plot the predicted values vs the actual values for easy readability
#plot(predict_rf_reg_FTHG, result_FTHG, xlab = "Predicted FTHG", ylab = "Actual FTHG", main = "Random Forest Regression FTHG")
#plot(predict_rf_reg_FTAG, result_FTAG, xlab = "Predicted FTAG", ylab = "Actual FTAG", main = "Random Forest Regression FTAG")
#plot(predict_p_FTHG, result_FTHG, xlab = "Predicted FTHG", ylab = "Actual FTHG", main = "Poisson Regression FTHG")
#plot(predict_p_FTAG, result_FTAG, xlab = "Predicted FTAG", ylab = "Actual FTAG", main = "Poisson Regression FTAG")

# Print the predicted values
print(paste("Predicted RF Regression FTHG: "))
print(predict_rf_reg_FTHG)
print(paste("Predicted Poisson FTHG: "))
print(predict_p_FTHG)
print(paste("Actual FTHG: "))
print(result_FTHG)
print(paste("Predicted Poisson FTAG: "))
print(predict_p_FTAG)
print(paste("Predicted RF Regression FTAG: "))
print(predict_rf_reg_FTAG)
print(paste("Actual FTAG: "))
print(result_FTAG)

#Test output
print(paste("Mean Squared Error: "))
print(paste("Poisson Regression:"))
print(paste("FTHG: ", mean((as.numeric(predict_p_FTHG) - result_FTHG)^2)))
print(paste("FTAG: ", mean((as.numeric(predict_p_FTAG) - result_FTAG)^2)))
print(paste("Random Forest Regression:"))
print(paste("FTHG: ", mean((as.numeric(predict_rf_reg_FTHG) - result_FTHG)^2)))
print(paste("FTAG: ", mean((as.numeric(predict_rf_reg_FTAG) - result_FTAG)^2)))

#calculate r^2 value
print(paste("R^2: "))
print(paste("Poisson FTHG:", cor(as.numeric(predict_p_FTHG), result_FTHG)^2))
print(paste("Poisson FTAG:", cor(as.numeric(predict_p_FTAG), result_FTAG)^2))
print(paste("Random Forest FTHG:", cor(as.numeric(predict_rf_reg_FTHG), result_FTHG)^2))
print(paste("Random Forest FTAG:", cor(as.numeric(predict_rf_reg_FTAG), result_FTAG)^2))

#Testing on live games (single match)
# Many Odds, including B365 and Max - https://www.oddschecker.com/football/english/premier-league/
# IW - https://www.interwetten.com/en/sportsbook/l/1021/england-premier-league
# BW - https://sports.bwin.com/en/sports/football-4/betting/england-14/premier-league-102841
# PSC - https://www.pinnacle.com/en/soccer/england-premier-league/matches
# Odds Converter - https://www.aceodds.com/bet-calculator/odds-converter.html

#Input live data
#live_data <- data.frame(HomeTeam="Chelsea", AwayTeam="Brighton", Referee="R Jones", B365H=2.7, B365D=3.6, B365A=2.75, BWH=2.6, BWD=3.3, BWA=2.7, IWH=2.6, IWD=3.3, IWA=2.75, PSCA=2.8, PSCD=3.53, PSCH=2.59, MaxA=2.8, MaxD=3.6, MaxH=2.65)
#live_data <- data.frame(HomeTeam="Man City", AwayTeam="Leicester", Referee="D England", B365H=1.14, B365D=8.5, B365A=15, BWH=1.15, BWD=8.75, BWA=17.5, IWH=1.15, IWD=8.25, IWA=18, PSCA=19.68, PSCD=9.29, PSCH=1.142, MaxA=23, MaxD=9.5, MaxH=1.17)
#live_data <- data.frame(HomeTeam="Wolves", AwayTeam="Brentford", Referee="P Tierney", B365H=2.55, B365D=2.9, B365A=3.1, BWH=2.5, BWD=3.2, BWA=2.9, IWH=2.55, IWD=3.15, IWA=2.9, PSCA=2.95, PSCD=3.27, PSCH=2.61, MaxA=3.05, MaxD=3.25, MaxH=2.6)
live_data <- data.frame(HomeTeam="Nott'm Forest", AwayTeam="Man United", Referee="S Hooper", B365H=4.75, B365D=3.8, B365A=1.73, BWH=4.8, BWD=3.8, BWA=1.69, IWH=4.7, IWD=3.85, IWA=1.73, PSCA=1.763, PSCD=3.88, PSCH=4.89, MaxA=1.76, MaxD=4, MaxH=5)

# Predict the values
game_result_h_p <- predict(p_FTHG, live_data)
game_result_a_p <- predict(p_FTAG, live_data)
game_result_h_rf <- predict(rf_reg_FTHG, live_data)
game_result_a_rf <- predict(rf_reg_FTAG, live_data)

# Print the results
print(paste("Predicted FTHG: "))
print(paste("Poisson Regression: ", game_result_h_p))
print(paste("Random Forest Regression: ", game_result_h_rf))

print(paste("Predicted FTAG: "))
print(paste("Poisson Regression: ", game_result_a_p))
print(paste("Random Forest Regression: ", game_result_a_rf))
