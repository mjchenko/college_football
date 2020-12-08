#####Please note, to view tables go to viewer tab#####
#####To view plots go to plots tab#####

##################installing required packages and loading libraries####
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(readr)
library(RCurl)
library(tidyverse)
library(stringr)
library(caret)
library(knitr)
library(kableExtra)
library(data.table)
####################




####################Downloading Data from Github####################
#downloading dataset 
url <- getURL("https://raw.githubusercontent.com/mjchenko/college_football/main/CFB2019.csv")
cfb_data <- read_csv(file = url, skip = 0, col_names = TRUE)

#making a df to examine the size of the dataset
tab <- data_frame("Number of Rows" = nrow(cfb_data), 
                  "Number of Columns" = ncol(cfb_data))
#table
kable(tab, caption = "Size of Dataset", booktabs = T, 
      linesep = "", digits = 3) %>% kable_styling(latex_options = "striped")
####################




####################Cleaning Data####################
#team column has both team and conference as TeamName (Conf) so we need to split
#two entries do not follow the pattern.  Miami (OH) (MAC) and Miami (FL) (ACC)
#adding conference to the data
conf <- str_split(cfb_data$Team, pattern = "[()]", simplify = TRUE)[,2]
conf <- str_replace(conf, pattern = "FL", replacement = "ACC")
conf <- str_replace(conf, pattern = "OH", replacement = "MAC")
cfb_data <- cfb_data %>% add_column(.after = 'Team', conf = conf)
cfb_data$conf <- cfb_data$conf <- as.factor(cfb_data$conf)

#splitting first entry to only be the team name
teams <- str_trim(str_split(cfb_data$Team, pattern = "[\\(]", simplify = TRUE)[,1], side = "right")
#two entries are both Miami so we need to differentiate
teams[58:59] <- c("Miami (FL)", "Miami (OH)")
#mutate the team names to get rid of conference
cfb_data <- cfb_data %>% mutate(Team = teams)


###win-loss is in one column separated by dash.  
###Split into own columns and then remove original columm
#splitting W-L column into two difference columns
wins <- str_split(cfb_data$`Win-Loss`, pattern = "-", simplify = TRUE)[,1]
wins <- as.numeric(wins)
losses <- str_split(cfb_data$`Win-Loss`, pattern = "-", simplify = TRUE)[,2]
losses <- as.numeric(losses)
cfb_data <- cfb_data %>% 
  add_column(.after = 'conf', wins = wins, losses = losses) %>% 
  select(-'Win-Loss')

###creating the win percentage column
cfb_data <- cfb_data %>% mutate(win_perc = wins/Games)


#removing columns that contain the pattern rank
ind <- str_detect(colnames(cfb_data), pattern = "Rank")
cfb_data <- cfb_data[!ind]

#mutating to per game or per attempt stats to make comparisons easier between teams
#not all teams have the same attempts or same number of games played
cfb_data <- cfb_data %>% 
  mutate(int_per_attempt = `Interceptions Thrown.y`/`Pass Attempts`,
         pass_attempt_per_game = `Pass Attempts`/Games,
         pass_completion_per_game = `Pass Completions`/Games,
         pass_attempt_per_game_allowed = `Opp Pass Attempts`/Games,
         pass_completion_per_game_allowed = `Opp Completions Allowed`/Games,
         rush_td_per_game = `Rushing TD`/Games,
         rush_td_per_game_allowed = `Opp Rush Touchdowns Allowed`/Games,
         pass_td_per_game = `Pass Touchdowns`/Games, 
         pass_td_per_game_allowed = `Opp Pass TDs Allowed`/Games,
         ko_return_td_per_game =`Kickoff Return Touchdowns`/Games,
         ko_return_td_per_game_allowed = `Opp Kickoff Return Touchdowns Allowed`/Games,
         fmb_rec_per_game = `Fumbles Recovered`/Games,
         fmb_loss_per_game = `Fumbles Lost`/Games
  )

#selecting stats we want to keep
cfb_data <- cfb_data %>%
  select(Team,
         conf,
         win_perc,
         wins,
         losses,
         off_yards_per_play = `Off Yards/Play`, 
         yards_per_play_allowed = `Yards/Play Allowed`, 
         off_yards_per_game = `Off Yards per Game`, 
         yards_per_game_allowed = `Yards Per Game Allowed`,
         percent_conv_4th = `4th Percent`, 
         percent_conv_4th_allowed = `Opponent 4th Percent`,
         yards_per_kickoff_return_allowed = `Avg Yards per Kickoff Return Allowed`, 
         yards_per_kickoff_return = `Avg Yard per Kickoff Return`,
         pass_yards_per_game = `Pass Yards Per Game`, 
         pass_yards_per_attempt = `Pass Yards/Attempt`,
         pass_yards_per_completion = `Yards/Completion`,
         pass_yards_per_game_allowed = `Pass Yards Per Game Allowed`, 
         penalty_yards_per_game = `Penalty Yards Per Game`,
         yards_per_punt_return = `Avg Yards Per Punt Return`, 
         yards_per_punt_return_allowed = `Avg Yards Allowed per Punt Return`,
         percent_rz_points_allowed = `Redzone Points Allowed`, 
         percent_rz_points = `Redzone Points`,
         yards_per_rush_allowed = `Yds/Rush Allowed`,
         rush_yards_per_game_allowed = `Rush Yards Per Game Allowed`,
         rush_yards_per_game = `Rushing Yards per Game`,
         yards_per_rush = `Yards/Rush`,
         sacks_per_game = `Average Sacks per Game`,
         points_per_game_allowed = `Avg Points per Game Allowed`, 
         points_per_game = `Points Per Game`, 
         percent_conv_3rd = `3rd Percent`, 
         turnover_margin_per_game = `Avg Turnover Margin per Game`,
         int_per_attempt,
         pass_attempt_per_game,
         pass_completion_per_game,
         pass_attempt_per_game_allowed,
         pass_completion_per_game_allowed,
         rush_td_per_game,
         rush_td_per_game_allowed,
         pass_td_per_game, 
         pass_td_per_game_allowed,
         ko_return_td_per_game,
         ko_return_td_per_game_allowed,
         fmb_rec_per_game,
         fmb_loss_per_game 
  )
####################



####################Data Visualization####################
###making a data table to show highest positive correlations of stats
#with win percentage
y <- cfb_data$win_perc
x <- cfb_data %>% select(-Team, - conf, -win_perc, - wins, - losses)

tab <- data_frame(Predictor = rownames(cor(x,y)), Correlation = cor(x,y)) %>%
  slice_max(order_by = Correlation, n = 10)

kable(tab, caption = "Offensive Effects", booktabs = T, 
      linesep = "", digits = 3) %>% kable_styling(latex_options = "striped")



###making a table to show highest negative correlations
tab <- data_frame(Predictor = rownames(cor(x,y)), Correlation = cor(x,y)) %>%
  slice_min(order_by = Correlation, n = 10)

kable(tab, caption = "Defensive Effects", booktabs = T, 
      linesep = "", digits = 3) %>% kable_styling(latex_options = "striped")



###making a table that shows variables that dont appear to have correlation 
#with win #percentage
tab <- data_frame(predictor = rownames(cor(x,y)), 
                  abs_correlation = abs(cor(x,y))) %>% 
  slice_min(order_by = abs_correlation, n = 5) 
colnames(tab) <- c("Predictor", "Absolute Value of Correlation")

kable(tab, caption = "Least Correlated to Wins", booktabs = T, 
      linesep = "", digits = 3) %>% kable_styling(latex_options = "striped")



#plotting both offensive yards per game and offensive yards per play to show confounding
ggplot(data = cfb_data, aes(x = off_yards_per_game, y = off_yards_per_play)) +
  geom_point() + xlab("Offensive Yards per Game") + ylab("Offensive Yards per Play")

ggplot(data = cfb_data, aes(x = off_yards_per_play, y = win_perc)) +
  geom_point() + xlab("Offensive Yards per Play") + ylab("Win Percentage")

ggplot(data = cfb_data, aes(x = off_yards_per_game, y = win_perc)) +
  geom_point() + xlab("Offensive Yards per Game") + ylab("Win Percentage")


#plotting both offensive yards per game allowed and offensive yards per play allowed to show confounding
ggplot(data = cfb_data, aes(x = yards_per_game_allowed, y = yards_per_play_allowed)) +
  geom_point() + xlab("Yards per Game Allowed") + ylab("Yards per Play Allowed")

ggplot(data = cfb_data, aes(x = yards_per_play_allowed, y = win_perc)) +
  geom_point() + xlab("Yards per Play Allowed") + ylab("Win Percentage")

ggplot(data = cfb_data, aes(x = yards_per_game_allowed, y = win_perc)) +
  geom_point() + xlab("Yards per Game Allowed") + ylab("Win Percentage")


#cleaning data by removing variables that are very similar
cfb_data <- cfb_data %>% 
  select(-off_yards_per_game, -yards_per_game_allowed, )



#histogram showing distribution of win percentage
cfb_data %>% ggplot(aes(x = win_perc)) +
  geom_histogram(bins = 10, alpha = 0.75, col = "black", fill = "blue") +
  xlab("Win Percentage") + ylab("Number of Teams") + 
  ggtitle("Distribution of Win Percentage")

#qq plot showing win_perc appears to be normal
ggplot(data = cfb_data, aes(sample = scale(win_perc))) + geom_qq() + geom_abline()



#boxplot showing win percentage for difference conferences
cfb_data %>% ggplot(aes(x = conf, y = win_perc, fill = conf)) + 
  geom_boxplot() +
  xlab("Conference") + ylab("Win Percentage") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(legend.position = "none")



#plot win percentage vs offensive yards per play
cfb_data %>% ggplot(aes(x = off_yards_per_play, y = win_perc)) +
  geom_point() + xlab("Offensive Yards Per Play") + ylab("Win Percentage") + 
  geom_smooth(method = "lm", se = FALSE)

#plot win percentage vs 3rd down conversion
cfb_data %>% ggplot(aes(x = percent_conv_3rd, y = win_perc)) +
  geom_point() + xlab("3rd Down Conversion Success") + ylab("Win Percentage") + 
  geom_smooth(method = "lm", se = FALSE)



#plot win percentage vs yards per play allowed
cfb_data %>% ggplot(aes(x = yards_per_play_allowed, y = win_perc)) +
  geom_point() + xlab("Yards Allowed Per Play") + ylab("Win Percentage") + 
  geom_smooth(method = "lm", se = FALSE)

#plot win percentage vs rush yards per game allowed
cfb_data %>% ggplot(aes(x = rush_yards_per_game_allowed, y = win_perc)) +
  geom_point() + xlab("Rush Yards Allowed per Game") + ylab("Win Percentage") + 
  geom_smooth(method = "lm", se = FALSE)
###################



##################Evaluation Function#################
#RMSE function used for evaluations
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
} 
#################



#################Partitioning Data#################
#partition data into train, and test sets using 20% for test set
set.seed(1, sample.kind="Rounding")
test_ind <- createDataPartition(y = cfb_data$win_perc, times = 1, p = 0.2, list = FALSE)
test_set <- cfb_data[test_ind,]
train_set <- cfb_data[-test_ind,]

y_train <- train_set$win_perc
x_train <- train_set %>% select(-win_perc, -Team, -wins, - losses, -conf)

y_test <- test_set$win_perc
x_test <- test_set %>% select(-win_perc, -Team, -wins, - losses, -conf)
#################


#################Linear Regression Train Set#################
#fit linear regression model
fit_lm <- train(x_train, y_train, method = "lm", tuneGrid = data.frame("intercept" = 0))

# predict win percentage based on model and calculate RMSE
pred_lm <- predict(fit_lm)
RMSE_lm <- RMSE(y_train,pred_lm)
RMSE_tab <- data_frame("Method" = "Linear Regression", "RMSE" = RMSE_lm)

#plot how well predictions fit linear model
df <- data_frame(y_train, pred_lm)
df %>% ggplot(aes(x = y_train, y = pred_lm)) + geom_point() + geom_abline()

#show table of results
kable(RMSE_tab, caption = "Model RMSEs (Training)", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")

#variable importance from linear model
varImp(fit_lm)
#################



#################KNN Train Set#################
#create values of k
ks = seq(4,20)

#Calculate RMSE for each value of k
RMSE_knn <- sapply(ks, function(k){
  fit_knn <- train(x_train, y_train, method = "knn", tuneGrid = data.frame("k" = k))
  pred_knn <- predict(fit_knn)
  RMSE(y_train, pred_knn)
})

#plotting RMSE vs k and selecting best value
plot(x = ks, y = RMSE_knn, xlab = "k", ylab = "RMSE")
ind <- which.min(RMSE_knn)
k_best <- ks[ind]


#calculating the predictions from that result for use later
fit_knn <- train(x_train, y_train, method = "knn", tuneGrid = data.frame("k" = k_best))
pred_knn <- predict(fit_knn)

#calculate RMSE of best knn
RMSE_knn <- RMSE(y_train, pred_knn)
RMSE_tab <- RMSE_tab %>% add_row("Method" = "KNN", "RMSE" = RMSE_knn)

#show results in table
kable(RMSE_tab, caption = "Model RMSEs (Training)", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")
#################


#################Random Forest Train Set#################
# fit the model with 38 (number of variables we have) different values of mtry
m <- seq(1,38)
RMSE_rf <- sapply(m, function(x){
  fit_rf <- train(x_train, y_train, method = "rf", 
                  tuneGrid = data.frame("mtry" = x))
  pred_rf <- predict(fit_rf)
  RMSE(y_train, pred_rf)
})
#plot to see which is best
plot(x = m, y = RMSE_rf, xlab = "mtry", ylab = "RMSE")
mtry_best <- which.min(RMSE_rf)

# fit model with best mtry
fit_rf <- train(x_train,y_train, method = "rf", tuneGrid = data.frame("mtry" = mtry_best),
                nodesize = 2, ntree = 2000)

#predictions
pred_rf <- predict(fit_rf)
#Calculating RMSE based on predictions
RMSE_rf <- RMSE(y_train, pred_rf)

#create table to see top predictors
tab <- data_frame(rownames(fit_rf$finalModel$importance), fit_rf$finalModel$importance) %>%
  arrange(desc(fit_rf$finalModel$importance[,"IncNodePurity"])) %>% slice(n = 1:10)
names(tab) <- c("Predictor", "Importance")

kable(tab, caption = "Variable Importance", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")


#show RMSE results in table
RMSE_tab <- RMSE_tab %>% add_row("Method" = "Random Forest", "RMSE" = RMSE_rf)
kable(RMSE_tab, caption = "Model RMSEs (Training)", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")
##################


#################Regularization Training Set#################
#use cross validation
train_control <- trainControl(method = "cv")
#select a variety of alphas and lambdas to test
grid <- expand.grid(alpha = seq(0,1,0.1), lambda = seq(0.0001, 1, length = 50))

#fit regularized model
fit_reg <- train(x_train, y_train, method = "glmnet", preProc = c("center", "scale", "nzv", "zv"), tuneGrid = grid)

#selecting the best alpha and lambda parameters from th model
a_best <- fit_reg$bestTune$alpha
l_best <- fit_reg$bestTune$lambda

#fit regularized model with best alpha and lambda
grid <- expand.grid(alpha = a_best, lambda = l_best)
fit_reg <- train(x_train, y_train, method = "glmnet", preProc = c("center", "scale", "nzv", "zv"), tuneGrid = grid)

#predict with optimized parameters and calculate RMSE
pred_reg <- predict(fit_reg)
RMSE_reg <- RMSE(y_train, pred_reg)

#a table showing the best alpha and lambda
df <- data_frame("alpha" = a_best, "lambda" = l_best, "RMSE" = RMSE_reg)
kable(df, caption = "Best Tuning Parameters", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")

#show RMSE results in table
RMSE_tab <- RMSE_tab %>% add_row("Method" = "Regularization", "RMSE" = RMSE_reg)
kable(RMSE_tab, caption = "Model RMSEs (Training)", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")
#################



#################Ensemble Training Set#################
# calculate predictions based on equal weight to each model
pred_ensemble <- (pred_knn + pred_rf + pred_lm + pred_reg)/4

#calculate RMSE
RMSE_ensemble <- RMSE(y_train, pred_ensemble)

#show RMSE results in table
RMSE_tab <- RMSE_tab %>% add_row("Method" = "Ensemble", "RMSE" = RMSE_ensemble)
kable(RMSE_tab, caption = "Model RMSEs (Training)", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")
#################







##############FINAL MODEL PREDICTING WIN PERCENTAGE ON TEST SET USING ENSEMBLE###########
#fit linear regression model on test set
fit_lm_test <- train(x_test, y_test, method = "lm", tuneGrid = data.frame("intercept" = 0))
# predict win percentage based on lm
pred_lm_test <- predict(fit_lm_test)


#fit knn on test set with best k
fit_knn_test <- train(x_test, y_test, method = "knn", tuneGrid = data.frame("k" = k_best))
#predict win percentage based on knn
pred_knn_test <- predict(fit_knn_test)


# fit rf on test set with best mtry
fit_rf_test <- train(x_test, y_test, method = "rf", tuneGrid = data.frame("mtry" = mtry_best),
                     nodesize = 2, ntree = 2000)
#predict win percentage based on rf
pred_rf_test <- predict(fit_rf_test)


# fit regularized model on test set with best alpha and lambda
#selecting the best alpha and lambda parameters from th model
grid <- expand.grid(alpha = a_best, lambda = l_best)
fit_reg_test <- train(x_test, y_test, method = "glmnet", preProc = c("center", "scale", "nzv", "zv"), tuneGrid = grid)
#predict win percentage bsed on regularization
pred_reg_test <- predict(fit_reg_test)


#final ensemble prediction for win percentage
pred_ensemble_test <- (pred_lm_test + pred_knn_test + pred_rf_test + pred_reg_test)/4

#final ensemble RMSE
RMSE_ensemble_test <- RMSE(y_test, pred_ensemble_test)

#making a table
RMSE_tab_test <- data_frame("Method" = "Ensemble", "RMSE" = RMSE_ensemble_test)

#plot how well predictions fit the data
df <- data_frame(y_test, pred_ensemble_test)
df %>% ggplot(aes(x = y_test, y = pred_ensemble_test)) + geom_point() + geom_abline()

#show table of results
kable(RMSE_tab_test, caption = "Final Model RMSE (Test Set)", booktabs = T, linesep = "", digits = 3) %>%
  kable_styling(latex_options = "striped")