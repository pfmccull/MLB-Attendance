library(tidyverse)
library(prophet)
library(lubridate)
library(randomForest)
library(caret)
library(xgboost)
library(gridExtra)

# Load file
attendance_raw <- read_csv("attendance_modeling.csv")

load("xgbt.Rdata")
# Select the variables to use in the model
att_mod_red <- attendance_raw %>%
  select(attendance, teams.home.team.name, teams.away.team.name, temperature,
         opener, dayNight, weekday, teams.home.leagueRecord.pct, teams.away.leagueRecord.pct,
         team.home.one.behind.win.pct, team.away.one.behind.win.pct,
         team.home.two.behind.win.pct, team.away.two.behind.win.pct,
         teams.home.seriesNumber,
         other_weather) %>%
  unclass() %>%
  as.data.frame()

# Make series number a factor as a proxy for time of year
att_mod_red$teams.home.seriesNumber <- cut(att_mod_red$teams.home.seriesNumber, breaks = seq(from = 0, to = 55, by = 5))

# Make variable names easier for display purposes
colnames(att_mod_red) <- c("Attendance",
                           "Home.Team.Name",
                           "Away.Team.Name",
                           "Temperature",
                           "Opening.Day",
                           "Game.Time",
                           "Day.of.Week",
                           "Home.Team.Current.Winning.Percentage",
                           "Away.Team.Current.Winning.Percentage",
                           "Home.Team.Previous.Season.Winning.Percentage",
                           "Away.Team.Previous.Season.Winning.Percentage",
                           "Home.Team.Winning.Percentage.Two.Years.Prior",
                           "Away.Team.Winning.Percentage.Two.Years.Prior",
                           "Series.Number",
                           "Weather")


## Create training and testing data frames
# Set seed for reproducibility
set.seed(1842)
# Get 80% of the rows to use as the training set
n <- nrow(att_mod_red)
train_n <- sample.int(n, .8*n)

# Create the test/train sets
att_train <- att_mod_red[train_n,]
att_test <- att_mod_red[-train_n,]

# Create the models
### Model parameters were tuned in "Attendence Modeling.Rmd"

# Linear Model
mod_lm <- lm(data = att_train, Attendance~.)

# Random Forest Model
mod_rf <- train(Attendance~.,
                  data = att_train,
                  method = "ranger",
                  importance = "impurity",
                  trControl = trainControl(method = "none"),
                  tuneGrid = expand.grid(.mtry = 14,
                                         .splitrule = "variance",
                                         .min.node.size = 2))

# XGBoost Linear Model
mod_xgbl <- train(Attendance~.,
                  data = att_train,
                  method = "xgbLinear",
                  trControl = trainControl(method = "none"),
                  tuneGrid = expand.grid(.nrounds = 275,
                                         .lambda = 1,
                                         .alpha = 1,
                                         .eta = .01))

# XGBoost Tree Model
mod_xgbt <- train(Attendance~.,
                data = att_train,
                method = "xgbTree",
                trControl = trainControl(method = "none"),
                tuneGrid = expand.grid(.nrounds = 1500,
                                       .max_depth = 6,
                                       .eta = .05,
                                       .gamma = 0,
                                       .colsample_bytree = .5,
                                       .min_child_weight = 1,
                                       .subsample = .8))
# Glm Model
mod_glm <- train(Attendance~.,
                   data = att_train,
                    method = "glmnet",
                    trControl = trainControl(method = "none"),
                    tuneGrid = expand.grid(.alpha = 1,
                                            .lambda = 1))


# Get predictions
predictions <- cbind(att = att_test[,1],
                     lm.pred = predict(mod_lm, att_test[,-1]),
                     rf.pred = predict(mod_rf, att_test[,-1]),
                     xgbl.pred = predict(mod_xgbl, att_test[,-1]),
                     xgbt.pred = predict(mod_xgbt, att_test[,-1]),
                     glm.pred = predict(mod_glm, att_test[,-1])) %>% as.data.frame()

## Get RMSEs
# Set up data frame
RMSE_dat <- data.frame(Model = c("Linear",
                                 "Random Forest", 
                                 "XGBoost Linear",
                                 "XGBoost Tree",
                                 "GLM"),
                       RMSE = rep(0, 5))

# Get the RMSE for each model
RMSE_dat[RMSE_dat$Model == "Linear", "RMSE"] <- RMSE(predictions$lm.pred, predictions$att)
RMSE_dat[RMSE_dat$Model == "Random Forest", "RMSE"] <- RMSE(predictions$rf.pred, predictions$att)
RMSE_dat[RMSE_dat$Model == "XGBoost Linear", "RMSE"] <- RMSE(predictions$xgbl.pred, predictions$att)
RMSE_dat[RMSE_dat$Model == "XGBoost Tree", "RMSE"] <- RMSE(predictions$xgbt.pred,predictions$att)
RMSE_dat[RMSE_dat$Model == "GLM", "RMSE"] <- RMSE(predictions$glm.pred, predictions$att)

# Arrange
RMSE_dat <- RMSE_dat %>%
  arrange(RMSE) %>%
  mutate(RMSE = round(RMSE, 2)) %>%
  select(Model, RMSE)

# Create RMSE table graphic
RMSE_dat %>%
  gridExtra::grid.table(rows = NULL)+
  gridExtra::ttheme_minimal()


# Plot residuals vs predicted values
predictions %>%
  ggplot(aes(x = xgbt.pred, y = -xgbt.pred + att))+
  geom_point()+
  geom_smooth(se = F)+
  theme_minimal()+
  labs(title = "XGBoost Tree Model", x = "Predicted Values", y = "Residuals")+
  scale_x_continuous(breaks = c(10000, 20000, 30000, 40000, 50000),
                     labels = c("10,000", "20,000", "30,000", "40,000", "50,000"))+
  scale_y_continuous(breaks = c(-20000, -10000, 0, 10000, 20000),
                     labels = c("-20,000", "-10,000", "0", "10,000", "20,000"))+
  theme(plot.title = element_text(hjust = .5))

# Get the variable imoprtance of each model
import <-  cbind(varImp(mod_lm),varImp(mod_rf)$importance) %>%
  cbind(varImp(mod_xgbl)$importance) %>%
  cbind(varImp(mod_xgbt)$importance) %>%
  cbind(varImp(mod_glm)$importance)

# name the appropriate columns of each model
colnames(import) <- c("lm", "rf", "xgbl", "xgbt", "glm")

# Add row names as a column
import$variable <- rownames(import)

# For dummy variables/factors get the original variable name
for(x in 1:length(colnames(att_test[,-1]))){
  import$variable[str_detect(import$variable, colnames(att_test[,-1])[x])] <- colnames(att_test[,-1])[x]
}

# Remove row names
rownames(import) <- NULL

# Get the scaled importantance of each variable for each model
import_sum <- import %>%
  gather(value = "imp", key = "model", lm, rf, xgbl, xgbt, glm) %>%
  group_by(variable, model) %>%
  summarise(Imp = sum(imp)) %>%
  ungroup() %>%
  group_by(model) %>%
  mutate(TotalImp = sum(Imp)) %>%
  ungroup() %>%
  mutate(imp.scale = Imp/TotalImp) %>%
  reshape2::dcast(variable~model, value.var = "imp.scale")

# plot the importance of each variable of the XGBoost Tree model
import_sum %>%
  select(variable, xgbt) %>%
  mutate(variable = str_replace_all(variable, "\\.", " ")) %>%
  ggplot(aes(x = reorder(variable,xgbt), y = xgbt))+
  geom_col(fill = "dodgerblue4")+
  labs(title = "XGBoost Tree Model Variable Importance", x = "Variable")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(name = "Variable importance", 
                     breaks = c(0, .2, .4, .6, .8),
                     labels = c("0%", "20%", "40%", "60%", "80%"))+
  theme_minimal()+
  coord_flip()

# Importance of the variables minus teams
import_sum %>%
  select(variable, xgbt) %>%
  filter(!variable %in% c("Home.Team.Name", "Away.Team.Name")) %>%
  mutate(variable = str_replace_all(variable, "\\.", " ")) %>%
  ggplot(aes(x = reorder(variable,xgbt), y = xgbt))+
  geom_col(fill = "dodgerblue4")+
  labs(title = "XGBoost Tree Model Variable Importance\n(Without the Home and Away Teams)", x = "Variable")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(name = "Variable importance", 
                     breaks = c(0, .002, .004, .006, .008),
                     labels = c("0%", ".2%", ".4%", ".6%", ".8%"))+
  theme_minimal()+
  coord_flip()

                     
# Average attendance by date 
attendance_raw %>%
  mutate(samedate = as.Date(paste("2019", month(gameDate), day(gameDate), sep = "-"), format = "%Y-%m-%d")) %>%
  group_by(samedate) %>%
  summarise(avg.att = mean(attendance), counts = n()) %>%
  ggplot()+
  geom_point(aes(x = samedate, y = avg.att,), size = 3, color = "dodgerblue4")+
  theme_minimal()+
  labs(title = "Average Attendance by Date")+
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%B")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(name = "Average Attendance", 
                     breaks = c(27500, 30000, 32500, 35000, 37500, 40000),
                     labels = c("27,500", "30,000", "32,500", "35,000", "37,500", "40,000"))
  
# Factorize weekdays in order for plotting
attendance_raw$weekday <- factor(attendance_raw$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Average attendance by day of week
attendance_raw %>% 
  group_by(weekday) %>%
  summarise(avg.att = mean(attendance)) %>%
  ggplot(aes(x = weekday, y = avg.att))+
  geom_col(fill = "dodgerblue4")+
  theme_minimal()+
  labs(title = "Average Attendance by Day of Week", x = "")+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(name = "Average Attendance",
                     breaks = c(10000, 20000, 30000),
                     labels = c("10,000", "20,000", "30,000"))
