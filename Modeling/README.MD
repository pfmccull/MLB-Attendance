The code and data in this folder is used to model the attendance for MLB games using data from the 2010-2019 MLB seasons. 
"attendance_data_merge.R" pulls team records from baseballreference for the prior two seasons and removes rescheduled games from 
the data set. "Attendance Modeling.Rmd" finds the optimal parameters for the linear, GLM, random forest, XGBoost Linear, and XGBoost Tree models. "attendance_modeling_inference.R" evaulates the performance of each model and explores the importance of variables in the best model (XGBoost Tree). 

A write-up of the analysis that this code is used for can be found here: https://www.tammanyball.com/2020/03/predicting-mlb-attendance-modeling.html
