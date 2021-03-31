
########################################################################################################################
########################################################################################################################
#############################     USER WRITTEN FUNCTIONS EMPLOYED IN analysis.R SCRIPT    ##############################
########################################################################################################################
########################################################################################################################


# COMMENT:
# This script contains all of the user written functions that are called in the analysis.R script. The functions are:
#     1 - prepare_repair_data()
#     2 - prepare_other_data()
#     3 - gen_train_test()
#     4 - cv_folds()
#     5 - mse()
#     6 - rmse()
#     7 - mae()
#     8 - rsq()
#     9 - generate_accuracy()
#     10 - append_accuracy()
#     11 - report_accuracy()
#     12 - nn_plot()
# A short description is provided above each each function.


########################################################################################################################
# Function that prepares the repair cost data for ML and regression models:
# Inputs into function:
#     1 - Cleaned strike data frame.
#     2 - Logical for whether destroyed aircraft observations should be included.
# Outputs of function:
#     1 - model_df is a data frame with the non-missing repair cost observations and the explanatory features.
#     2 - missing_df is a data frame with the missing repair cost observations and the explanatory features.

prepare_repair_data <- function(df,drop_destroyed) {
   
  # Subset to damaging strikes with non-zero or missing reported repair costs:
  df <- subset(df, damage==1)
  
  # Generate log repair costs:
  df$log_cost_real <- log(df$real_cost_repair)
  
  # Remove gliders from data (only a handful of observations - none of which report costs):
  df <- subset(df, arcft_class=='Airplane' | arcft_class=='Helicopter' | is.na(arcft_class))
  
  # Condition applied if destoyed aircraft are to be exluded in the analysis:
  if (drop_destroyed) {
    df <- subset(df, damage_type=='M' | damage_type=='M?' | damage_type=='N' | damage_type=='S' | is.na(damage_type))
  }
  
  # Generate two data frames ( one with reported costs and one with missing costs):
  cost <- subset(df, !is.na(log_cost_real))
  no_cost <- subset(df, is.na(log_cost_real))
  
  # Subsetting the cleaned data into a data frame containing the relevant features for cost prediction.
  feature_vars <- c('arcft_class', 'arcft_mass', 'number_seen', 'number_struck', 'damage_type', 'dam_eng1', 
                    'dam_eng2', 'dam_eng3', 'dam_eng4', 'dam_fuse', 'dam_lg', 'dam_light', 'dam_nose', 'dam_other', 
                    'dam_prop', 'dam_rad', 'dam_tail', 'dam_windshld', 'dam_wing_rot', 'strike_effect', 'damage', 
                    'ingested', 'num_engns', 'phase_of_flight', 'animal_size', 'sky', 'time_of_day', 'engn_type', 
                    'warned', 'str_eng1', 'str_eng2', 'str_eng3', 'str_eng4', 'str_fuse', 'str_lg', 'str_light', 
                    'str_nose', 'str_other', 'str_prop', 'str_rad', 'str_tail', 'str_windshld', 'str_wing_rot')
  feature_df_1 <- cost[,feature_vars]         
  feature_df_2 <- no_cost[,feature_vars]  
  
  
  # Separate features into those that are dummies and those that are factor variables that will need to be dimmuied:
  nondum_vars <- c('dam_eng1', 'dam_eng2', 'dam_eng3', 'dam_eng4', 'dam_fuse', 'dam_lg', 'dam_light',
                   'dam_nose', 'dam_other', 'dam_prop', 'dam_rad', 'dam_tail', 'dam_windshld', 'dam_wing_rot', 
                   'str_eng1', 'str_eng2', 'str_eng3', 'str_eng4', 'str_fuse', 'str_lg', 'str_light', 'str_nose', 
                   'str_other', 'str_prop', 'str_rad', 'str_tail', 'str_windshld', 'str_wing_rot', 'ingested')
  dum_vars <- c('arcft_class', 'arcft_mass', 'number_seen', 'number_struck', 'damage_type', 'strike_effect', 
                'num_engns', 'phase_of_flight', 'animal_size', 'sky', 'time_of_day', 'engn_type', 'warned')
  
  # Dummying the data and replacing the NA's with zeros since we now have a dummy variable for missing values:
  feature_df_dummies_1 <- as.character(feature_df_1[,dum_vars])
  feature_df_dummies_1 <- data.frame(dummy_cols(feature_df_1[,dum_vars]))
  feature_df_dummies_1 <- feature_df_dummies_1[ , !(names(feature_df_dummies_1) %in% dum_vars)]
  feature_df_dummies_1 [is.na(feature_df_dummies_1)] <- 0
  feature_df_dummies_2 <- as.character(feature_df_2[,dum_vars])
  feature_df_dummies_2 <- data.frame(dummy_cols(feature_df_2[,dum_vars]))
  feature_df_dummies_2 <- feature_df_dummies_2[ , !(names(feature_df_dummies_2) %in% dum_vars)]
  feature_df_dummies_2 [is.na(feature_df_dummies_2)] <- 0
  
  # Merging the dummied data and the data that did not need to be dummied back together:
  feature_df_1 <- data.frame(cbind(feature_df_dummies_1, feature_df_1[nondum_vars]))
  feature_df_2 <- data.frame(cbind(feature_df_dummies_2, feature_df_2[nondum_vars]))
  
  # Dropping unnecessary variables and data frame objects from environment.
  rm(dum_vars, feature_vars, nondum_vars, feature_df_dummies_1, feature_df_dummies_2)
  
  # Add the target variable (costs) and the strike ID number:
  model_df <- data.frame(cbind(cost$index_nr, cost$log_cost_real, feature_df_1))
  model_df <- rename(model_df, index_nr = cost.index_nr, log_cost_real = cost.log_cost_real)
  missing_df <- data.frame(cbind(no_cost$index_nr, no_cost$log_cost_real, feature_df_2))
  missing_df <- rename(missing_df, index_nr = no_cost.index_nr, log_cost_real = no_cost.log_cost_real)
  
  # Return data frames for analysis:
  model_df <<- model_df
  missing_df <<- missing_df
  
}

########################################################################################################################


########################################################################################################################
# Function that prepares the other cost data for ML and regression models:
# Inputs into function:
#     1 - Cleaned strike data frame.
#     2 - Logical for whether destroyed aircraft observations should be included.
# Outputs of function:
#     1 - model_df is a data frame with the non-missing other cost observations and the explanatory features.
#     2 - missing_df is a data frame with the missing other cost observations and the explanatory features.

prepare_other_data <- function(df,drop_destroyed) {
  
  # Replace reported other costs of $0 with $0.01 to enable log transformation of other cost variable:
  df$real_cost_other[df$real_cost_other==0] <- 0.01
  df$log_cost_real <- log(df$real_cost_other)
  
  # Remove gliders from data (only a handful of observations - none of which report costs):
  df <- subset(df, arcft_class=='Airplane' | arcft_class=='Helicopter' | is.na(arcft_class))
  
  # Condition applied if destoyed aircraft are to be exluded in the analysis:
  if (drop_destroyed) {
    df <- subset(df, damage_type=='M' | damage_type=='M?' | damage_type=='N' | damage_type=='S' | is.na(damage_type))
  }
  
  # Generate two data frames ( one with reported costs and one with missing costs):
  cost <- subset(df, !is.na(log_cost_real))
  no_cost <- subset(df, is.na(log_cost_real))
  
  # Subsetting the cleaned data into a data frame containing the relevant features for cost prediction.
  feature_vars <- c('arcft_class', 'arcft_mass', 'number_seen', 'number_struck', 'damage_type', 'dam_eng1', 
                    'dam_eng2', 'dam_eng3', 'dam_eng4', 'dam_fuse', 'dam_lg', 'dam_light', 'dam_nose', 'dam_other', 
                    'dam_prop', 'dam_rad', 'dam_tail', 'dam_windshld', 'dam_wing_rot', 'strike_effect', 'damage', 
                    'ingested', 'num_engns', 'phase_of_flight', 'animal_size', 'sky', 'time_of_day', 'engn_type', 
                    'warned', 'str_eng1', 'str_eng2', 'str_eng3', 'str_eng4', 'str_fuse', 'str_lg', 'str_light', 
                    'str_nose', 'str_other', 'str_prop', 'str_rad', 'str_tail', 'str_windshld', 'str_wing_rot',
                    'damage')
  feature_df_1 <- cost[,feature_vars]         
  feature_df_2 <- no_cost[,feature_vars]  
  
  
  # Separate features into those that are dummies and those that are factor variables that will need to be dimmuied:
  nondum_vars <- c('dam_eng1', 'dam_eng2', 'dam_eng3', 'dam_eng4', 'dam_fuse', 'dam_lg', 'dam_light',
                   'dam_nose', 'dam_other', 'dam_prop', 'dam_rad', 'dam_tail', 'dam_windshld', 'dam_wing_rot', 
                   'str_eng1', 'str_eng2', 'str_eng3', 'str_eng4', 'str_fuse', 'str_lg', 'str_light', 'str_nose', 
                   'str_other', 'str_prop', 'str_rad', 'str_tail', 'str_windshld', 'str_wing_rot', 'ingested',
                   'damage')
  dum_vars <- c('arcft_class', 'arcft_mass', 'number_seen', 'number_struck', 'damage_type', 'strike_effect', 
                'num_engns', 'phase_of_flight', 'animal_size', 'sky', 'time_of_day', 'engn_type', 'warned')
  
  # Dummying the data and replacing the NA's with zeros since we now have a dummy variable for missing values:
  feature_df_dummies_1 <- as.character(feature_df_1[,dum_vars])
  feature_df_dummies_1 <- data.frame(dummy_cols(feature_df_1[,dum_vars]))
  feature_df_dummies_1 <- feature_df_dummies_1[ , !(names(feature_df_dummies_1) %in% dum_vars)]
  feature_df_dummies_1 [is.na(feature_df_dummies_1)] <- 0
  feature_df_dummies_2 <- as.character(feature_df_2[,dum_vars])
  feature_df_dummies_2 <- data.frame(dummy_cols(feature_df_2[,dum_vars]))
  feature_df_dummies_2 <- feature_df_dummies_2[ , !(names(feature_df_dummies_2) %in% dum_vars)]
  feature_df_dummies_2 [is.na(feature_df_dummies_2)] <- 0
  
  # Merging the dummied data and the data that did not need to be dummied back together:
  feature_df_1 <- data.frame(cbind(feature_df_dummies_1, feature_df_1[,nondum_vars]))
  feature_df_2 <- data.frame(cbind(feature_df_dummies_2, feature_df_2[,nondum_vars]))
  
  # Dropping unnecessary variables and data frame objects from environment.
  rm(dum_vars, feature_vars, nondum_vars, feature_df_dummies_1, feature_df_dummies_2)
  
  # Add the target variable (costs) and the strike ID number:
  model_df <- data.frame(cbind(cost$index_nr, cost$log_cost_real, feature_df_1))
  model_df <- rename(model_df, index_nr = cost.index_nr, log_cost_real = cost.log_cost_real)
  missing_df <- data.frame(cbind(no_cost$index_nr, no_cost$log_cost_real, feature_df_2))
  missing_df <- rename(missing_df, index_nr = no_cost.index_nr, log_cost_real = no_cost.log_cost_real)
  
  # Return data frames for analysis:
  model_df <<- model_df
  missing_df <<- missing_df
  
}

########################################################################################################################


########################################################################################################################
# Function that shuffles the model ready data and generates the test/train split. Split = number between 0 and 1
# that is the chosen size of the train set. It produces a plot that compares the distribution of the target variable 
# for the overall, train, and test sets.
# Inputs into function:
#     1 - model_df data frame generated by prepare_repair_data() or prepare_other_data().
#     2 - Number between 0 and 1 that indicates the size of the training data (e.g., 0.8 means 80% of data).
# Outputs of function:
#     1 - train_df is a data frame for the predictive models to be trained on.
#     2 - test_df is a data frame for the predictive models to be tested on.
#     3 - Produces a plot that compares the distribution of the target variable for the overall, train, and test sets.

gen_train_test <- function(df,split) {
  
  set.seed(123)
  
  # Shuffle observations in the model ready data.
  rows <- sample(nrow(df))
  df <- df[rows,]
  rm(rows)
  
  # Splitting data on selected percentage.
  sample_size = floor(split*nrow(df))
  selected = sample(seq_len(nrow(df)),size = sample_size)
  train_df = df[selected,]
  test_df = df[-selected,]
  rm(sample_size, selected)
  
  # Return split and comparison plot.
  plot(density(model_df$log_cost_real, width = 0.25), col = 'green',
       main = 'Distribution of Target Variable in the Full, Train, and Test Sets',
       xlab = 'Natural Log of Real Repair Cost')
  lines(density(train_df$log_cost_real, width = 0.25), col = 'blue')
  lines(density(test_df$log_cost_real, width = 0.25), col = 'red',)
  legend("topleft", legend = c(paste('Full = ', dim(model_df)[1], sep = ''),
                               paste('Train = ', dim(train_df)[1], sep = ''),
                               paste('Test = ', dim(test_df)[1], sep = '')),
         col = c('green', 'blue', 'red'),
         pch = c(15,15,15), 
         bty = "n", 
         pt.cex = 2, 
         cex = 1.2, 
         inset = c(0.1, 0.1))
  
  test_df <<- test_df
  train_df <<- train_df
  
}

########################################################################################################################


########################################################################################################################
# This function takes in the training data frame and a specified number of folds for CV. It returns a list object called
# fold_indx that contains the row indices in the training data that correspond to each fold.

cv_folds <- function(df,folds) {
  set.seed(123)
  fold_indx <- createFolds(df$log_cost_real, k = folds)
  fold_indx <<- fold_indx
}

########################################################################################################################


########################################################################################################################
# Functions for accuracy measures used for model evaluation: mean squared error, root mean squared error, mean
# absolute error, and R-squared.

mse <- function(true,pred){
  return(mean((true-pred)*(true-pred)))
}

rmse <- function(true,pred){
  return(sqrt(mean((true-pred)*(true-pred))))
}

mae <- function(true,pred){
  return(mean(abs(true-pred)))
}

rsq <- function(true,pred){
  SSR = sum((true-pred)*(true-pred))
  SST = sum((true - mean(true))*(true - mean(true)))
  return(1 - (SSR/SST))
}

########################################################################################################################
# This function generates empty vectors to be populated by accuracy measures.

generate_accuracy <- function() {
  mean_sq_err <<- c()
  rt_mean_sq_err <<- c()
  mean_abs_err <<- c()
  r_square <<- c()
}

########################################################################################################################


########################################################################################################################
# This function takes in the validation values and predicted values, genereates our accuracy measures, and 
# appends these measures to the vectors generated in the generate_accuracy() function.

append_accuracy <- function(true,pred) {
  mean_sq_err <<- append(mean_sq_err, mse(true,pred))
  rt_mean_sq_err <<- append(rt_mean_sq_err, rmse(true,pred))
  mean_abs_err <<- append(mean_abs_err, mae(true,pred))
  r_square <<- append(r_square, rsq(true,pred))
}

########################################################################################################################


########################################################################################################################
# This function reports the mean and standard deviation of the accuracy measures across the CV folds.

report_accuracy <- function() {
  report <- cbind(c('MEAN_SQUARE_ERROR', 'ROOT_MEAN_SQUARE_ERROR', 'MEAN_ABSOLUTE_ERROR', 'R_SQUARED'),
                  c(mean(mean_sq_err), mean(rt_mean_sq_err), mean(mean_abs_err), mean(r_square)),
                  c(sd(mean_sq_err), sd(rt_mean_sq_err), sd(mean_abs_err), sd(r_square)))
  colnames(report) <- c('Accuracy Measure', 'Mean', 'S.D.')
  return(report)
}

########################################################################################################################

########################################################################################################################
# This function takes the neural netwrk output (k = 10) and returns a plot with the specified y range.

nn_plot <- function(ymin,ymax) {
  plot(history1$metrics$mse, ylim=c(ymin,ymax), type='l', col = 'blue', lwd = 2, ylab = 'MSE', xlab = 'Epoch')
  lines(history1$metrics$val_mse, col = 'red', lwd = 2)
  lines(history2$metrics$mse, col = 'blue', lwd = 2)
  lines(history2$metrics$val_mse, col = 'red', lwd = 2)
  lines(history3$metrics$mse, col = 'blue', lwd = 2)
  lines(history3$metrics$val_mse, col = 'red', lwd = 2)
  lines(history4$metrics$mse, col = 'blue', lwd = 2)
  lines(history4$metrics$val_mse, col = 'red', lwd = 2)
  lines(history5$metrics$mse, col = 'blue', lwd = 2)
  lines(history5$metrics$val_mse, col = 'red', lwd = 2)
  lines(history6$metrics$mse, col = 'blue', lwd = 2)
  lines(history6$metrics$val_mse, col = 'red', lwd = 2)
  lines(history7$metrics$mse, col = 'blue', lwd = 2)
  lines(history7$metrics$val_mse, col = 'red', lwd = 2)
  lines(history8$metrics$mse, col = 'blue', lwd = 2)
  lines(history8$metrics$val_mse, col = 'red', lwd = 2)
  lines(history9$metrics$mse, col = 'blue', lwd = 2)
  lines(history9$metrics$val_mse, col = 'red', lwd = 2)
  lines(history10$metrics$mse, col = 'blue', lwd = 2)
  lines(history10$metrics$val_mse, col = 'red', lwd = 2)
  legend("topright", legend = c("Training", "Validation"),
         col = c('blue', 'red'),
         pch = c(15,15), 
         bty = "n", 
         pt.cex = 2, 
         cex = 1.2, 
         inset = c(0.1, 0.1))
}

########################################################################################################################
