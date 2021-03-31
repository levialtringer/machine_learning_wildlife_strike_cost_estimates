
########################################################################################################################
########################################################################################################################
################     ESTIMATING WILDLIFE STRIKE COSTS AT US AIRPORTS: A MACHINE LEARNING APPROACH    ###################
########################################################################################################################
########################################################################################################################


# COMMENT:
# This script employs the strike_data_raw_x.csv files to perform the analysis in "Estimating Wildlife Strike Costs at 
# US Airports: A Machine Learning Approach". The code is annotated for ease of replication.


# Clear environment:
rm(list = ls())

# Load required libraries:
library(tidyverse)
library(dplyr)
library(car)
library(hms)
library(psych)
library(stringr)
library(countrycode)
library(USAboundaries)
library(doBy)
library(scales)
library(fastDummies)
library(foreach)
library(gridExtra)
library(ggplot2)
library(viridis)
library(reshape2)
library(caret)
library(keras)
library(tensorflow)
library(randomForest)

# Run the clean_nwsd.R script to get cleaned NWSD data (last accessed on April 15th, 2020):
source("clean_nwsd.R")
# Bring in the user written functions defined in the functions.R script:
source("functions.R")

########################################################################################################################
# FIGURE 1: Strike and total costs reported to the NWSD, 1990-2018

# Create copy of data frame for plot:
plot_df <- strike

# Generate variable that indicates disruptive strikes:
plot_df$repair <- ifelse(is.na(plot_df$real_cost_repair), 0, 
                         ifelse(plot_df$real_cost_repair==0, 0, 1))
plot_df$other <- ifelse(is.na(plot_df$real_cost_other), 0,
                        ifelse(plot_df$real_cost_other==0, 0, 1))
plot_df$neof <- ifelse(is.na(plot_df$strike_effect), 0,
                       ifelse(plot_df$strike_effect=='None', 0, 1))
plot_df$downtime <- ifelse(is.na(plot_df$hours_aos), 0, 
                           ifelse(plot_df$hours_aos==0, 0, 1))
plot_df$disruptive <- ifelse(plot_df$repair==1 | 
                               plot_df$other==1 |
                               plot_df$neof==1 |
                               plot_df$downtime==1 |
                               plot_df$damage==1, 1, 0)

# Sum reported costs and strike by year (subset to 1990-2018 period):
plot_df <- summaryBy(real_cost_repair + real_cost_other + damage + disruptive ~ incident_year, 
                     FUN=c(sum), data=plot_df, na.rm=TRUE, keep.names = TRUE)
plot_df <- subset(plot_df, incident_year>1989 & incident_year<2019)

# Plot:
ggplot(plot_df, aes(x = incident_year)) + 
  geom_line(aes(y = (real_cost_repair + real_cost_other)/1000000, colour = "Reported total costs")) + 
  geom_line(aes(y = disruptive/38, colour = 'Disruptive strikes')) + 
  geom_line(aes(y = damage/38, colour = 'Damaging strikes')) + 
  scale_y_continuous(limits = c(0,80), expand = expand_scale(mult=c(0.0015,0.0015)), breaks = seq(0,100,20),
                     sec.axis = sec_axis(~.*38, name = "Number of strikes reported\n",
                                         labels = comma, breaks = seq(0,3000,500))) + 
  labs(y = "Reported total costs (millions of 2018 $)\n", x = "\n\nYear") + 
  theme_bw() + 
  theme(legend.position = 'top', legend.direction = 'horizontal', legend.title = element_blank()) +
  scale_x_continuous(expand = expand_scale(mult=c(0.0015,0.0015)), 
                     breaks = c(1990,1995,2000,2005,2010,2015),
                     minor_breaks = seq(1990, 2019, 1)) +
  scale_colour_manual(values = c("#73D055FF", "#440154FF", "#287D8EFF"))

# Remove plot data frame from environment:
rm(plot_df)

########################################################################################################################


########################################################################################################################
# FIGURES 2, 3, & 4 - Not produced in R.
########################################################################################################################


########################################################################################################################
# TABLE 1: Repair and other cost summary statistics

# Repair costs:

  # Call prepare_repair_data() and gen_train_test() to get train and test data:
  prepare_repair_data(df = strike, drop_destroyed = FALSE)
  gen_train_test(df = model_df, split = 0.8)
  
  # Number of damaging strike observations with missing repair costs:
  dim(missing_df)[1]

  # Summary statistics:
  r1 <- describe(exp(model_df$log_cost_real))
  r2 <- describe(exp(train_df$log_cost_real))
  r3 <- describe(exp(test_df$log_cost_real))
  r4 <- describe(model_df$log_cost_rea)
  r5 <- describe(train_df$log_cost_rea)
  r6 <- describe(test_df$log_cost_rea)
  data.frame(mean = round(c(r1[,3], r2[,3], r3[,3], r4[,3], r5[,3], r6[,3]), digits = 2),
             sd = round(c(r1[,4], r2[,4], r3[,4], r4[,4], r5[,4], r6[,4]), digits = 2),
             min = round(c(r1[,8], r2[,8], r3[,8], r4[,8], r5[,8], r6[,8]), digits = 2),
             median = round(c(r1[,5], r2[,5], r3[,5], r4[,5], r5[,5], r6[,5]), digits = 2),
             max = round(c(r1[,9], r2[,9], r3[,9], r4[,9], r5[,9], r6[,9]), digits = 2),
             n = round(c(r1[,2], r2[,2], r3[,2], r4[,2], r5[,2], r6[,2]), digits = 2),
             row.names = c("Actual - All", "Actual - Training", "Actual - Test",
                           "Log - All", "Log - Training", "Log - Test"))
 
  # remove summary statistics objects
  rm(model_df, missing_df, train_df, test_df, r1, r2, r3, r4, r5, r6)
  
# Other costs:
  
  # Call prepare_repair_data() and gen_train_test() to get train and test data:
  prepare_other_data(df = strike, drop_destroyed = FALSE)
  gen_train_test(df = model_df, split = 0.8)
  
  # Number of damaging strike observations with missing repair costs:
  dim(missing_df)[1]
  
  # Summary statistics:
  r1 <- describe(exp(model_df$log_cost_real))
  r2 <- describe(exp(train_df$log_cost_real))
  r3 <- describe(exp(test_df$log_cost_real))
  r4 <- describe(model_df$log_cost_rea)
  r5 <- describe(train_df$log_cost_rea)
  r6 <- describe(test_df$log_cost_rea)
  data.frame(mean = round(c(r1[,3], r2[,3], r3[,3], r4[,3], r5[,3], r6[,3]), digits = 2),
             sd = round(c(r1[,4], r2[,4], r3[,4], r4[,4], r5[,4], r6[,4]), digits = 2),
             min = round(c(r1[,8], r2[,8], r3[,8], r4[,8], r5[,8], r6[,8]), digits = 2),
             median = round(c(r1[,5], r2[,5], r3[,5], r4[,5], r5[,5], r6[,5]), digits = 2),
             max = round(c(r1[,9], r2[,9], r3[,9], r4[,9], r5[,9], r6[,9]), digits = 2),
             n = round(c(r1[,2], r2[,2], r3[,2], r4[,2], r5[,2], r6[,2]), digits = 2),
             row.names = c("Actual - All", "Actual - Training", "Actual - Test",
                           "Log - All", "Log - Training", "Log - Test"))
  
  # remove summary statistics objects
  rm(model_df, missing_df, train_df, test_df, r1, r2, r3, r4, r5, r6)
  
########################################################################################################################


########################################################################################################################
# TABLE 2 - Not produced in R.
########################################################################################################################

  
########################################################################################################################
# EXAMPLE OF MODEL TRAINING/TUNING: RANDOM FOREST FOR REPAIR COSTS

# While not included here, we tuned the Random Forest and Neural Networks manually - looping over several combinations
# of hyperparameter specifications. This process is computationally intense and, for ease of replication, we provide a 
# brief example of what model tuning entails, but not the full extent of our search. This example shows how 
# tuning the "ntree" (number of trees to grow) hyperparameter in the Random Forest model affects performance.
  
# Call prepare_repair_data() and gen_train_test() to get train and test data:
prepare_repair_data(df = strike, drop_destroyed = FALSE)
gen_train_test(df = model_df, split = 0.8)

# Generate 10 CV folds in training data:
num_folds = 10
cv_folds(df=train_df,folds=num_folds)


# RANDOM FOREST:

# The Random Forest ML algorithm is performed 10 times, where the model is trained on 90% of the training data and 
# validated on the other 10% in each iteration (see Figure 2 of the paper). Thus, 10 performance metrics emerge, one 
# for each train/validation fold, and we take the average as a measure of the model performance.

# 1 - Random Forest (ntree = 50, mtry = 20, nodesize = 20):
generate_accuracy()
for (i in 1:num_folds) {
  y_train <- train_df[-fold_indx[[i]],2]
  y_valid <- train_df[fold_indx[[i]],2]
  x_train <- train_df[-fold_indx[[i]],3:96]
  x_valid <- train_df[fold_indx[[i]],3:96]
  set.seed(123)
  model <- randomForest(x_train, y = y_train, ntree = 50, mtry = 20, nodesize = 20)
  y_hat <- predict(model, x_valid)
  append_accuracy(y_valid,y_hat)
  rm(x_train,x_valid, y_train, y_valid)
}
report_accuracy()
accuracy50 <- report_accuracy()

# 2 - Random Forest (ntree = 100, mtry = 20, nodesize = 20):
generate_accuracy()
for (i in 1:num_folds) {
  y_train <- train_df[-fold_indx[[i]],2]
  y_valid <- train_df[fold_indx[[i]],2]
  x_train <- train_df[-fold_indx[[i]],3:96]
  x_valid <- train_df[fold_indx[[i]],3:96]
  set.seed(123)
  model <- randomForest(x_train, y = y_train, ntree = 100, mtry = 20, nodesize = 20)
  y_hat <- predict(model, x_valid)
  append_accuracy(y_valid,y_hat)
  rm(x_train,x_valid, y_train, y_valid)
}
report_accuracy()
accuracy100 <- report_accuracy()

# 3 - Random Forest (ntree = 300, mtry = 20, nodesize = 20):
generate_accuracy()
for (i in 1:num_folds) {
  y_train <- train_df[-fold_indx[[i]],2]
  y_valid <- train_df[fold_indx[[i]],2]
  x_train <- train_df[-fold_indx[[i]],3:96]
  x_valid <- train_df[fold_indx[[i]],3:96]
  set.seed(123)
  model <- randomForest(x_train, y = y_train, ntree = 300, mtry = 20, nodesize = 20)
  y_hat <- predict(model, x_valid)
  append_accuracy(y_valid,y_hat)
  rm(x_train,x_valid, y_train, y_valid)
}
report_accuracy()
accuracy300 <- report_accuracy()

# 4 - Random Forest (ntree = 600, mtry = 20, nodesize = 20):
generate_accuracy()
for (i in 1:num_folds) {
  y_train <- train_df[-fold_indx[[i]],2]
  y_valid <- train_df[fold_indx[[i]],2]
  x_train <- train_df[-fold_indx[[i]],3:96]
  x_valid <- train_df[fold_indx[[i]],3:96]
  set.seed(123)
  model <- randomForest(x_train, y = y_train, ntree = 600, mtry = 20, nodesize = 20)
  y_hat <- predict(model, x_valid)
  append_accuracy(y_valid,y_hat)
  rm(x_train,x_valid, y_train, y_valid)
}
report_accuracy()
accuracy600 <- report_accuracy()

# Report the MSE for each model:
paste0("MSE when ntree=50: ", round(as.numeric(accuracy50[1,2]), digits = 4))
paste0("MSE when ntree=100: ", round(as.numeric(accuracy100[1,2]), digits = 4))
paste0("MSE when ntree=300: ", round(as.numeric(accuracy300[1,2]), digits = 4))
paste0("MSE when ntree=600: ", round(as.numeric(accuracy600[1,2]), digits = 4))

# One can see that MSE falls as ntree increases from 50 to 300. But then rises at ntree=600. So we would identify 
# ntree=300 as the best performing model, holding the other hyperparameters constant. However, ntree=600 might produce
# more accurate results if we changed the "mtry" or "nodesize" parameters. So multiple combinations of different values
# for different hyperparameters must be inspected to make a final decision concerning which model produces the best
# predictive performance (i.e., grid search).

# Remove objects from R environment:
rm(accuracy50, accuracy100, accuracy300, accuracy600, model, model_df, missing_df, test_df, train_df, fold_indx)
rm(list = ls.str(mode = 'numeric'))

########################################################################################################################


########################################################################################################################
# FIGURE 5: Model performance on repair cost test set


# WHEN DESTROYED AIRCRAFT ARE EXCLUDED:

# Call prepare_repair_data() and gen_train_test() to get train and test data:
prepare_repair_data(df = strike, drop_destroyed = TRUE)
gen_train_test(df = model_df, split = 0.8)

# Perform 100 draws on test data:
set.seed(123)
draws = 100
test_samples <- createDataPartition(test_df$log_cost_real, times = draws, p = 0.65)

# Run linear regression model on all training data:
model_lm <- lm(log_cost_real ~ ., data = train_df[,2:95])

# Get the performance of the Linear Regression Model on all test set subsamples:
test_lm <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_lm, test_df[test_samples[[i]],3:95])
  test_lm <- rbind(test_lm, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_lm <- cbind(test_lm, c('LR'), c('EXCLUDED'))
colnames(test_lm) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run random forest model on all training data:
set.seed(123)
model_rf <- randomForest(train_df[,3:95], y = train_df[,2], ntree = 300, mtry = 20, nodesize = 20)

# Get the performance of the Random Forest Model on all test set subsamples:
test_rf <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_rf, test_df[test_samples[[i]],3:95])
  test_rf <- rbind(test_rf, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat), 
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_rf <- cbind(test_rf, c('RF'), c('EXCLUDED'))
colnames(test_rf) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run neural network model on all training data:
set.seed(123)
model_nn <- keras_model_sequential() %>% 
  layer_dense(units = 75, activation = 'relu', input_shape = c(93)) %>%
  layer_dense(units = 55, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'linear')
model_nn %>% compile(loss = 'mse', optimizer = 'adam', metrics = list('mse'))
model_nn %>% fit(as.matrix(train_df[,3:95]),
                 as.matrix(train_df[,2]),
                 batch_size = 128,
                 epochs = 20,
                 view_metrics = FALSE)

# Get the performance of the Neural Network Model on all test set subsamples:
test_nn <- c()
for (i in 1:draws) {
  y_hat <- model_nn %>% predict(as.matrix(test_df[test_samples[[i]],3:95]))
  meas <- c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
            mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat))
  test_nn <- rbind(test_nn,meas)
  rm(meas)
}
test_nn <- cbind(test_nn, c('NN'), c('EXCLUDED'))
colnames(test_nn) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_nn) <- c(1:100)

# Combine LR, RF, and NN test set results:
test_excluded <- rbind(test_lm, test_rf, test_nn)
rm(test_lm, test_rf, test_nn, y_hat)


# WHEN DESTROYED AIRCRAFT ARE INCLUDED:

# Call prepare_repair_data() and gen_train_test() to get train and test data:
prepare_repair_data(df = strike, drop_destroyed = FALSE)
gen_train_test(df = model_df, split = 0.8)

# Perform 100 draws on test data:
set.seed(123)
draws = 100
test_samples <- createDataPartition(test_df$log_cost_real, times = draws, p = 0.65)

# Run linear regression model on all training data:
model_lm <- lm(log_cost_real ~ ., data = train_df[,2:96])

# Get the performance of the Linear Regression Model on all test set subsamples:
test_lm <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_lm, test_df[test_samples[[i]],3:96])
  test_lm <- rbind(test_lm, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_lm <- cbind(test_lm, c('LR'), c('INCLUDED'))
colnames(test_lm) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run random forest model on all training data:
set.seed(123)
model_rf <- randomForest(train_df[,3:96], y = train_df[,2], ntree = 300, mtry = 20, nodesize = 20)

# Get the performance of the Random Forest Model on all test set subsamples:
test_rf <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_rf, test_df[test_samples[[i]],3:96])
  test_rf <- rbind(test_rf, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_rf <- cbind(test_rf, c('RF'), c('INCLUDED'))
colnames(test_rf) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run neural network model on all training data:
set.seed(123)
model_nn <- keras_model_sequential() %>% 
  layer_dense(units = 75, activation = 'relu', input_shape = c(94)) %>%
  layer_dense(units = 55, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'linear')
model_nn %>% compile(loss = 'mse', optimizer = 'adam', metrics = list('mse'))
model_nn %>% fit(as.matrix(train_df[,3:96]),
                 as.matrix(train_df[,2]),
                 batch_size = 128,
                 epochs = 20,
                 view_metrics = FALSE)

# Get the performance of the Neural Network Model on all test set subsamples:
test_nn <- c()
for (i in 1:draws) {
  y_hat <- model_nn %>% predict(as.matrix(test_df[test_samples[[i]],3:96]))
  meas <- c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
            mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat))
  test_nn <- rbind(test_nn,meas)
  rm(meas)
}
test_nn <- cbind(test_nn, c('NN'), c('INCLUDED'))
colnames(test_nn) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_nn) <- c(1:100)

# Combine LR, RF, and NN test set results:
test_included <- rbind(test_lm, test_rf, test_nn)
rm(test_lm, test_rf, test_nn, y_hat, test_df, train_df, missing_df, model_df, model_lm, model_rf, model_nn,
   test_samples)


# COMBINE ALL TEST SET ACCURACY:
test_accuracy_repair <- data.frame(rbind(test_excluded, test_included))
test_accuracy_repair$MSE <- as.numeric(test_accuracy_repair$MSE)
test_accuracy_repair$RMSE <- as.numeric(test_accuracy_repair$RMSE)
test_accuracy_repair$MAE <- as.numeric(test_accuracy_repair$MAE)
test_accuracy_repair$R.SQUARE <- as.numeric(test_accuracy_repair$R.SQUARE)
rownames(test_accuracy_repair) <- c()
rm(test_excluded, test_included)


# PLOT TEST SET ACCURACY:

get_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

scaleFUN <- function(x) sprintf("%.3f", x)

p1 <- ggplot(subset(test_accuracy_repair, DESTROYED=='INCLUDED'),
             aes(MSE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1,) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Square Error') + 
  theme(legend.position = "none") + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p2 <- ggplot(subset(test_accuracy_repair, DESTROYED=='EXCLUDED'),
             aes(MSE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Square Error') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p3 <- ggplot(subset(test_accuracy_repair, DESTROYED=='INCLUDED'),
             aes(MAE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Absolute Error') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p4 <- ggplot(subset(test_accuracy_repair, DESTROYED=='EXCLUDED'),
             aes(MAE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Absolute Error') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p5 <- ggplot(subset(test_accuracy_repair, DESTROYED=='INCLUDED'),
             aes(R.SQUARE, fill = MODEL, colour = MODEL)) + scale_x_continuous(labels=scaleFUN) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nR-Squared') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif')) + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p6 <- ggplot(subset(test_accuracy_repair, DESTROYED=='EXCLUDED'),
             aes(R.SQUARE, fill = MODEL, colour = MODEL)) + scale_x_continuous(labels=scaleFUN) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nR-Squared') + 
  theme(legend.position = 'bottom', legend.title = element_blank()) + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif')) + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
legend <- get_legend(p6)
p6 <- p6 + theme(legend.position = "none")

grid.arrange(arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=2, nrow = 3), legend, ncol=1, nrow = 2, heights=c(10, 1))

# Remove plot objects:
rm(p1,p2,p3,p4,p5,p6,legend)

########################################################################################################################


########################################################################################################################
# FIGURE 6: Model performance on other cost test set


# WHEN DESTROYED AIRCRAFT ARE EXCLUDED:

# Call prepare_repair_data() and gen_train_test() to get train and test data:
prepare_other_data(df = strike, drop_destroyed = TRUE)
gen_train_test(df = model_df, split = 0.8)

# Perform 100 draws on test data:
set.seed(123)
draws = 100
test_samples <- createDataPartition(test_df$log_cost_real, times = draws, p = 0.65)

# Run linear regression model on all training data:
model_lm <- lm(log_cost_real ~ ., data = train_df[,2:96])

# Get the performance of the Linear Regression Model on all test set subsamples:
test_lm <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_lm, test_df[test_samples[[i]],3:96])
  test_lm <- rbind(test_lm, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_lm <- cbind(test_lm, c('LR'), c('EXCLUDED'))
colnames(test_lm) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run random forest model on all training data:
set.seed(123)
model_rf <- randomForest(train_df[,3:96], y = train_df[,2], ntree = 300, mtry = 20, nodesize = 20)

# Get the performance of the Random Forest Model on all test set subsamples:
test_rf <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_rf, test_df[test_samples[[i]],3:96])
  test_rf <- rbind(test_rf, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat), 
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_rf <- cbind(test_rf, c('RF'), c('EXCLUDED'))
colnames(test_rf) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run neural network model on all training data:
set.seed(123)
model_nn <- keras_model_sequential() %>% 
  layer_dense(units = 96, activation = 'relu', input_shape = c(94)) %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 55, activation = 'relu') %>% 
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 1, activation = 'linear')
model_nn %>% compile(loss = 'mse', optimizer = 'adam', metrics = list('mse'))
model_nn %>% fit(as.matrix(train_df[,3:96]),
                 as.matrix(train_df[,2]),
                 batch_size = 128,
                 epochs = 20,
                 view_metrics = FALSE)

# Get the performance of the Neural Network Model on all test set subsamples:
test_nn <- c()
for (i in 1:draws) {
  y_hat <- model_nn %>% predict(as.matrix(test_df[test_samples[[i]],3:96]))
  meas <- c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
            mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat))
  test_nn <- rbind(test_nn,meas)
  rm(meas)
}
test_nn <- cbind(test_nn, c('NN'), c('EXCLUDED'))
colnames(test_nn) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_nn) <- c(1:100)

# Combine LR, RF, and NN test set results:
test_excluded <- rbind(test_lm, test_rf, test_nn)
rm(test_lm, test_rf, test_nn, y_hat)


# WHEN DESTROYED AIRCRAFT ARE INCLUDED:

# Call prepare_repair_data() and gen_train_test() to get train and test data:
prepare_other_data(df = strike, drop_destroyed = FALSE)
gen_train_test(df = model_df, split = 0.8)

# Perform 100 draws on test data:
set.seed(123)
draws = 100
test_samples <- createDataPartition(test_df$log_cost_real, times = draws, p = 0.65)

# Run linear regression model on all training data:
model_lm <- lm(log_cost_real ~ ., data = train_df[,2:97])

# Get the performance of the Linear Regression Model on all test set subsamples:
test_lm <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_lm, test_df[test_samples[[i]],3:97])
  test_lm <- rbind(test_lm, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_lm <- cbind(test_lm, c('LR'), c('INCLUDED'))
colnames(test_lm) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run random forest model on all training data:
set.seed(123)
model_rf <- randomForest(train_df[,3:97], y = train_df[,2], ntree = 300, mtry = 20, nodesize = 20)

# Get the performance of the Random Forest Model on all test set subsamples:
test_rf <- c(NULL, NULL, NULL, NULL)
for (i in 1:draws) {
  y_hat <- predict(model_rf, test_df[test_samples[[i]],3:97])
  test_rf <- rbind(test_rf, c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat), 
                              mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat)))
}
test_rf <- cbind(test_rf, c('RF'), c('INCLUDED'))
colnames(test_rf) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_lm) <- c(1:100)

# Run neural network model on all training data:
set.seed(123)
model_nn <- keras_model_sequential() %>% 
  layer_dense(units = 96, activation = 'relu', input_shape = c(95)) %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 55, activation = 'relu') %>% 
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 1, activation = 'linear')
model_nn %>% compile(loss = 'mse', optimizer = 'adam', metrics = list('mse'))
model_nn %>% fit(as.matrix(train_df[,3:97]),
                 as.matrix(train_df[,2]),
                 batch_size = 128,
                 epochs = 20,
                 view_metrics = FALSE)

# Get the performance of the Neural Network Model on all test set subsamples:
test_nn <- c()
for (i in 1:draws) {
  y_hat <- model_nn %>% predict(as.matrix(test_df[test_samples[[i]],3:97]))
  meas <- c(mse(test_df[test_samples[[i]],2],y_hat), rmse(test_df[test_samples[[i]],2],y_hat),
            mae(test_df[test_samples[[i]],2],y_hat), rsq(test_df[test_samples[[i]],2],y_hat))
  test_nn <- rbind(test_nn,meas)
  rm(meas)
}
test_nn <- cbind(test_nn, c('NN'), c('INCLUDED'))
colnames(test_nn) <- c('MSE', 'RMSE', 'MAE', 'R-SQUARE', 'MODEL', 'DESTROYED')
rownames(test_nn) <- c(1:100)

# Combine LR, RF, and NN test set results:
test_included <- rbind(test_lm, test_rf, test_nn)
rm(test_lm, test_rf, test_nn, y_hat, test_df, train_df, missing_df, model_df, model_lm, model_rf, model_nn,
   test_samples)


# COMBINE ALL TEST SET ACCURACY:
test_accuracy_other <- data.frame(rbind(test_excluded, test_included))
test_accuracy_other$MSE <- as.numeric(test_accuracy_other$MSE)
test_accuracy_other$RMSE <- as.numeric(test_accuracy_other$RMSE)
test_accuracy_other$MAE <- as.numeric(test_accuracy_other$MAE)
test_accuracy_other$R.SQUARE <- as.numeric(test_accuracy_other$R.SQUARE)
rownames(test_accuracy_other) <- c()
rm(test_excluded, test_included)


# PLOT TEST SET ACCURACY:

get_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

scaleFUN <- function(x) sprintf("%.3f", x)

p1 <- ggplot(subset(test_accuracy_other, DESTROYED=='INCLUDED'),
             aes(MSE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1,) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Square Error') + 
  theme(legend.position = "none") + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p2 <- ggplot(subset(test_accuracy_other, DESTROYED=='EXCLUDED'),
             aes(MSE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Square Error') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p3 <- ggplot(subset(test_accuracy_other, DESTROYED=='INCLUDED'),
             aes(MAE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Absolute Error') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p4 <- ggplot(subset(test_accuracy_other, DESTROYED=='EXCLUDED'),
             aes(MAE, fill = MODEL, colour = MODEL)) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nMean Absolute Error') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif'))  + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p5 <- ggplot(subset(test_accuracy_other, DESTROYED=='INCLUDED'),
             aes(R.SQUARE, fill = MODEL, colour = MODEL)) + scale_x_continuous(labels=scaleFUN) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nR-Squared') + 
  theme(legend.position = "none")  + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif')) + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
p6 <- ggplot(subset(test_accuracy_other, DESTROYED=='EXCLUDED'),
             aes(R.SQUARE, fill = MODEL, colour = MODEL)) + scale_x_continuous(labels=scaleFUN) +
  geom_density(alpha = 0.1) + theme_bw() + theme(axis.title.y = element_blank()) + xlab('\nR-Squared') + 
  theme(legend.position = 'bottom', legend.title = element_blank()) + theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif')) + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  scale_colour_viridis(discrete = TRUE, end = 0.85) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), labels=scaleFUN) +
  scale_y_continuous(expand = expand_scale(mult=c(0.01,0.05)))
legend <- get_legend(p6)
p6 <- p6 + theme(legend.position = "none")

grid.arrange(arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=2, nrow = 3), legend, ncol=1, nrow = 2, heights=c(10, 1))

# Remove plot objects:
rm(p1,p2,p3,p4,p5,p6,legend)

########################################################################################################################


########################################################################################################################
# TABLE 3: Test set accuracy

panel_a <- summaryBy(MSE + MAE + R.SQUARE ~ MODEL + DESTROYED, FUN=c(mean,sd), data=test_accuracy_repair)
panel_a <- panel_a[,c(1,2,3,6,4,7,5,8)]
panel_a <- panel_a[order(panel_a$DESTROYED, panel_a$MODEL),]
panel_a <- panel_a[c(4,5,6,1,2,3),]
panel_b <- summaryBy(MSE + MAE + R.SQUARE ~ MODEL + DESTROYED, FUN=c(mean,sd), data=test_accuracy_other)
panel_b <- panel_b[,c(1,2,3,6,4,7,5,8)]
panel_b <- panel_b[order(panel_b$DESTROYED, panel_b$MODEL),]
panel_b <- panel_b[c(4,5,6,1,2,3),]

# Panel A: Repair Costs
panel_a
# Panel B: Other Costs
panel_b

# Remove table data frames:
rm(panel_a, panel_b)

########################################################################################################################


########################################################################################################################
# FIGURE 7: Lower bound total cost estimates, 1990-2018

# Run final Random Forest models and collect cost predictions for the missing data: 

# Repair costs w/ destroyed aircraft included:
  prepare_repair_data(df = strike, drop_destroyed = FALSE)
  missing_df$strike_effect_Aborted.Take.off..Other <- 0
  # Model:
  generate_accuracy()
  model_rf <- randomForest(model_df[,3:96], y = model_df[,2], ntree = 300, mtry = 20, nodesize = 20, importance = TRUE)
  y_hat <- predict(model_rf, model_df[,3:96])
  append_accuracy(model_df[,2],y_hat)
  report_accuracy()
  # Ten most important variables in final repair cost model:
  import <- tail(model_rf$importance[order(model_rf$importance[,1]),], 20)
  import[order(row.names(import)),]
  rm(import)
  # Model predictions:
  model_df$repair_cost_prediction_inc <- predict(model_rf, model_df[,3:96])
  missing_df$repair_cost_prediction_inc <- predict(model_rf, missing_df[,3:96])
  prediction_repair <- rbind(model_df, missing_df)
  prediction_repair <- prediction_repair[,c(1,97)]

# Repair costs w/ destroyed aircraft excluded:
  prepare_repair_data(df = strike, drop_destroyed = TRUE)
  missing_df$strike_effect_Aborted.Take.off..Other <- 0
  # Model:
  model_rf <- randomForest(model_df[,3:95], y = model_df[,2], ntree = 300, mtry = 20, nodesize = 20)
  # Model predictions:
  model_df$repair_cost_prediction_exc <- predict(model_rf, model_df[,3:95])
  missing_df$repair_cost_prediction_exc <- predict(model_rf, missing_df[,3:95])
  prediction_repair_exc  <- rbind(model_df, missing_df)
  prediction_repair_exc <- prediction_repair_exc[,c(1,96)]
  prediction_repair <- merge(prediction_repair, prediction_repair_exc, by="index_nr", all = TRUE)
  rm(prediction_repair_exc)

# Other costs w/ destroyed aircraft included:
  prepare_other_data(df = strike, drop_destroyed = FALSE)
  missing_df$strike_effect_Aborted.Take.off..Other <- NULL
  # Model:
  generate_accuracy()
  model_rf <- randomForest(model_df[,3:97], y = model_df[,2], ntree = 300, mtry = 20, nodesize = 20, importance = TRUE)
  y_hat <- predict(model_rf, model_df[,3:97])
  append_accuracy(model_df[,2],y_hat)
  report_accuracy()
  # Ten most important variables in final repair cost model:
  import <- tail(model_rf$importance[order(model_rf$importance[,1]),], 20)
  import[order(row.names(import)),]
  rm(import)
  # Model predictions:
  model_df$other_cost_prediction_inc <- predict(model_rf, model_df[,3:97])
  missing_df$other_cost_prediction_inc <- predict(model_rf, missing_df[,3:97])
  prediction_other <- rbind(model_df, missing_df)
  prediction_other <- prediction_other[,c(1,98)]

# Repair costs w/ destroyed aircraft excluded:
  prepare_other_data(df = strike, drop_destroyed = TRUE)
  missing_df$strike_effect_Aborted.Take.off..Other <- NULL
  # Model:
  model_rf <- randomForest(model_df[,3:96], y = model_df[,2], ntree = 300, mtry = 20, nodesize = 20)
  # Model predictions:
  model_df$other_cost_prediction_exc <- predict(model_rf, model_df[,3:96])
  missing_df$other_cost_prediction_exc <- predict(model_rf, missing_df[,3:96])
  prediction_other_exc  <- rbind(model_df, missing_df)
  prediction_other_exc <- prediction_other_exc[,c(1,97)]
  prediction_other <- merge(prediction_other, prediction_other_exc, by="index_nr", all = TRUE)
  rm(prediction_other_exc)

# Merge repair and other cost predictions:
predictions <- merge(prediction_repair, prediction_other, by="index_nr", all = TRUE)

# Remove unneeded data frames and model objects:
rm(missing_df, model_df, model_rf, prediction_other, prediction_repair)

# Generate data frame with actual reported costs:
reported <- strike[,c('index_nr', 'incident_year', 'real_cost_repair','real_cost_other')]

# Merge reported and predicted costs (and exponentiate predicted log costs):
costs <- merge(predictions, reported, by='index_nr', all = TRUE)
costs$repair_cost_prediction_inc <- exp(costs$repair_cost_prediction_inc)
costs$repair_cost_prediction_exc <- exp(costs$repair_cost_prediction_exc)
costs$other_cost_prediction_inc <- exp(costs$other_cost_prediction_inc)
costs$other_cost_prediction_exc <- exp(costs$other_cost_prediction_exc)

# Replace predictions with reported costs for observations with reported costs:
costs$final_repair_prediction_inc <- ifelse(!is.na(costs$real_cost_repair),
                                            costs$real_cost_repair,
                                            costs$repair_cost_prediction_inc)
costs$final_repair_prediction_exc <- ifelse(!is.na(costs$real_cost_repair),
                                            costs$real_cost_repair,
                                            costs$repair_cost_prediction_exc)
costs$final_other_prediction_inc <- ifelse(!is.na(costs$real_cost_other),
                                            costs$real_cost_other,
                                            costs$other_cost_prediction_inc)
costs$final_other_prediction_exc <- ifelse(!is.na(costs$real_cost_other),
                                            costs$real_cost_other,
                                            costs$other_cost_prediction_exc)

# Generate final total cost variables:
costs$total_cost_reported <- rowSums(costs[,c('real_cost_repair', 'real_cost_other')], na.rm = TRUE)
costs$total_cost_prediction_inc <- rowSums(costs[,c('final_repair_prediction_inc', 'final_other_prediction_inc')], na.rm = TRUE)
costs$total_cost_prediction_exc <- rowSums(costs[,c('final_repair_prediction_exc', 'final_other_prediction_exc')], na.rm = TRUE)

# Plot data frame:
plot_df <- summaryBy(final_repair_prediction_inc + final_other_prediction_inc ~ incident_year, 
                     FUN=c(sum), 
                     data=costs,
                     na.rm = TRUE)
plot_df <- subset(plot_df, incident_year>1989 & incident_year<2019)
plot_df <- melt(plot_df, id.vars = 'incident_year', measure.vars = c('final_repair_prediction_inc.sum', 
                                                                     'final_other_prediction_inc.sum'))
plot_df$value <- (plot_df$value/1000000)
plot_df$variable <- ifelse(plot_df$variable=='final_repair_prediction_inc.sum', 'Repair costs', 'Other costs')
plot_df$variable <- factor(plot_df$variable, levels = c("Repair costs",'Other costs'))

# Plot:
ggplot(plot_df, 
            aes(incident_year, value, color=variable, fill=variable)) + geom_area(alpha = 0.1)  + theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), legend.direction = 'vertical') +
  theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif')) +
  scale_x_continuous(expand = expand_scale(mult=c(0.0015,0.0015)), name="\n\nYear", limits=c(1990, 2018), breaks = c(1990,1995,2000,2005,2010,2015)) +
  scale_y_continuous(expand = expand_scale(mult=c(0.002,0.002)), name="Total costs (millions of 2018 $)\n", limits=c(0, 120), breaks = c(15,30,45,60,75,90,105,120))+
  scale_fill_manual(values = c("#440154FF", "#287d8eff")) +
  scale_color_manual(values = c("#440154FF", "#287d8eff"))

########################################################################################################################


########################################################################################################################
# FIGURE 8: Comparing lower bound cost estimates

# Plot data frame:
plot_df <- summaryBy(total_cost_prediction_inc +
                       total_cost_prediction_exc + 
                       real_cost_repair +
                       real_cost_other ~
                       incident_year, FUN=c(sum), data=costs, na.rm=TRUE)
plot_df <- subset(plot_df, incident_year>1989 & incident_year<2019)
plot_df$actual <- (plot_df$real_cost_repair.sum + plot_df$real_cost_other.sum)
plot_df$total_cost_prediction_inc.sum <- plot_df$total_cost_prediction_inc.sum/1000000
plot_df$total_cost_prediction_exc.sum <- plot_df$total_cost_prediction_exc.sum/1000000
plot_df$actual <- plot_df$actual/1000000

# Add the Dolbeer et al. (2019) estimates:
plot_df$dolbeer_repair <- c(94,36,53,46,46,339,59,61,166,111,112,284,171,165,102,267,206,173,109,
                            455,150,276,150,95,201,221,91,224,79)
plot_df$dolbeer_other <- c(27,20,3,5,55,148,18,32,24,21,130,39,73,44,22,77,13,33,13,18,16,18,12,19,16,29,20,20,13)
plot_df$dolbeer_total <- c(122,56,56,51,100,487,77,93,189,132,242,324,244,209,124,344,219,206,122,
                           472,166,294,162,113,217,250,111,245,92)


# Plot:
ggplot(plot_df, aes(x = incident_year)) +
  geom_line(aes(y = total_cost_prediction_inc.sum, colour = "Machine learning estimates (Destroyed aircraft included in model training)")) + 
  geom_line(aes(y = total_cost_prediction_exc.sum, colour = "Machine learning estimates (Destroyed aircraft excluded in model training)")) + 
  geom_line(aes(y = dolbeer_total, colour = 'Dolbeer et. al. (2019) estimates')) + 
  geom_line(aes(y = actual, colour = 'Actual reported total costs')) + 
  labs(y = "Total costs (millions of 2018 $)\n", x = "\n\nYear") + 
  theme_bw() + 
  theme(legend.position = 'bottom', legend.title = element_blank(), legend.direction = 'vertical') + 
  theme(axis.title.x = element_text(size=9)) +
  theme(text = element_text(family = 'CMU Serif')) +
  scale_x_continuous(expand = expand_scale(mult=c(0.001,0.001)), breaks = c(1990,1995,2000,2005,2010,2015)) +
  scale_colour_viridis(discrete = TRUE)

########################################################################################################################


########################################################################################################################
# Appendix Table A1: A comparison of lower bound cost estimates (in millions of 2018 $)

round(plot_df[,c("incident_year", 
                 "actual", 
                 "dolbeer_total", 
                 "total_cost_prediction_inc.sum", 
                 "total_cost_prediction_exc.sum")], digits = 0)

########################################################################################################################


########################################################################################################################
########################################################################################################################
#####################################                   END                   ##########################################
########################################################################################################################
########################################################################################################################