#FINAL PROJECT, GROUP C, MODULE 6 FINAL REPORT 
#TRANG TRAN
#DANIELLE DANE
#PETER BROWN

cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

################################################################################

#Data Cleaning & EDA

################################################################################

library(pacman)
p_load(tidyverse, RColorBrewer, car, corrplot, skimr)

#1/Load the Ames housing dataset
df <- read.csv("AmesHousing.csv")

#2/Perform EDA and Clean Data
colnames(df)
skim(df) #data types, missing values, min, max, mean, sd, hist

# remove Identifier variables & columns with > 20% missing values
df <- dplyr::select(df,-Order, -PID, -Alley, -Fireplace.Qu, -Pool.QC, -Fence, 
                    -Misc.Feature)

#convert character variables used in analysis into factors
df$Bsmt.Exposure <- factor(df$Bsmt.Exposure)

#remove all the rest of the character variables & quality variables not used
df <- dplyr::select(df,-MS.Zoning, -Street, -Lot.Shape, -Land.Contour, 
                    -Utilities, -Lot.Config, -Land.Slope, -Condition.1, 
                    -Condition.2, -Bldg.Type, -House.Style, -Roof.Style, 
                    -Roof.Matl, -Exterior.1st, -Exterior.2nd, -Mas.Vnr.Type, 
                    -Exter.Qual, -Exter.Cond, -Foundation, -Bsmt.Qual, 
                    -Bsmt.Cond, -BsmtFin.Type.1, -BsmtFin.Type.2, -Heating, 
                    -Heating.QC, -Central.Air, -Electrical, -Kitchen.Qual, 
                    -Functional, -Garage.Type,-Garage.Finish, -Garage.Qual, 
                    -Garage.Cond, -Paved.Drive, -Sale.Type, -Sale.Condition, 
                    -Overall.Cond, -Overall.Qual, -MS.SubClass)

# impute mean in all numeric columns
nums <- colnames(df[, unlist(lapply(df, is.numeric)), drop = FALSE]) 
for (col_name in nums) {
  df[is.na(df[, col_name]), col_name] <- mean(df[, col_name], na.rm = TRUE)
}

################################################################################
#PART 1:Home features: Basement Exposure & Sale Prices
#method: Kruskal-Wallis test
#Trang Tran
################################################################################

## Question: Do the features of a basement significantly impact the price of a property?
# Bsmt Exposure	(Ordinal): Refers to walkout or garden level walls

# Gd	Good Exposure
# Av	Average Exposure (split levels or foyers typically score average or above)	
# Mn	Mimimum Exposure
# No	No Exposure
# NA	No Basement

df %>%  group_by(Bsmt.Exposure) %>%  summarise(count = n())

# replace 79 'NA' values with 'No Bsmt'
df$Bsmt.Exposure <- ifelse(is.na(df$Bsmt.Exposure), "No.Bsmt", df$Bsmt.Exposure)

# drop 4 rows with null values
df <- subset(df, Bsmt.Exposure != "")

# Create a data frame with 'Bsmt Exposure' and 'SalePrice'
df_bsmt <- data.frame(Bsmt.Exposure = df$Bsmt.Exposure,
                      SalePrice = df$SalePrice)

# Test for normality (ANOVA assumption)
writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')
for (Bsmt.Exposure in unique(df_bsmt$Bsmt.Exposure)) {
  print(Bsmt.Exposure)
  print(shapiro.test(df_bsmt[df_bsmt$Bsmt.Exposure == Bsmt.Exposure, 'SalePrice']))
}

writeLines('Form Hypotheses:')
writeLines('\nH0: mean1 = mean2 = mean3 = mean4 = mean5')
writeLines('H1: at least one of the means is different than the others')
writeLines('claim: H1')

# perform Kruskal-Wallis test
alpha <- 0.05
writeLines(paste('\nKruskal-Wallis test alpha:', alpha))
results <- kruskal.test(SalePrice ~ Bsmt.Exposure, data = df_bsmt)
test_statistic <- results$statistic
p_value <- results$p.value

writeLines('\n***************************')
writeLines('results of Kruskal-Wallis test:\n')
print(test_statistic)
print(p_value)

writeLines('\n***************************')
# compare the p-value to alpha and make decision
if (p_value > alpha) {
  decision = 'fail to reject H0'
} else {
  decision = 'reject H0'
}
writeLines(paste('decision: ', decision))

# visualize the results
boxplot(SalePrice ~ Bsmt.Exposure, data = df_bsmt)



################################################################################
#PART 2:Neighborhoods & Sale Prices
#method: ANOVA
#Peter Brown
################################################################################

# load packages

library(VIM)
library(hash)
library(corrplot)
library(RColorBrewer)
library(Metrics)
library(car)
library(hexbin)
library(tidyverse)
library(dplyr)
library(pacman)
p_load(DescTools)
library(DescTools)

################################################################################
# ensure reproducibility
################################################################################
writeLines("\n\n")
writeLines("***************ensure reproducibility - set seed******************")

set.seed(43)

# reduce to neighborhood and sale price
df_nghbhd <- df %>%  select(SalePrice, Neighborhood)

df_nghbhd <-  df_nghbhd[order(df_nghbhd$Neighborhood),]

summary(df_nghbhd)
glimpse(df_nghbhd)
View(df_nghbhd)
################################################################################

################################################################################
# 2. check means
##############################################################################,##

writeLines('\n***************************')
writeLines('Check out the means of the groups')
means_nghbhd <- aggregate(df_nghbhd$SalePrice, 
                          by = list(df_nghbhd$Neighborhood), FUN = mean)

means_nghbhd[order(means_nghbhd$x, decreasing = TRUE),]

colnames(means_nghbhd) <- c('Neighborhood', 'Price')

################################################################################

################################################################################
# 3. Create df of all neighborhoods with means > $300K
################################################################################

nghbhd_300k <- subset(df_nghbhd,
                      Neighborhood %in% c("NoRidge", "NridgHt", "StoneBr"))

nghbhd_300k$Neighborhood[nghbhd_300k$Neighborhood == 'NoRidge'] <- 'Northridge'
nghbhd_300k$Neighborhood[nghbhd_300k$Neighborhood == 'NridgHt'] <- 
  'Northridge_Hts'
nghbhd_300k$Neighborhood[nghbhd_300k$Neighborhood == 'StoneBr'] <- 'Stone_Brook'

################################################################################

################################################################################
# CHECK ASSUMPTIONS FOR ANOVA
################################################################################

################################################################################
# 4. check standard deviations
################################################################################

writeLines('\n***************************')
writeLines('Check out the standard deviations (sqrt(variance)) of the groups')
print(aggregate(nghbhd_300k$SalePrice, 
                by = list(nghbhd_300k$Neighborhood), FUN = sd))

119273.02/95932.35
# [1] 1.243303
# 0.05 < 1.243 <  - assumption met for equal variance

################################################################################

################################################################################
# 5. check for normal distribution -  Shapiro-Wilk normality test
################################################################################

writeLines('\n***************************')
writeLines('Check one-way annova assumptions:')

writeLines('\n***************************')
writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')

for (nh in unique(nghbhd_300k$Neighborhood)) {
  print(nh)
  print(shapiro.test(nghbhd_300k[nghbhd_300k$Neighborhood == nh,'SalePrice']))
}

# none of the neighborhood sale prices are normally distributed
# ANOVA cannot be used. Non-parametric test needed.

################################################################################

################################################################################
# 6. Kruskal-Wallis test - test if three or more independent samples were 
# selected from populations that have the same distributions
################################################################################

# create dataframe
df_300K <-  read.csv('300K Neighborhood Sale Prices.csv')

# add column names
colnames(df_300K) <- c("NoRidge", "NridgHt", "StoneBr")


alpha = 0.05

# hypotheses
# H0: mu1 == mu2 == mu2
# H1: one or more mu different
# claim is H1

# kruskal.test
results <- kruskal.test(list('a' = df_300K$NoRidge, 
                             'b' = df_300K$NridgHt, 
                             'c' = df_300K$StoneBr))

test_statistic <- results$statistic
print(test_statistic)

p_value <- results$p.value
print(p_value)

writeLines('\n***************************')
# compare the p-value to alpha and make decision
if (p_value > alpha) {
  decision = 'fail to reject H0'
} else {
  decision = 'reject H0'
}
writeLines(paste('decision: ', decision))

################################################################################

################################################################################
# 8. visualize the results
################################################################################

boxplot(SalePrice ~ Neighborhood, data = nghbhd_300k)

################################################################################
#part 3: PREDICTING SALE PRICE 
#method: RIDGE AND LASSO REGRESSION
#Danielle Dane
################################################################################


#build a df with only numeric variables in order to run LASSO and Ridge models
df<- dplyr::select(df, SalePrice, Lot.Frontage, Lot.Area, Mas.Vnr.Area, BsmtFin.SF.1, 
            BsmtFin.SF.2, Bsmt.Unf.SF, Total.Bsmt.SF, X1st.Flr.SF, X2nd.Flr.SF,
            Low.Qual.Fin.SF, Gr.Liv.Area, Bsmt.Full.Bath, Bsmt.Half.Bath, 
            Full.Bath, Half.Bath, Bedroom.AbvGr, Kitchen.AbvGr, TotRms.AbvGrd,
            Garage.Cars, Garage.Area, Wood.Deck.SF, Open.Porch.SF, 
            Enclosed.Porch, X3Ssn.Porch, Screen.Porch, Pool.Area)


#-------------------------------------------------------------------------------

#Use the "cor()" function to produce a correlation matrix of the numeric values 

corrs <- round(cor(df[, unlist(lapply(df, is.numeric))]), 2) 

#-------------------------------------------------------------------------------

#Produce a plot of the correlation matrix, and explain how to interpret it 

corrplot(corrs, type = "upper", col = brewer.pal(n = 8, name = "RdYlBu"))

################################################################################

################################################################################
# load packages
################################################################################
library(car)
library(VIM)
library(ISLR)
library(caret)  # don't install through compilation
library(ggplot2)
library(gridExtra)
# library(pROC)
library(glmnet)
library(Metrics)
library(corrplot)
library(RColorBrewer)
# library(matrixStats)
library(stringr)

################################################################################
# check for normality & transform data
################################################################################

shapiro.test(df$SalePrice) #p is less than 0.05 so not normally distributed. uh oh.

#hist of sales price dependent variable
hist_SalePrice<-hist(df$SalePrice, main="Sale Price Frequency")


#Try Log of SalePrice
#log transformation to get sale price more normally distributed
log_SalePrice <- log10(df$SalePrice)

shapiro.test(log_SalePrice) #p is still less than 0.05, but W = 0.986, very close.

#hist of log sales price dependent variable
hist_log_SalePrice<-hist(log_SalePrice, main="Log Sale Price Frequency")


#Try square root transformation
sqrt_SalePrice <- sqrt(df$SalePrice)

shapiro.test(sqrt_SalePrice) #p is still less than 0.05, W = 0.957

#hist of sqrt sales price dependent variable
hist_sqrt_SalePrice<-hist(sqrt_SalePrice, main="Sqrt Sale Price Frequency")


#Try cube root transformation
cube_SalePrice <- df$SalePrice^(1/3)

shapiro.test(cube_SalePrice) #p is still less than 0.05, W = 0.974

#hist of cube sales price dependent variable
hist_sqrt_SalePrice<-hist(sqrt_SalePrice, main="Cube Sale Price Frequency")

#because log has highest W value of all transformations it improved normality from original
#create log_SalePrice column in df and use this as Y variable

df$log_SalePrice=log(df$SalePrice)
df

#remove Sale Price as a variable since we now have log_SalePrice as dependent variable
df<-select(df,-SalePrice)
df


################################################################################
# split data into train and test sets
################################################################################
seed = 123
writeLines('\n****************************************************************')
writeLines('split data into train and test sets:')

train_index <- sample(x = nrow(df), size = nrow(df) * 0.70)
train_df <- df[train_index, ]
test_df <- df[-train_index, ]

writeLines('\n*********************')
writeLines('split out design matrix and target vector from train_df:')
train_x <- model.matrix(log_SalePrice ~ ., train_df)[, -1]
train_y <- train_df$log_SalePrice

writeLines('\n*********************')
writeLines('split out design matrix and target vector from test_df:')
test_x <- model.matrix(log_SalePrice ~ ., test_df)[, -1]
test_y <- test_df$log_SalePrice

################################################################################
# regularization
################################################################################

#do a min and 1se model for both alpha = 0 and alpha = 1
# repeat 4x for
# model: lambda_1se, alpha = 1
# model: lambda_min, alpha = 1
# model: lambda_1se, alpha = 0
# model: lambda_min, alpha = 0

#find best lambda using cross validation

set.seed(123)


#LASSO LAMBDA MIN AND 1SE CALCULATIONS

alpha <- 1 #(lasso)                                         
cv <- cv.glmnet(train_x, train_y, nfolds = 10, alpha = alpha) #use 10 folds
plot(cv)

lambda_min_a1 <- cv$lambda.min  
lambda_1se_a1 <- cv$lambda.1se

print(lambda_min_a1)
print(lambda_1se_a1)

log_lambda_min_a1 <- log(cv$lambda.min) #this one will match plot because it is the log
log_lambda_1se_a1 <- log(cv$lambda.1se) #this one will match plot because it is the log

#---------------------------------------

# MODEL 1: LASSO lambda.1se and alpha = 1
model_1se_a1 <- glmnet(train_x, train_y, alpha = alpha, lambda = cv$lambda.1se)

# print coefficients of model
print(coef(model_1se_a1))


# eval model on train set
preds_train_1se_a1 <- predict(model_1se_a1, newx = train_x)
rmse_train_1se_a1 <- rmse(train_df$log_SalePrice, preds_train_1se_a1)

# eval model on test set
preds_test_1se_a1 <- predict(model_1se_a1, newx = test_x)
rmse_test_1se_a1 <- rmse(test_df$log_SalePrice, preds_test_1se_a1)



# apply model to antilog data

#build new matrix
test_x_antilog <- model.matrix(exp(log_SalePrice) ~ ., test_df)[, -1]
test_y_antilog <- exp(test_df$log_SalePrice)


preds_test_1se_a1_antilog <- predict(model_1se_a1, newx = test_x_antilog)
rmse_test_1se_a1_antilog <- rmse(exp(test_df$log_SalePrice), preds_test_1se_a1)



#-----------------------------

# MODEL 2: LASSO lambda_min, alpha = 1
model_min_a1 <- glmnet(train_x, train_y, alpha = alpha, lambda = cv$lambda.min)

# print coefficients of model
print(coef(model_min_a1))

# eval model on train set
preds_train_min_a1 <- predict(model_min_a1, newx = train_x)
rmse_train_min_a1 <- rmse(train_df$log_SalePrice, preds_train_min_a1)

# eval model on test set
preds_test_min_a1 <- predict(model_min_a1, newx = test_x)
rmse_test_min_a1 <- rmse(test_df$log_SalePrice, preds_test_min_a1)

#Preds vs. actual plots

#train min
ggplot(train_df, aes(x=preds_train_min_a1, y= train_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Train: Predicted vs. Actual Sale Prices using lambda.min LASSO')

#test min
ggplot(test_df, aes(x=preds_test_min_a1, y= test_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Test: Predicted vs. Actual Sale Prices using lambda.min LASSO')

#train 1se
ggplot(train_df, aes(x=preds_train_1se_a1, y= train_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Train: Predicted vs. Actual Sale Prices using lambda.1se LASSO')

#test 1se
ggplot(test_df, aes(x=preds_test_1se_a1, y= test_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Test: Predicted vs. Actual Sale Prices using lambda.1se LASSO')

#----------------------------

#RIDGE LAMBDA MIN AND 1SE CALCULATIONS

#set alpha to 0 (Ridge)
set.seed(123)

alpha <- 0 #(ridge)
cv <- cv.glmnet(train_x, train_y, nfolds = 10, alpha = alpha) #use 10 folds
plot(cv)

lambda_min_a0 <- cv$lambda.min
lambda_1se_a0 <- cv$lambda.1se

print(lambda_min_a0)
print(lambda_1se_a0)

fit<-cv$glmnet.fit
summary(fit)

log_lambda_min_a0 <-log(cv$lambda.min) #this one will match plot because it is the log
log_lambda_1se_a0 <-log(cv$lambda.1se) #this one will match plot because it is the log

#--------------------------------

# MODEL 3: RIDGE lambda.1se and alpha = 0
model_1se_a0 <- glmnet(train_x, train_y, alpha = alpha, lambda = cv$lambda.1se)

# print coefficients of model
print(coef(model_1se_a0))

# eval model on train set
preds_train_1se_a0 <- predict(model_1se_a0, newx = train_x)
rmse_train_1se_a0 <- rmse(train_df$log_SalePrice, preds_train_1se_a0)

# eval model on test set
preds_test_1se_a0 <- predict(model_1se_a0, newx = test_x)
rmse_test_1se_a0 <- rmse(test_df$log_SalePrice, preds_test_1se_a0)

#----------------------------

# MODEL 4: RIDGE lambda_min, alpha = 0
model_min_a0 <- glmnet(train_x, train_y, alpha = alpha, lambda = cv$lambda.min)

# print coefficients of model
print(coef(model_min_a0))

# eval model on train set
preds_train_min_a0 <- predict(model_min_a0, newx = train_x)
rmse_train_min_a0 <- rmse(train_df$log_SalePrice, preds_train_min_a0)    

# eval model on test set
preds_test_min_a0 <- predict(model_min_a0, newx = test_x)
rmse_test_min_a0 <- rmse(test_df$log_SalePrice, preds_test_min_a0)

#Preds vs. actuals plots:

#train min
ggplot(train_df, aes(x=preds_train_min_a0, y= train_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Train: Predicted vs. Actual Sale Prices using lambda.min Ridge')

#test min
ggplot(test_df, aes(x=preds_test_min_a0, y= test_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Test: Predicted vs. Actual Sale Prices using lambda.min Ridge')

#train 1se
ggplot(train_df, aes(x=preds_train_1se_a0, y= train_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Train: Predicted vs. Actual Sale Prices using lambda.1se Ridge')

#test 1se
ggplot(test_df, aes(x=preds_test_1se_a0, y= test_df$log_SalePrice)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Sale Prices', y='Actual Sale Prices', title='Test: Predicted vs. Actual Sale Prices using lambda.1se Ridge')

################################################################################
################################################################################



