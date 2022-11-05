here::i_am("analysis/code/hypothesis_test.R")

start.time <- Sys.time()
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(gbm)

outdir = here::here("analysis/data/preprocessed")
source(here::here("analysis/code/helper_functions.R"))

set.seed(100)

# load data
df = read.csv(here::here("analysis/data/preprocessed","XY_WHO_trust.csv"))
iso = df$iso

# exclude features
index.exclude = which(names(df) %in% c("iso",
                                       "Days_Until_All_Vulnerable_Vacc_Elig",
                                       "Ages_15_To_64_Percent",
                                       "V1",
                                       "Sub.region.Name"
))

# convert region variables into factor
df$Region.Name <- as.factor(df$Region.Name)
df$Sub.region.Name <- as.factor(df$Sub.region.Name)



#filter for 10M instead of 1M population
N = 5000000
iso=iso[df$Population>N]
Y = df[df$Population>N, "excess_death"]
df = df[df$Population>N,-index.exclude]

# Convert Y into crude rate from count 
X = df %>% select(-excess_death)
Y = (Y / X$Population) * 100000


###########################################
# Log transform and scale skewed features #
###########################################
# log transform
# later create function loop to output all histograms
X[,'Population'] = log(X[,'Population']+1)
X[,"People_Per_Sq_Km_of_Land"] = log(X[,"People_Per_Sq_Km_of_Land"]+.01)
X[,"People_Per_Sq_Km_of_Land"] = log(X[,"People_Per_Sq_Km_of_Land"]+.01)
X[,"GDP_Per_Capita"] = log(X[,"GDP_Per_Capita"]+.01)
X[,"Health_Expenditure_Per_Capita"] = log(X[,"Health_Expenditure_Per_Capita"]+.01)


###########################################
# Transform and Z-score target variable   #
###########################################

# apply none (0) log (1) or cube root (2) transform then scale target
Y = cube_root(Y)

##############
# continents #
##############

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=X)

#perform one-hot encoding on data frame
X.dummy <- data.frame(predict(dummy, newdata=X))

# drop columns with 0 variance -> just Oceania
X.dummy <- X.dummy %>% select(-Region.Name.Oceania)

X = X.dummy


##########################################
#             set indices                #
##########################################
# modifiable policy features grouped
index.policy.grouped = which(names(X) %in% c("Govt_Swiftness_Stringency",
                                             "Govt_Persistent_Stringency"))

# index for dummy variable continent
index.region = which(names(X) %in% c("Region.Name.Africa",
                                     "Region.Name.Americas",
                                     "Region.Name.Asia",
                                     "Region.Name.Europe"
))

index.intrinsic = which(names(X) %in% c("Population",
                                        "Obese_Adult_Percentage",
                                        "Hospital_Beds_Per_1000",
                                        "Nurses_And_Midwives_Per_1000",
                                        "People_Per_Sq_Km_of_Land" ,
                                        "GDP_Per_Capita",
                                      #  "Health_Expenditure_Per_Capita",
                                        "Age_65_Older_Percent",
                                        "Trust_In_Neighborhood",
                                        "Trust_In_Govt",
                                        "Confidence_In_Hospitals"
))

index.intrinsic = c(index.intrinsic, index.region)

index.actionable = which(names(X) %in% c("Percent_One_Dose_As_Of_Nov_1",
                                        "Total_Days_Over_1_Test_Per_Thousand",
                                        "Trust_Covid_Advice_Govt"))

index.actionable = c(index.actionable, index.policy.grouped)

index.I = index.intrinsic
index.IA = c(index.intrinsic, index.actionable)


#######################################
#         GBM best hyperparams        #
#######################################

best_params_intrinsic =  data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 10)

best_params_modifiable = data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 7)


######################################################## 
#       Hypothesis testing - observed statistic        #
########################################################

# --------------------- Setup CV folds for intrinsic model --------------------- 
# generate shuffled row index
k = 10
k_ = nrow(X) / k
X_index = 1:nrow(X)
shuffled_index = sample(X_index)

# randomly shuffle the rows for I
df.I = cbind(X[,index.I], Y=Y, iso=iso)
df.I = df.I[order(shuffled_index),]
df.I = df.I %>% select(-iso)

# randomly shuffle the rows for IA
df.IA = cbind(X[,index.IA], Y=Y, iso=iso)
df.IA = df.IA[order(shuffled_index),]
df.IA = df.IA %>% select(-iso)

# split indices into k evenly sized folds
split_index = split(shuffled_index, ceiling(seq_along(shuffled_index)/k_))

# --------------------- INTRINSIC model CV residuals --------------------- 
gbm.params = best_params_intrinsic
control <- trainControl(method='none')
res = c()
Yhat.I = c()

for (i in 1:k){
  index.test = split_index[[i]]
  index.train = split_index[-i] %>% unlist %>% as.vector
  
  boost <- train(Y~., data=df.I[index.train,], 
                 method='gbm',
                 tuneGrid=gbm.params, 
                 trControl=control, 
                 verbose=F)
  
  Yhat = predict(boost, newdata = df.I[index.test,])
  Yhat.I = c(Yhat.I, Yhat)
  res = c(res, df.I$Y[index.test] - Yhat)
}



# add the residuals to intrinsic model yhat
yhat.star = Yhat.I + res
# ----------------  Fit full model on yhat.star -------------------

# replace original Y with yhat.star
df.IA$Y = yhat.star   

# -------- Setup CV folds for intrinsic + actionable model ----------
# generate shuffled row index
k = 10
k_ = nrow(X) / k
X_index = 1:nrow(X)
shuffled_index = sample(X_index)

# randomly shuffle the rows for IA
df.IA = df.IA[order(shuffled_index),]

# split indices into k evenly sized folds
split_index = split(shuffled_index , ceiling(seq_along(shuffled_index)/k_))

# fit full model
gbm.params = best_params_modifiable
control <- trainControl(method='none')
res.IA = c()
Yhat.IA = c()

for (i in 1:k){
  index.test = split_index[[i]]
  index.train = split_index[-i] %>% unlist %>% as.vector
  
  boost <- train(Y~., data=df.IA[index.train,], 
                 method='gbm',
                 tuneGrid=gbm.params, 
                 trControl=control, 
                 verbose=F)
  
  Yhat = predict(boost, newdata = df.IA[index.test,])
  Yhat.IA = c(Yhat.IA, Yhat)
  res.IA = c(res.IA, df.IA$Y[index.test] - Yhat)   # obs - pred
}

MSE.star = mean(res^2)
MSE.full = mean(res.IA^2)

delta.MSE.oberved = MSE.star - MSE.full








######################################################## 
#       Hypothesis testing - null distribution          #
########################################################

# --------------------- Setup CV folds for intrinsic model --------------------- 
# generate shuffled row index
k = 10
k_ = nrow(X) / k
X_index = 1:nrow(X)
shuffled_index = sample(X_index)

# randomly shuffle the rows for I
df.I = cbind(X[,index.I], Y=Y, iso=iso)
df.I = df.I[order(shuffled_index),]
df.I = df.I %>% select(-iso)

# randomly shuffle the rows for IA
df.IA = cbind(X[,index.IA], Y=Y, iso=iso)
df.IA = df.IA[order(shuffled_index),]
df.IA = df.IA %>% select(-iso)

# split indices into k evenly sized folds
split_index = split(shuffled_index , ceiling(seq_along(shuffled_index)/k_))

# --------------------- INTRINSIC model CV residuals --------------------- 
gbm.params = best_params_intrinsic
control <- trainControl(method='none')
res = c()
Yhat.I = c()

for (i in 1:k){
  index.test = split_index[[i]]
  index.train = split_index[-i] %>% unlist %>% as.vector
  
  boost <- train(Y~., data=df.I[index.train,], 
                 method='gbm',
                 tuneGrid=gbm.params, 
                 trControl=control, 
                 verbose=F)
  
  Yhat = predict(boost, newdata = df.I[index.test,])
  Yhat.I = c(Yhat.I, Yhat)
  res = c(res, df.I$Y[index.test] - Yhat)
}


B=100
yhat.star = list()
res.star = list()
delta.MSE = c()


# 
for (b in 1:B){
  
  print(b)
  
  # bootstrap the residuals
  res.star[[b]] = sample(res, replace = TRUE)
  
  # add the residuals to intrinsic model yhat
  yhat.star[[b]] = Yhat.I + res.star[[b]]
  # ----------------  Fit full model on yhat.star -------------------
  
  # replace original Y with yhat.star
  df.IA$Y = yhat.star[[b]]   
  
  # -------- Setup CV folds for intrinsic + actionable model ----------
  # generate shuffled row index
  k = 10
  k_ = nrow(X) / k
  X_index = 1:nrow(X)
  shuffled_index = sample(X_index)
  
  # randomly shuffle the rows for IA
  df.IA = df.IA[order(shuffled_index),]
  
  # split indices into k evenly sized folds
  split_index = split(shuffled_index , ceiling(seq_along(shuffled_index)/k_))
  
  # fit full model
  gbm.params = best_params_intrinsic
  control <- trainControl(method='none')
  res.IA = c()
  Yhat.IA = c()
  
  for (i in 1:k){
    index.test = split_index[[i]]
    index.train = split_index[-i] %>% unlist %>% as.vector
    
    boost <- train(Y~., data=df.IA[index.train,], 
                   method='gbm',
                   tuneGrid=gbm.params, 
                   trControl=control, 
                   verbose=F)
    
    Yhat = predict(boost, newdata = df.IA[index.test,])
    Yhat.IA = c(Yhat.IA, Yhat)
    res.IA = c(res.IA, df.IA$Y[index.test] - Yhat)
  }
  
  MSE.star = mean(res.star[[b]]^2)
  MSE.full = mean(res.IA^2)
  
  delta.MSE[b] = MSE.star - MSE.full
  
}



pval = sum(delta.MSE.oberved < delta.MSE) / length(delta.MSE)

print(pval)