#!/usr/bin/bash
#SBATCH --job-name=pmt
#SBATCH --time=22:00:00
#SBATCH --mem=32G
#SBATCH --output=/home/users/minwoos/projects/covid-mortality/output.log
#SBATCH -c 4
#SBATCH --mail-type=END,FAIL       # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=minwoos@stanford.edu

pwd; hostname; date

# load the module
module load R/4.0.2

# run R code
R --no-save << EOF
####################### R code #######################
start.time <- Sys.time()
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(gbm)

outdir = "/scratch/users/minwoos/covid-mortality/"

source("/scratch/users/minwoos/repos/covid_mortality/analysis/World/code/helper_functions.R")

set.seed(100)

# load data
df = read.csv("/scratch/users/minwoos/repos/covid_mortality/analysis/World/data/preprocessed/XY_WHO_trust.csv")
iso = df\$iso

# exclude features
index.exclude = which(names(df) %in% c("iso",
                                       "Days_Until_All_Vulnerable_Vacc_Elig",
                                       "Ages_15_To_64_Percent",
                                       "V1",
                                       "Sub.region.Name"
))

# convert region variables into factor
df\$Region.Name <- as.factor(df\$Region.Name)
df\$Sub.region.Name <- as.factor(df\$Sub.region.Name)



#filter for 10M instead of 1M population
N = 5000000
iso=iso[df\$Population>N]
Y = df[df\$Population>N, "excess_death"]
df = df[df\$Population>N,-index.exclude]

# Convert Y into crude rate from count 
X = df %>% select(-excess_death)
Y = (Y / X\$Population) * 100000





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
                                        "Health_Expenditure_Per_Capita",
                                        "Age_65_Older_Percent",
                                 #       "Ages_0_To_14_Percent",
                                        "Trust_In_Neighborhood",
                                        "Trust_In_Govt",
                                        # "Percent_Using_Internet",
                                        # "Percent_Ppl_Poor_Air_Quality",
                                        # "Percent_Health_Expenditure_Private",
                                        # "Avg_Gov_Stringency_Index",
                                        # "Vaccines_Safe_50Plus",
                                        "Confidence_In_Hospitals"
                                        # "Trust_In_Journalists"
))

index.intrinsic = c(index.intrinsic, index.region)

index.modifiable =which(names(X) %in% c("Percent_One_Dose_As_Of_Nov_1",
                     "Total_Days_Over_1_Test_Per_Thousand",
                     "Trust_Covid_Advice_Govt"))

index.modifiable = c(index.modifiable, index.policy.grouped)

index.I = index.intrinsic
index.M = index.modifiable
index.IM = c(index.intrinsic, index.modifiable)


#######################################
# Fit initial GBM // best hyperparams #
#######################################

best_params_intrinsic =  data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 10)

best_params_modifiable = data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 10)


















#--- cv code
k = 10
k_ = nrow(X) / k
X_index = 1:nrow(X)
shuffled_index = sample(X_index)

X_IM = cbind(X[,index.IM], Y=Y, iso=iso)
X_IM = X_IM[order(shuffled_index),]
Y_ = X_IM\$Y
iso_ = X_IM\$iso
X_IM = X_IM %>% select(-iso)

X_I = cbind(X[,index.I], Y=Y, iso=iso)
X_I = X_I[order(shuffled_index),]
Y_ = X_I\$Y
iso_ = X_I\$iso
X_I = X_I %>% select(-iso)

iso.shuffled = data.frame(cbind(iso, shuffled_index ))
split_index = split(shuffled_index , ceiling(seq_along(shuffled_index)/k_))

cverror = cbind(iso.shuffled, 
                cverror_IM = rep(NA, nrow(iso.shuffled)),
                cverror_I = rep(NA, nrow(iso.shuffled)))


for (i in 1:k){
  
  index.test = split_index[[i]]
  index.train = split_index[-i] %>% unlist %>% as.vector
  
  # INTRINSIC + MODIFIABLE CV Error ---------------------
  gbm.params = best_params_modifiable
  
  # enter model here
  control <- trainControl(method='none')
  
  #perform cv process
  boost <- train(Y~., data=X_IM[index.train,], 
                 method='gbm',
                 tuneGrid=gbm.params, 
                 trControl=control, 
                 verbose=F)
  
  # predictions from gbm
  Y.hat = predict(boost, newdata = X_IM[index.test,])
  
  cverror[order(as.integer(cverror\$shuffled_index)),][index.test,]\$cverror_IM = Y.hat^3
  
  # INTRINSIC CV Error ----------------------------
  
  gbm.params = best_params_intrinsic
  
  #perform cv process
  boost <- train(Y~., data=X_I[index.train,], 
                 method='gbm',
                 tuneGrid=gbm.params, 
                 trControl=control, 
                 verbose=F)
  
  # predictions from gbm
  Y.hat = predict(boost, newdata = X_I[index.test,])
  
  cverror[order(as.integer(cverror\$shuffled_index)),][index.test,]\$cverror_I = Y.hat^3
}

rMSE_modif = sqrt(mean((Y_^3 - cverror[order(as.integer(cverror\$shuffled_index)),]\$cverror_IM)^2))
rMSE_intrin = sqrt(mean((Y_^3 - cverror[order(as.integer(cverror\$shuffled_index)),]\$cverror_I)^2))
rMSE_delta_observed = rMSE_intrin - rMSE_modif
paste0("OBSERVED rmse diff: ", rMSE_delta_observed) %>% print()
#---


################################
# PERMUTATION Residual difference #
################################


N.permute = 10000
X_permute_list = list()
N.country = dim(X)[1]


### same shared shuffled index for all modifiable variables
# for (i in 1:N.permute){
#   X_permute_list[[i]] = sample(x=1:N.country, size=N.country, replace = FALSE)
# }

## difference shuffled index for each modifiable variables
for (i in 1:N.permute){
  mat.shuffled = matrix(NA, nrow=N.country, ncol=length(index.M))
  for (j in 1:length(index.M)){
    index.shuffled = sample(x=1:N.country, size=N.country, replace = FALSE)
    mat.shuffled[,j] = index.shuffled
  }
  X_permute_list[[i]] = mat.shuffled
}


null.dist = c()



# ---------------------
for (p in 1:N.permute){
  
  k_ = nrow(X) / k
  X_index = 1:nrow(X)
  shuffled_index = sample(X_index)
  
  ####------------------------- intrisinc + modifiable model --------------------------####
  
  ## same shared shuffled index for all modifiable variables
  # print(paste0("permutation: ",p))
  # X_ = X
  # X_[,index.M] = X_[order(X_permute_list[[p]]), index.M] 
  # X_ = cbind(X_[, index.IM], Y=Y, iso=iso)
  # iso_ = X_\$iso
  # X_ = X_ %>% select(-iso) # contains Y
  
  ## difference shuffled index for each modifiable variables
  print(paste0("permutation: ",p))
  X_ = X
  for (j in 1:length(index.M)){
    X_[,index.M[j]] = X_[order(X_permute_list[[p]][,j]), index.M[j]] 
  }
  # X_ = cbind(X_[, index.IM], Y=Y, iso=iso)
  # iso_ = X_\$iso
  # X_ = X_ %>% select(-iso) # contains Y
  
  X_IM = cbind(X_[,index.IM], Y=Y, iso=iso)
  X_IM = X_IM[order(shuffled_index),]
  Y_ = X_IM\$Y
  iso_ = X_IM\$iso
  X_IM = X_IM %>% select(-iso)
  
  X_I = cbind(X_[,index.I], Y=Y, iso=iso)
  X_I = X_I[order(shuffled_index),]
  Y_ = X_I\$Y
  iso_ = X_I\$iso
  X_I = X_I %>% select(-iso)
  
  iso.shuffled = data.frame(cbind(iso, shuffled_index ))
  split_index = split(shuffled_index , ceiling(seq_along(shuffled_index)/k_))
  
  cverror = cbind(iso.shuffled, 
                  cverror_IM = rep(NA, nrow(iso.shuffled)),
                  cverror_I = rep(NA, nrow(iso.shuffled)))
  
  
  

  
  
  for (i in 1:k){
    
    index.test = split_index[[i]]
    index.train = split_index[-i] %>% unlist %>% as.vector
    
    # INTRINSIC + MODIFIABLE CV Error ---------------------
    gbm.params = best_params_modifiable
    
    # enter model here
    control <- trainControl(method='none')
    
    #perform cv process
    boost <- train(Y~., data=X_IM[index.train,], 
                   method='gbm',
                   tuneGrid=gbm.params, 
                   trControl=control, 
                   verbose=F)
    
    # predictions from gbm
    Y.hat = predict(boost, newdata = X_IM[index.test,])
    
    cverror[order(as.integer(cverror\$shuffled_index)),][index.test,]\$cverror_IM = Y.hat^3
    
    # INTRINSIC CV Error ----------------------------
    
    gbm.params = best_params_intrinsic
    
    #perform cv process
    boost <- train(Y~., data=X_I[index.train,], 
                   method='gbm',
                   tuneGrid=gbm.params, 
                   trControl=control, 
                   verbose=F)
    
    # predictions from gbm
    Y.hat = predict(boost, newdata = X_I[index.test,])
    
    cverror[order(as.integer(cverror\$shuffled_index)),][index.test,]\$cverror_I = Y.hat^3
  }
  
  rMSE_modif = sqrt(mean((Y_^3 - cverror[order(as.integer(cverror\$shuffled_index)),]\$cverror_IM)^2))
  rMSE_intrin = sqrt(mean((Y_^3 - cverror[order(as.integer(cverror\$shuffled_index)),]\$cverror_I)^2))


  
  null.dist = c(null.dist, rMSE_intrin - rMSE_modif )
  print(rMSE_intrin - rMSE_modif)
}


pval = sum(null.dist > rMSE_delta_observed) / N.permute
print(pval)

hist(null.dist, 
     breaks=20, 
     xlim=range(min(null.dist)-10, 
                max(null.dist)+10),
     main="Null distribution of rMSE difference")
abline(v=rMSE_delta_observed, col="red")


result = list()
result[[1]] = pval
result[[2]] = rMSE_delta_observed
result[[3]] = null.dist



save(result, file=paste0(outdir,"permutation_test",".RData"))
####################################
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste0("total time: ",time.taken))
###################################my#
####################### R code #######################
EOF
