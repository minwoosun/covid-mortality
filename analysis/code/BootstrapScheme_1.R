#Authors: David Troxell and Win Woo Sun
#Date: 10/31/22
#Description: Generates a bootstrapped null distribution for the hypothesis test specified in the paper, and reports empirical p-value

library(here)
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(gbm)
library(reshape2)
library(reshape)
library(pdp)
library(plotly)

source(here::here("analysis/code/helper_functions.R"))

set.seed(100)

####Use same data and pre-processing steps as other scripts ####

# load data
df = read.csv(here::here("analysis/data/preprocessed","XY_WHO_trust.csv"))
iso = df$iso

# exclude features due to either multicollinearity identified in other scripts, or because of variables' status as just a name/identifier
index.exclude = which(names(df) %in% c("iso",
                                       "Days_Until_All_Vulnerable_Vacc_Elig",
                                       "Ages_15_To_64_Percent",
                                       "V1",
                                       "Sub.region.Name",
                                       "Vaccines_Safe_50Plus",
                                       "Health_Expenditure_Per_Capita",
                                       "Ages_0_To_14_Percent",
                                       "Trust_In_Journalists",
                                       "Percent_Health_Expenditure_Private",
                                       "Avg_Gov_Stringency_Index",
                                       "Percent_Ppl_Poor_Air_Quality",
                                       "Percent_Using_Internet",
                                       "Total_Days_Masks_At_Least_Recommended"
))

# convert region variables into factor
df$Region.Name <- as.factor(df$Region.Name)
df$Sub.region.Name <- as.factor(df$Sub.region.Name)


#filter for 5M. Discussed in paper
N = 5000000
iso=iso[df$Population>N]
Y = df[df$Population>N, "excess_death"]
df = df[df$Population>N,-index.exclude]

# Convert Y into crude rate from count 
X = df %>% select(-excess_death)
Y = (Y / X$Population) * 100000

# Log transform and scale skewed features #

X[,'Population'] = log(X[,'Population']+1)
X[,"People_Per_Sq_Km_of_Land"] = log(X[,"People_Per_Sq_Km_of_Land"]+.01)
X[,"GDP_Per_Capita"] = log(X[,"GDP_Per_Capita"]+.01)


# Transform and Z-score target variable

# apply none (0) log (1) or cube root (2) transform then scale target
Y = cube_root(Y)

# continents

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=X)

#perform one-hot encoding on data frame
X.dummy <- data.frame(predict(dummy, newdata=X))

# drop columns with 0 variance -> just Oceania
X.dummy <- X.dummy %>% select(-Region.Name.Oceania)

X = X.dummy

#  set indices  #

# static features
index.static = which(names(X) %in% c("Population",
                                     "Obese_Adult_Percentage",
                                     "Hospital_Beds_Per_1000",
                                     "Nurses_And_Midwives_Per_1000",
                                     "People_Per_Sq_Km_of_Land" ,
                                     "GDP_Per_Capita",
                                     "Age_65_Older_Percent",
                                     "Trust_In_Neighborhood",
                                     "Trust_In_Govt",
                                     "Confidence_In_Hospitals"
))

# modifiable features -- exclude grouping, policy 
index.modifiable = which(names(X) %in% c("Percent_One_Dose_As_Of_Nov_1",
                                         "Total_Days_Over_1_Test_Per_Thousand"
))  

#  modifiable policy features without PCA
index.policy.ungrouped = which(names(X) %in% c("Days_Until_All_Vulnerable_Vacc_Elig",
                                               "Days_Until_Masks_Recommended",
                                               "Days_Until_Masks_Required",
                                               "Days_Until_Workplace_Closures_Except_Key",           
                                               "Days_Until_Testing_Key_Groups",  
                                               "Total_Days_Masks_Required_Public",
                                               "Total_Days_Masks_At_Least_Recommended",
                                               "Total_Days_Workplace_Closures_Except_Key",
                                               "Total_Days_Workplace_Closures_Recommended",
                                               "Total_Days_Stay_At_Home_Required_Except_Essentials",
                                               "Total_Days_Comprehensive_Contact_Tracing",
                                               "Total_Days_Open_Public_Testing"
))

# modifiable policy features grouped
index.policy.grouped = which(names(X) %in% c("Govt_Swiftness_Stringency",
                                             "Govt_Persistent_Stringency"))

# index for dummy variable continent
index.region = which(names(X) %in% c("Region.Name.Africa",
                                     "Region.Name.Americas",
                                     "Region.Name.Asia",
                                     "Region.Name.Europe"
))

#modifiable trust
index.trust = which(names(X) %in% c( "Trust_Covid_Advice_Govt"))




#hyperparameters just stay constant throughout boostrapping scheme.
#use the same values as the best hyperparameters found in the repeated cv process
best_params_intrinsic =  data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .01,
                                    n.minobsinnode = 7)

best_params_modifiable = data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 10)


#####BOOTSTRAPPING SCHEME / HYPOTHESIS TEST ####
set.seed(888)
#manually set sampling methodology to help reproducibility issues across versions/machines
RNGkind(sample.kind = "Rounding") 

#specify number of re-samples taken
B<-1000
#specify number of countries used in the test
num_countries<-nrow(X)

#variables used in the "intrinsic" model
fit.indices.I = c(index.static,
                index.region)

#variables used in the "intrinsic + actionable" model
fit.indices.IA =  c(index.static,
                    index.region,
                    index.trust,
                    index.modifiable,
                    index.policy.grouped)


#STEP 1:Find observed residuals to later create the observed test statistic

#fit the original response vector using the intrinsic + actionable model
IA<-cbind(X[,fit.indices.IA],Y)
#create hyperparameter grid of just one option
n.trees=best_params_modifiable$n.trees
interaction.depth=best_params_modifiable$interaction.depth
shrinkage=best_params_modifiable$shrinkage
minobsinnode=best_params_modifiable$n.minobsinnode
tunegrid <- expand.grid(n.trees=n.trees,interaction.depth=interaction.depth,shrinkage=shrinkage,n.minobsinnode=minobsinnode)

#10-fold cv
control <- trainControl(method='cv', number=10, savePredictions ="all")
boost<- train(Y~.,data=IA, method='gbm',tuneGrid=tunegrid, trControl=control, verbose=F)

#find preval fits and get residual vector
allPreds<-boost$pred
allPreds<-allPreds[order(allPreds$rowIndex),]
allPreds<-allPreds[,1:2]
allPreds<-allPreds^3

residuals_observed<-allPreds$obs-allPreds$pred

#STEP 2: Fit data on intrinsic alone

#get dataset of just intrinsic variables
I<-cbind(X[,fit.indices.I],Y)

#10 fold cv to get the preval fit
#create hyperparameter grid of just one option for the caret function
n.trees=best_params_intrinsic$n.trees
interaction.depth=best_params_intrinsic$interaction.depth
shrinkage=best_params_intrinsic$shrinkage
minobsinnode=best_params_intrinsic$n.minobsinnode

#make grid
tunegrid <- expand.grid(n.trees=n.trees,interaction.depth=interaction.depth,shrinkage=shrinkage,n.minobsinnode=minobsinnode)
control <- trainControl(method='cv', number=10, savePredictions ="all")

#train model and get preval fits
boost<- train(Y~.,data=I, method='gbm',tuneGrid=tunegrid, trControl=control, verbose=F)
allPreds<-boost$pred
allPreds<-allPreds[order(allPreds$rowIndex),]
allPreds<-allPreds[,1:2]
allPreds<-allPreds^3

#get original residuals from intrinsic alone
residuals<-allPreds$obs-allPreds$pred 

#create observed test statistic now that we have residuals from intrinsic alone
observed_stat<-sqrt((1/num_countries)*sum(residuals^2))-sqrt((1/num_countries)*sum(residuals_observed^2))

#STEP 3: BOOTSTRAP THE RESIDUALS

#create matrix to store bootstrapped residuals
bootstrappedResid<-matrix(data=0,nrow=num_countries,ncol=B)

#sample with replacement
for(i in 1:B){
newIdx<-sample(1:num_countries,num_countries,replace=TRUE)
bootstrappedResid[,i]<-residuals[newIdx]
}

#create new response variable for next model by adding back in bootstrapped residuals to original predictions
y_b_star = allPreds$pred+bootstrappedResid

#STEP 4: FIT NEW RESPONSE VARIABLE USING INTRINSIC + ACTIONABLE VARIABLES

#get correct dataset
IA<-X[,fit.indices.IA]

#again, create hyperparameter grid of just one option
n.trees=best_params_modifiable$n.trees
interaction.depth=best_params_modifiable$interaction.depth
shrinkage=best_params_modifiable$shrinkage
minobsinnode=best_params_modifiable$n.minobsinnode
tunegrid <- expand.grid(n.trees=n.trees,interaction.depth=interaction.depth,shrinkage=shrinkage,n.minobsinnode=minobsinnode)

#10-fold cv
control <- trainControl(method='cv', number=10, savePredictions ="all")

#initialize matrix to hold the B residual vectors for this model
residuals_star<-matrix(data=0,nrow=num_countries,ncol=B)

#for each new response vector, create model and find preval fits
for(i in 1:B){
IA$y_b_star<-y_b_star[,i]
boost<- train(y_b_star~.,data=IA, method='gbm',tuneGrid=tunegrid, trControl=control, verbose=F)
allPreds<-boost$pred
allPreds<-allPreds[order(allPreds$rowIndex),]
allPreds<-allPreds[,1:2] #no need to cube since y_b_star already cubed 
residuals_star[,i]<-allPreds$obs-allPreds$pred

#update user on progress
if(i%%100==0){
  print(i)
}
}

#STEP 5: CREATE NULL DISTRIBUTION and PLOT RESULTS

#find the RMSE for each residual vector for intrinsic + actionable model
IA_RMSE<-sqrt((1/num_countries)*colSums(residuals_star^2))
#find the RMSE for each residual vector for intrinsic model
I_RMSE<-sqrt((1/num_countries)*colSums(bootstrappedResid^2))
#null distribution is the difference of these B rmse values

#plot the results
differences<-I_RMSE - IA_RMSE
hist(differences, main = "Null Distribution from Bootstrapping Scheme", xlab = "I_RMSE - IA_RMSE")
abline(v=observed_stat,col="red",lwd=3)
legend("topleft", "Observed Test Statistic", fill="red")

#Empirical p-value
empirical_p<-sum(differences>observed_stat)/B
print(paste0("p-value: ", empirical_p))
print(paste0("observed test statistic: ", observed_stat))


df = data.frame(null=differences)

p <-
  ggplot(df, aes(x=null)) + 
  geom_histogram(bins=30, colour="gray61", fill="gray61") +
  geom_vline(xintercept = observed_stat, linetype="dashed", 
             color = "blue", size=0.8) +
  theme_minimal() +
  xlab("RMSE difference") +
  ylab("Count") +
  ggtitle("Bootstrap Null Distribution for RMSE difference (10000 iterations)") + 
  #annotate(geom="text", x=-34, y=750, label="p-value: 0.001",
  #         color="blue") +
  annotate(geom="text", x=5, y=100, label="Observed RMSE \n difference: 19",
           color="blue") + geom_rug()
p

