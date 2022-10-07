#!/usr/bin/env Rscript

############################################################################################
# ::DESCRIPTION::
#     - Script to process feature matrix for analysis
#     - feature transformation, missing data imputation
#
# @ INPUT: [csv] CurrentDataset.csv: dataframe of features from 2_get_features.sql
# @ OUTPUT: [csv] features_{region_filter}.csv: dataframe with (X, X.groupings, Y)
############################################################################################
here::i_am("analysis/code/data_preprocessing/R/4_process_XY.R")

library(here)
library(softImpute)
library(dplyr)
library(caret)
library(ggplot2)
library(devtools)
library(reshape)
library(factoextra)
set.seed(100)

# set to root directory of repo
#setwd("/Users/minwoosun/repos/covid_mortality") 
path_input = here::here("analysis/data/")
path_output = here::here("analysis/data/preprocessed/")

# filtering for EU + NA countries using iso3
# this could be generalized to any vector of countries
select.region.only = F
extension=""
if (select.region.only){
  western <- c("USA", "CAN", "UKR", "BLR", "DEU", "POL", "MDA", "FRA", "ITA", "GBR", 
               "FIN", "CHE", "SWE", "GRC", "NLD", "SRB", "ROU", "HUN", "HRV", "NOR", 
               "MLT", "ALB", "AUT", "CZE", "LTU", "BEL", "CYP", "DNK", "ISL", "MCO", 
               "BGR", "EST", "MKD", "LVA", "BIH", "LUX", "MNE", "SVK", "SVN", "IRL", 
               "VAT", "AND", "LIE", "GIB", "SMR", "FRO", "IMN", "JEY", "ALA", "SJM")
  region_filter = western
  extension = "_western"
}


######################
# Data preprocessing #
######################
# load data
df.X = read.csv(paste0(path_input, "preprocessed/FeatureDataset.csv"))
df.Y = read.csv(paste0(path_input, "preprocessed/target_WHO.csv"))

# - remove urban pop % bc of correlation with ppl per sq km
# - remove adult literacy rate bc outlier and correlation with % people who use internet. 
# - remove avg tests per thousand per day since it was derived by us from other features across different 
#   sites and may not be accurate. also highly correlated with number of days with >1 test per thousand
cols_to_remove <- c("Average_Tests_Per_Thousand_Per_Day",
                    "Urban_Pop_Percentage",
                    "Adult_Literacy_Rate",
                    "Total_Cases_Per_1000_People",
                    "Days_Until_Income_Support",    
                    "Days_Until_Freeze_Some_Financial_Obligations", 
                    "Total_Days_Income_Support", 
                    "Total_Freeze_Some_Financial_Obligations")
df.X <- df.X %>% select(!all_of(cols_to_remove))

# remove kosovo (missing for response variable)
# df.X <- df.X[-which(df.X$Entity == "Kosovo"),]

# remove countries with missing pop (small population)
df.X <- df.X[-which(is.na(df.X$Population)),]

## filter for countries with over 10M population
# df.X <- df.X[df.X$Population > 10000000,] 

# merge with target variable
df.Y = df.Y[,c("iso3", "excess_death")]
df = merge(df.X, df.Y, by.x="iso3c", by.y="iso3")

# filter for EU and NA countries
if (select.region.only){
  df <- df[df$iso3c %in% region_filter,]
}

#remove iso3c and country name
iso <- df$iso3c
df = df[,-c(1,2)]

# Change "Days Until" Columns to Ranks  
#   - missing data if country did not implement a policy in the time frame. 
#   - replace column with ranks and assign missing obs highest rank
#   - "Days_Until" cols describe how long it took for govt to implement a policy

daysUntilIndx<-grep("Days_Until", colnames(df)) 
DaysUntilCols<-df[,daysUntilIndx]

# Make artificially large constant for NA's so they will be given last rank
DaysUntilCols[is.na(DaysUntilCols)] <- 1000000
for(i in 1:ncol(DaysUntilCols)){
  DaysUntilCols[,i]<-rank(DaysUntilCols[,i],na.last =TRUE,ties.method = "average")
}
#replace with ranks
df[,daysUntilIndx]<-DaysUntilCols

# resplit X and Y for processing X
df.X = df %>% select(-excess_death)
df.Y = df %>% select(excess_death)




###############################
# Drop categorical variables  #
###############################
drop.cols = c("Region.Name", "Sub.region.Name")
df.X.cat = df.X %>% select(drop.cols)
df.X.num = df.X %>% select(-one_of(drop.cols))


#######################
# Impute missing data #
#######################

# run if there is any NA in df.X
if (any(is.na(df.X.num))){
  # run svd 
  df.fits <- softImpute(df.X.num, trace=TRUE, type = "svd")
  df.complete = df.fits$u %*% (df.fits$d * diag(2)) %*% t(df.fits$v)
  
  # impute df.X with svd result
  df.X.copy <- df.X.num
  df.X.na.indices = which(is.na(df.X.num), arr.ind=TRUE)
  df.X.copy[df.X.na.indices] = df.complete[df.X.na.indices]
  df.X.num = df.X.copy
  df.X = df.X.num %>% as.matrix # (152 x 35)
}



#################################
# Log transform skewed features #
#################################
# X[,'population'] = log(X[,'population']+1)
# X[,"People_Per_Sq_Km_of_Land"] = log(X[,"People_Per_Sq_Km_of_Land"]+.01)
# X[,"People_Per_Sq_Km_of_Land"] = log(X[,"People_Per_Sq_Km_of_Land"]+.01)
# X[,"GDP_Per_Capita"] = log(X[,"GDP_Per_Capita"]+.01)
# X[,"Health_Expenditure_Per_Capita"] = log(X[,"Health_Expenditure_Per_Capita"]+.01)


#################################################################
# Group together policy-related features using grouping methods #
#################################################################

# normalize just for PCA (final output not normalized)
X.unscaled = df.X
X = scale(df.X)

### FIRST OPTION: Group many variables together into 2 groups: ###

## First group represents government initial response time to COVID ##
daysUntilIndx<-grep("Days_Until", colnames(X))
DaysUntilCols<-X[,daysUntilIndx]
pc <- prcomp(DaysUntilCols,center = TRUE,scale. = TRUE)

#signs of rotation matrix for pca are arbitrary
#need to ensure interpretation of coefficients in future models is always consistent
#define these pca variables such that a greater positive value represents a more stringent policy

#for this first variable, waiting less days = more stringent
#so if rotation matrix has negative entries for PCA1, no change needed (bc higher values of original variables contribute negatively to pca variable)
#if rotation matrix has positive entries for PCA1, multiply pc$x by -1.
#pc$x is rotated data (i.e. data%*%rotationMatrix)
#Therefore multiplying pc$x  by -1 is same as multiplying rotations by -1

#all original variables need to contribute in same direction for pca variable to be interpretable
#get direction that is most common, and throw out other variables
if(sum(sign(pc$rotation[,1])) < 0){
  throwOut<-which(pc$rotation[,1]>0)
  if (length(throwOut) != 0){
    DaysUntilCols<-DaysUntilCols[,-throwOut]    
  }
}else{
  throwOut<-which(pc$rotation[,1]<0)
  if (length(throwOut) != 0){
    DaysUntilCols<-DaysUntilCols[,-throwOut]    
  }
}

#now that all original variables contribute to pca variable in same direction, make positive = more stringent
pc <- prcomp(DaysUntilCols,center = TRUE,scale. = TRUE)
if(pc$rotation[1,1]>0){
  pc$x<-pc$x*-1 #change for the model
  pc$rotation<-pc$rotation*-1 #change for the plot (pc$x already multiplied by rotation matrix)
}

X<-cbind(X[,-daysUntilIndx],pc$x[,1]) #replace all the original cols with this one PC
X.unscaled <- cbind(X.unscaled, pc$x[,1])

colnames(X)[colnames(X) == ""] <- 'Govt_Swiftness_Stringency'
colnames(X.unscaled)[colnames(X.unscaled) == ""] <- 'Govt_Swiftness_Stringency'
#view correlation btwn input variables and first PC via the plot
fviz_pca_var(pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

## Second group represents duration of government stringency across time horizon ##
#here, positive entries already equate more stringency

TotalDaysIndx<-grep("Total", colnames(X))
TotalDaysCols<-X[,TotalDaysIndx]
pc <- prcomp(TotalDaysCols,center = TRUE,scale. = TRUE)

#get direction that is most common, and throw out other variables
if(sum(sign(pc$rotation[,1])) < 0){
  throwOut<-which(pc$rotation[,1]>0)
  if (length(throwOut) != 0){
    DaysUntilCols<-DaysUntilCols[,-throwOut]    
  }
}else{
  throwOut<-which(pc$rotation[,1]<0)
  if (length(throwOut) != 0){
    DaysUntilCols<-DaysUntilCols[,-throwOut]    
  }
}


#now that all original variables contribute to pca variable in same direction, make positive = more stringent
pc <- prcomp(TotalDaysCols,center = TRUE,scale. = TRUE)
if(pc$rotation[1,1]<0){
  pc$x<-pc$x*-1 #change for the model
  pc$rotation<-pc$rotation*-1 #change for the plot (pc$x already multiplied by rotation matrix)
}

X<-cbind(X[,-TotalDaysIndx],pc$x[,1])#replace all the original cols with this one PC
X.unscaled <- cbind(X.unscaled, pc$x[,1])
colnames(X)[colnames(X) == ""] <- 'Govt_Persistent_Stringency'
colnames(X.unscaled)[colnames(X.unscaled) == ""] <- 'Govt_Persistent_Stringency'
#view correlation btwn input variables and first PC via the plot
fviz_pca_var(pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


#######################
# Combine variables   #
#######################
df_XY = cbind(X.unscaled, df.X.cat, excess_death = df.Y)
df_XY = cbind(iso=data.frame(iso), df_XY) # append iso

#######################
# Merge trust dataset #
#######################
trust <- read.csv(here::here("analysis/data/preprocessed/trust.csv"))
df_XY_trust  <- merge(x = df_XY, y = trust, by.x="iso",by.y ="iso3c")

######################
# Output data matrix #
######################
# no log transforms, no scaling except for groupings

# save without trust
out_file = paste0(path_output,"XY_WHO",extension,".csv")
write.csv(df_XY, out_file, row.names=FALSE)
print(paste0("File saved to: ",out_file))

# save with trust
out_file = paste0(path_output,"XY_WHO_trust",extension,".csv")
write.csv(df_XY_trust, out_file, row.names=FALSE)
print(paste0("File saved to: ",out_file))

print("***************** 4_process_XY.R completed ******************")