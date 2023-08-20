here::i_am("analysis/code/bootstrap_delta.R")
library(here)
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(gbm)
library(gridExtra)

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


#filter for 5M
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

# static features
index.static = which(names(X) %in% c("Population",
                                     "Obese_Adult_Percentage",
                                     "Hospital_Beds_Per_1000",
                                     "Nurses_And_Midwives_Per_1000",
                                     "People_Per_Sq_Km_of_Land" ,
                                     "GDP_Per_Capita",
                                     "Health_Expenditure_Per_Capita",
                                     "Age_65_Older_Percent",
                                     "Trust_In_Neighborhood",
                                     "Trust_In_Govt"
))

# modifiable features -- exclude grouping, policy 
index.modifiable = which(names(X) %in% c("Percent_One_Dose_As_Of_Nov_1",
                                         "Total_Days_Over_1_Test_Per_Thousand"
))  

# modifiable policy features grouped
index.policy.grouped = which(names(X) %in% c("Govt_Swiftness_Stringency",
                                             "Govt_Persistent_Stringency"))

# index.region = which(names(X) %in% names.sub.region)

# index for dummy variable continent
index.region = which(names(X) %in% c("Region.Name.Africa",
                                     "Region.Name.Americas",
                                     "Region.Name.Asia",
                                     "Region.Name.Europe"
))

index.trust = which(names(X) %in% c( "Trust_Covid_Advice_Govt"))


#######################################
# Fit initial GBM // best hyperparams #
#######################################

# select features for X
index.intrinsic = c(index.static,
                      index.region)

index.modifiable = c(index.static,
                        index.region,
                        index.trust,
                        index.modifiable,
                        index.policy.grouped)

best_params_intrinsic =  data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 10)

best_params_modifiable = data.frame(interaction.depth = 2,
                                    n.trees = 1600,
                                    shrinkage = .007,
                                    n.minobsinnode = 7)

###################
# Bootstrap index #
###################
N.bootstrap.final = 100
N.bootstrap = 150 # total bootstrap iteration
X_bootstrap_list = list()
N.country = dim(X)[1]

for (i in 1:N.bootstrap){
  X_bootstrap_list[[i]] = sample(x=1:N.country, size=N.country, replace = TRUE)
}


#################################################
# repeated CV procedure for each bootstrapped X # 
#################################################

k = 10 # k-fold CV
df = data.frame(iso=character(),
                pred.I = numeric(),
                pred.IM = numeric(),
                delta = numeric(),
                bootstrap = integer(),
                stringsAsFactors=FALSE
)

# one iteration of k-fold CV
# randomly shuffle data X and Y
# ---------------------
for (b in 1:N.bootstrap){
  #for (r in 1:N.repeatedCV){
    
    print(paste0("bootstrap: ",b))

    shuffled_index = sample(X_bootstrap_list[[b]])
    X_ = cbind(X, Y=Y, iso=iso)
    X_ = X_[shuffled_index,]
    iso_ = X_$iso
    X_ = X_ %>% select(-iso)
    
    
    # create k folds for CV
    # ---------------------
    X_index = floor(as.numeric(rownames(X_)))
    X_index_uniq = unique(X_index) # do this cos of the constraint
    R = length(X_index_uniq) %% k
    
    k_ = floor(length(X_index_uniq) / k)
    
    # divide indices in to k groups in a list 
    split_index = split(X_index_uniq , ceiling(seq_along(X_index_uniq )/k_))
    
    # easier to work with index
    dups = as.numeric(grep("\\.", rownames(X_), value = TRUE))
    for (i in 1:length(split_index)){
      index.dups = which(floor(dups) %in% split_index[[i]])
      split_index[[i]] = c(split_index[[i]], dups[index.dups])
    }
    
    # redistribute remainder to smallest fold
    # if R > 1, will need to iterate and repeat this procedure
    if (R != 0 ){
      size_rank = lapply(split_index, length) %>% unlist %>% as.vector
      index.small = which.min(head(size_rank, -1))
      split_index[[index.small]] = c(split_index[[index.small]], split_index[[length(split_index)]])
      split_index[[length(split_index)]] <- NULL
    }
    
    # rearrange iso
    # iso order changed b/c duplicate constraint and balancing
    index.iso.initial = cbind(id=rownames(X_), iso=iso_) %>% data.frame
    index.new = split_index %>% unlist %>% as.vector %>% as.character() %>% data.frame
    colnames(index.new) = "iso"
    iso_final = merge(index.new, index.iso.initial, all.y=TRUE, sort=FALSE)
    
    #############
    # k-fold CV #
    #############
    
    preds.intrinsic = list()
    preds.modifiable = list()
    preds.iso = list()
    
    for (i in 1:k){
      index.match = which(as.numeric(rownames(X_)) %in% as.vector(unlist(split_index[c(1:k)[-i]])) )
      index.predict = which(as.numeric(rownames(X_)) %in% split_index[[i]])
      
      # prediction iso
      preds.iso[[i]] = iso_final$iso[index.predict]
    
      #############
      # INTRINSIC #
      #############
      allData = X_[index.match, c(index.intrinsic, 39)]
      
      # enter model here
      control <- trainControl(method='none')
      
      #perform cv process
      gbm.params = best_params_intrinsic
      boost <- train(Y~., data=allData, 
                     method='gbm',
                     tuneGrid=gbm.params, 
                     trControl=control, 
                     verbose=F)
      
      # predictions from gbm
      Y.hat = predict(boost, newdata = X_[index.predict, index.intrinsic])
      preds.intrinsic[[i]] = Y.hat
    
    
     ##########################
     # INTRINSIC + MODIFIABLE #
     ##########################
      allData = X_[index.match, c(index.modifiable, 39)]
      
      # enter model here
      control <- trainControl(method='none')
      
      #perform cv process
      gbm.params = best_params_modifiable
      boost <- train(Y~., data=allData, 
                     method='gbm',
                     tuneGrid=gbm.params, 
                     trControl=control, 
                     verbose=F)
      
      # predictions from gbm
      Y.hat = predict(boost, newdata = X_[index.predict, index.modifiable])
      preds.modifiable[[i]] = Y.hat
    }
    
    preds.iso = preds.iso %>% unlist %>% as.vector
    preds.intrinsic = preds.intrinsic  %>% unlist %>% as.vector
    preds.modifiable = preds.modifiable  %>% unlist %>% as.vector
    
    df_ = cbind(iso=as.character(preds.iso), 
                pred.I=as.numeric(preds.intrinsic^3), 
                pred.IM=as.numeric(preds.modifiable^3), 
                delta = as.numeric(preds.modifiable^3-preds.intrinsic^3),
                bootstrap = as.integer(rep(b, length(preds.iso)))
                ) %>% data.frame
    
    df = rbind(df, df_)
    
  #}
}


df.count = df %>% group_by(iso) %>% dplyr::summarize(count=n())
all(df.count$count  >= N.bootstrap.final) # check that there's enough per country

# select same amount of bootstrap delta for each country
index.keep = c()
for (ISO in unique(df$iso)){
  index.ISO = which(df$iso == ISO)
  index.keep = c(index.keep, index.ISO[1:N.bootstrap.final])
}

# compute the sample means
df$delta = df$delta %>% as.numeric
df.delta = df %>%
  group_by(iso) %>%
  dplyr::summarize(avg=mean(delta, na.rm=T))

# compute the bootstrap standard deviations
df.delta.se = df[index.keep,] %>% 
  group_by(iso) %>% 
  dplyr::summarize(se=sd(delta))

# compute the confidence intervals
#df.delta.initial$delta = df.delta.initial$delta %>% as.numeric
df.final = merge(df.delta, df.delta.se, by="iso")
df.final$ci_lo = df.final$avg - (1.96 * df.final$se / sqrt(N.bootstrap.final))
df.final$ci_hi = df.final$avg + (1.96 * df.final$se / sqrt(N.bootstrap.final))


#######################################
# Plot confidence intervals for delta #
#######################################
country.filter = c("NOR", "KOR", "CAN", "AUS", "MYS", "USA", "IND", "RUS", "PER", "SVK")

df.final_ = df.final[df.final$iso %in% country.filter,]
df.final_ = df.final_[order(df.final_$avg),] 
row.names(df.final_) = 1:nrow(df.final_)

ggplot(df.final_, aes(x=avg, y=reorder(iso,avg))) +   
  geom_vline(xintercept = 0, linetype=1, color="red", size=0.5) +
  geom_point() +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), width=0.1) +
  xlab("Delta") +
  ylab("Country ISO") +
  theme_minimal()

# plot all
ggplot(df.final, aes(x=avg, y=reorder(iso,avg))) +    
  geom_vline(xintercept = 0, linetype=1, color="red", size=0.5) +
  geom_point() +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), width=0.1) +
  xlab("Delta") +
  ylab("Country") +
  theme_minimal() + 
  theme(axis.text.y=element_blank())


###################################
# RMSE comparison: I vs IA models #
###################################

df_ = merge(df, cbind(iso, as.numeric(Y)^3), by="iso")
colnames(df_)[6] = "Y"
df_$pred.I = df_$pred.I %>% as.numeric
df_$pred.IM = df_$pred.IM %>% as.numeric
df_$Y = df_$Y %>% as.numeric

df_ = df_ %>% mutate(sqerror_I = sqrt((Y-pred.I)^2))
df_ = df_ %>% mutate(sqerror_IM = sqrt((Y-pred.IM)^2))

df_RMSE_I = df_ %>% group_by(iso) %>% dplyr::summarize(RMSE_I = mean(sqerror_I, na.rm=TRUE))
df_RMSE_IM = df_ %>% group_by(iso) %>% dplyr::summarize(RMSE_M = mean(sqerror_IM, na.rm=TRUE))

df_RMSE = merge(df_RMSE_I, df_RMSE_IM, by="iso")
df_RMSE = df_RMSE %>% mutate(improve = df_RMSE$RMSE_I > df_RMSE$RMSE_M)

# filter for countries where adding modifiable variables decreased the error
df_RMSE[df_RMSE$improve,] 

df_final = merge(df_RMSE, df.delta, by="iso")
df_final[df_final$improve,]




###############################################
# Plot delta for countries with improved pred #
###############################################

country.filter = df_final$iso[df_final$improve == TRUE]

df.final_ = df.final[df.final$iso %in% country.filter,]
df.final_ = df.final_[order(df.final_$avg),] 
row.names(df.final_) = 1:nrow(df.final_)

ggplot(df.final_, aes(x=avg, y=reorder(iso,avg))) +   
  geom_vline(xintercept = 0, linetype=1, color="red", size=0.5) +
  geom_point() +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), width=0.1) +
  xlab("Delta") +
  ylab("Country ISO") +
  theme_minimal()




##################################
# Do we get the direction right? #
##################################


df_dir = df_ %>% select(iso, pred.I, pred.IM, Y)

df_dir_avg = df_dir %>% 
              group_by(iso) %>% 
              dplyr::summarize(yhat_I = mean(pred.I),
                               yhat_IM = mean(pred.IM),
                               y = mean(Y))

correct_dir = c()

for (i in 1:nrow(df_dir_avg)){
  if ((df_dir_avg$yhat_I[i] > df_dir_avg$y[i]) & (df_dir_avg$yhat_I[i] > df_dir_avg$yhat_IM[i])){
    correct_dir[i] <- 1
  } else if ((df_dir_avg$yhat_I[i] < df_dir_avg$y[i]) & (df_dir_avg$yhat_I[i] < df_dir_avg$yhat_IM[i])){
    correct_dir[i] <- 1
  }
  else {
    correct_dir[i] <- 0
  }
}

df_final_dir <- data.frame(cbind(df_dir_avg, correct_dir))

df_final2 = merge(df_final, df_final_dir)

# df_final_dir$iso[df_final_dir$correct_dir==1]


country.filter = df_final2$iso[df_final2$correct_dir == 1]

df.final_ = df.final[df.final$iso %in% country.filter,]
df.final_ = df.final_[order(df.final_$avg),]
row.names(df.final_) = 1:nrow(df.final_)

plot.conf_int <- ggplot(df.final_, aes(x=avg, y=reorder(iso,avg))) +
  geom_vline(xintercept = 0, linetype=1, color="red", size=0.5) +
  geom_point() +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), width=0.1) +
  xlab("Delta") +
  ylab("Country ISO") +
  theme_minimal()







# Get vaccine and trust valeus for 10 countries
df_xy = read.csv(here::here("analysis/data/preprocessed","XY_WHO_trust.csv"))
df.filtered = df_xy[df_xy$iso %in% country.filter,]

df.final_sorted = df.final_ %>% select(iso, avg)

df.filtered = df.filtered %>% select(iso,
                                      Percent_One_Dose_As_Of_Nov_1,
                                      Trust_Covid_Advice_Govt)

df_table = merge(df.final_sorted, df.filtered)


df_table = df_table[order(df_table$avg, decreasing=T),]
df_table$Group = ifelse(df_table$avg > 0, "Pos_Delta", "Neg_Delta")

median_one_dose <- df_table %>% group_by(Group) %>% dplyr::summarise(grp.median=median(Percent_One_Dose_As_Of_Nov_1))
median_covid_advice <- df_table %>% group_by(Group) %>% dplyr::summarise(grp.median=median(Trust_Covid_Advice_Govt))



  # plot histograms comparing countries with delta below and above 0
plot.trust <-  ggplot(df_table, aes(Trust_Covid_Advice_Govt, color = Group, fill=Group)) + 
                  geom_density(alpha = 0.5, size=1) +
                  theme_minimal() +
                  theme(text = element_text(size=13.5))+
                  geom_vline(data=median_covid_advice, aes(xintercept=grp.median, color=Group),
                             linetype="dashed", size=1)
                  xlab("Trust Covid Advice Govt")
                 

plot.vax <-  ggplot(df_table, aes(Percent_One_Dose_As_Of_Nov_1, color = Group, fill=Group)) + 
              geom_density(alpha = 0.5, size=1) +
              theme_minimal() +
              theme(text = element_text(size=13.5))+
              geom_vline(data=median_one_dose, aes(xintercept=grp.median, color=Group),
                         linetype="dashed", size=1) +
              xlab("Percent One Dose as of Nov. 1")

hlay <- rbind(c(1,1),
              c(1,1),
              c(1,1),
              c(2,3),
              c(2,3))
grid.arrange(plot.conf_int,plot.trust, plot.vax, layout_matrix=hlay)
