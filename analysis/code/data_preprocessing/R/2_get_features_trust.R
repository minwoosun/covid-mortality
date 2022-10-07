
############################################################################################
#Author: David Troxell
#Date: 7/23/22
#Description: This script outputs a csv of trust-related variables
            # 2 input csvs are used and both were unchanged after downloading from websites
            # 2020 csv downloaded on 7/18/22 from https://wellcome.org/reports/wellcome-global-monitor-covid-19/2020#read-chapters-online-b73f
            # 2018 csv downloaded on 7/18/22 from https://wellcome.org/reports/wellcome-global-monitor/2018#contact-us-f270
############################################################################################

#install.packages("readxl")
here::i_am("analysis/code/data_preprocessing/R/2_get_features_trust.R")

library(here)
library("readxl")
library(reshape)
library(ggplot2)

#### Obtain Citizens' Thoughts Regarding COVID during COVID ####
#Survey questions conducted btwn oct 2020 to Feb 2021

#read-in the downloaded csv from the site
Survey2020 <- read_excel(here::here("analysis/data/raw/trust/wgm-public-file-covid-crosstabs.xlsx")
                         ,sheet = "Country Tabs")
#grab survey questions
DataDescription<-Survey2020[,1:2]
#grab the percent values for each question for each country
CountryPercents<-Survey2020 [,colSums(is.na(Survey2020 ))==0]
Survey2020<-cbind(DataDescription,CountryPercents)

#get list of names for future joining
CountryNames<-t(CountryPercents[1,])
rownames(CountryNames) <- NULL #reset row indexes

#for each survey question, we want respondents who respond "a lot" or "some"
#this is why we add 1 to the row and sum the values

Indx<-which(Survey2020[,1]=="W15_1A Base Coronavirus Decisions on Scientific Advice: National Govt")
Trust_Covid_Advice_Govt<-as.numeric(t(CountryPercents[Indx,]))+as.numeric(t(CountryPercents[Indx+1,]))
Trust2020<-cbind(CountryNames,Trust_Covid_Advice_Govt)

Indx<-which(Survey2020[,1]=="W15_1B Base Coronavirus Decisions on Scientific Advice: Friends/Family")
Trust_Covid_Advice_FamFriends<-as.numeric(t(CountryPercents[Indx,]))+as.numeric(t(CountryPercents[Indx+1,]))
Trust2020<-cbind(Trust2020,Trust_Covid_Advice_FamFriends)

Indx<-which(Survey2020[,1]=="W15_1C Base Coronavirus Decisions on Scientific Advice: The W.H.O.")
Trust_Covid_Advice_WHO<-as.numeric(t(CountryPercents[Indx,]))+as.numeric(t(CountryPercents[Indx+1,]))
Trust2020<-cbind(Trust2020,Trust_Covid_Advice_WHO)

#convert all the gathered answers to df, fix data types, and  fix row indexes
Trust2020<-as.data.frame(Trust2020)
Trust2020[2:4] <- sapply(Trust2020[2:4],as.numeric)
Trust2020[Trust2020 == 0] <- NA

#### Obtain Citizens' Trust in Different Entities Pre-Pandemic ####
#Survey questions conducted 2018 and 2019

#download csv
Survey2018 <- read_excel(here::here("analysis/data/raw/trust/wgm2018-dataset-crosstabs-all-countries.xlsx"),
                         sheet="Crosstabs all countries")
#grab answers from 50+ y/o respondents in case we are interested
Ages50PlusIndx<-which(Survey2018[1,]=="50+")
Ages50Plus<-Survey2018[,Ages50PlusIndx]
#grab question results for each country
CountryTotals<-Survey2018$`National results`
#grab survey info
DataDescription<-Survey2018[,1:3]

#only need percents
CountryPercents<-cbind(DataDescription,CountryTotals,Ages50Plus)
#get unique list of country names for future joining
CountryNames<-unique(Survey2018[,1])
CountryNames<-CountryNames[-c(1:2),1]

#for each survey question, we want respondents who respond "a lot" or "some"
#this is why we add 1 to the row and sum the values

Indx<-which(Survey2018[,2]=="Q11A How much do you trust each of the following? How about the people in your neighborhood? Do you trust them a lot, some, not much, or not at all?")
Trust_In_Neighborhood<-as.numeric(t(CountryPercents[Indx,4]))+as.numeric(t(CountryPercents[Indx+1,4]))
Trust2018<-cbind(CountryNames, Trust_In_Neighborhood)

Indx<-which(Survey2018[,2]=="Q25 Do you strongly or somewhat agree, strongly or somewhat disagree or neither agree nor disagree with the following statement? Vaccines are safe.")
Vaccines_Safe_50Plus<-as.numeric(t(CountryPercents[Indx,5]))+as.numeric(t(CountryPercents[Indx+1,5]))
Trust2018<-cbind(Trust2018, Vaccines_Safe_50Plus)

#no need to add once since yes or no question
Indx<-which(Survey2018[,2]=="Q10B In (country), do you have confidence in each of the following, or not? How about Hospitals and Health Clinics.")
Confidence_In_Hospitals<-as.numeric(t(CountryPercents[Indx,4]))
Trust2018<-cbind(Trust2018, Confidence_In_Hospitals)

#Question not available for all Countries
Indx<-which(Survey2018[,2]=="Q11B How much do you trust each of the following? How about the national government in this country? Do you trust them a lot, some, not much, or not at all?")
Trust_In_Govt<-as.numeric(t(CountryPercents[Indx,4]))+as.numeric(t(CountryPercents[Indx+1,4]))
#find which countries we have the info for
Trust_In_Govt_Available<-(CountryPercents[Indx,1])
#join with list of all countries in the survey and place NA where needed
dataToJoin<-data.frame("CountryNames"=Trust_In_Govt_Available,Trust_In_Govt)
Trust_In_Govt<-merge(x = CountryNames,y = dataToJoin, by.x=colnames(CountryNames),by.y ="CountryNames" ,all.x = TRUE)
Trust2018<-cbind(Trust2018, Trust_In_Govt[,2])


Indx<-which(Survey2018[,2]=="Q11D How much do you trust each of the following? How about journalists in this country? Do you trust them a lot, some, not much, or not at all?")
Trust_In_Journalists<-as.numeric(t(CountryPercents[Indx,4]))+as.numeric(t(CountryPercents[Indx+1,4]))
Trust2018<-cbind(Trust2018, Trust_In_Journalists)

  
Indx<-which(Survey2018[,2]=="Q12 In general, would you say that you trust science a lot, some, not much, or not at all?")
Trust_In_Science<-as.numeric(t(CountryPercents[Indx,4]))+as.numeric(t(CountryPercents[Indx+1,4]))
Trust2018<-cbind(Trust2018, Trust_In_Science)

#Question not available for all Countries. Follow same pattern as before
Indx<-which(Survey2018[,2]=="Q21 In general, how much do you trust medical and health advice from the government in this country? A lot, some, not much, or not at all?")
Trust_In_Science_From_Govt<-as.numeric(t(CountryPercents[Indx,4]))+as.numeric(t(CountryPercents[Indx+1,4]))
Trust_In_Govt_Available<-(CountryPercents[Indx,1])
dataToJoin<-data.frame("CountryNames"=Trust_In_Govt_Available,Trust_In_Science_From_Govt)
Trust_In_Science_From_Govt<-merge(x = CountryNames,y = dataToJoin, by.x=colnames(CountryNames),by.y ="CountryNames" ,all.x = TRUE)
Trust2018<-cbind(Trust2018,Trust_In_Science_From_Govt[,2])
Trust2018[Trust2018 == 0] <- NA #change 0 percent to NA

#rename columns for interpretability
names(Trust2018)[names(Trust2018) == 'Trust_In_Govt[, 2]'] <- 'Trust_In_Govt'
names(Trust2018)[names(Trust2018) == 'Trust_In_Science_From_Govt[, 2]'] <- 'Trust_In_Science_From_Govt'



#### Correlation Matrix of Trust Variables ####
#First, join 2018 and 2020 metrics

CompleteTrustMetrics<-merge(x = Trust2020,y = Trust2018, by.x=colnames(Trust2020)[1],by.y =colnames(Trust2018)[1], all.x = TRUE)
numericTrustMetrics<-CompleteTrustMetrics[,-1]
numericTrustMetrics<-numericTrustMetrics[complete.cases(numericTrustMetrics), ]

displayCorrMat<-function(numericTrustMetrics){
cormat1 <- round(x = cor(numericTrustMetrics), digits = 1)
cormat1[lower.tri(cormat1)]<- NA
melted_cormat1 <- melt(cormat1,as.is = TRUE)
 p<-ggplot(data = melted_cormat1, aes(X1, X2, fill = value))+
   geom_tile(color = "white")+geom_text(aes(label = value),size = 2)+
   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1,1), space = "Lab", 
                        name="Pearson\nCorrelation") +
   theme_minimal()+ 
   theme(axis.text.x = element_text(angle = 75, vjust = 1, 
                                    size = 8, hjust = 1))+
   coord_fixed()
 print(p)
}

displayCorrMat(numericTrustMetrics)
 
#remove some bc of multicollinearity
CompleteTrustMetrics<-subset(CompleteTrustMetrics,select=-c(Trust_Covid_Advice_WHO,Trust_In_Science,Trust_In_Science_From_Govt,Trust_Covid_Advice_FamFriends))
# country_names<-CompleteTrustMetrics[,1]
# numericTrustMetrics<-CompleteTrustMetrics[,-1] 
CompleteTrustMetrics<-CompleteTrustMetrics[complete.cases(CompleteTrustMetrics), ]
# CompleteTrustMetrics<-cbind(country_names,CompleteTrustMetrics)
# displayCorrMat(numericTrustMetrics)


# add iso code
isodict<-read.csv(here::here("analysis/data/raw/trust/isodict.csv"))
trust<-merge(x = CompleteTrustMetrics,y = isodict, by.x="V1",by.y ="Entity" ,all.x = TRUE)
# impute because countries population > 10M
trust[trust$V1=="Ivory Coast", "iso3c"]  <- "CIV" 
trust[trust$V1=="Czech Republic", "iso3c"]  <- "CZE"


#output final csv
path_output = here::here("analysis/data/preprocessed/")
out_file = paste0(path_output,"trust.csv")
write.csv(trust, out_file, row.names=FALSE)
print(paste0("File saved to: ",out_file))

print("****************** 2_trust_features.R completed ******************")
