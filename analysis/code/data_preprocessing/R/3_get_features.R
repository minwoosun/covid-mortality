#!/usr/bin/env Rscript

############################################################################################
# ::DESCRIPTION::
#     
#     
#
# @ INPUT: [csv] 
# @ OUTPUT: [csv] 
############################################################################################

here::i_am("analysis/code/data_preprocessing/R/3_get_features.R")

library(here)
library(DBI)
library(RSQLite)

dataBase <- dbConnect(RSQLite::SQLite(), ":memory:")

# pre-processing for all csv files from worldbank.org
worldBankPreprocessing_<-function(filename){
  table<-read.csv(filename, header=FALSE)
  table<-table[-c(1:2),]
  names(table) <- table[1,]
  table <- table[-1,]
  name = substring(filename, 1, nchar(filename)-4)
  
  # update country names to match AllCountries table
  table[table['Country Name'] == "Bahamas, The", "Country Name"] <- "Bahamas"  
  table[table['Country Name'] == "Brunei Darussalam", "Country Name"] <- "Brunei"  
  table[table['Country Name'] == "Cabo Verde", "Country Name"] <- "Cape Verde"  
  table[table['Country Name'] == "Czech Republic", "Country Name"] <- "Czechia"
  table[table['Country Name'] == "Congo, Rep.", "Country Name"] <- "Congo"  
  table[table['Country Name'] == "Congo, Dem. Rep.", "Country Name"] <- "Democratic Republic of Congo"  
  table[table['Country Name'] == "Faroe Islands", "Country Name"] <- "Faeroe Islands"
  table[table['Country Name'] == "Hong Kong SAR, China", "Country Name"] <- "Hong Kong"  
  table[table['Country Name'] == "Iran, Islamic Rep.", "Country Name"] <- "Iran"    
  table[table['Country Name'] == "Lao PDR", "Country Name"] <- "Laos"  
  table[table['Country Name'] == "Macao SAR, China", "Country Name"] <- "Macao"
  table[table['Country Name'] == "Russian Federation", "Country Name"] <- "Russia"
  table[table['Country Name'] == "Korea, Rep.", "Country Name"] <- "South Korea"
  table[table['Country Name'] == "Syrian Arab Republic", "Country Name"] <- "Syria"  
  table[table['Country Name'] == "Timor-Leste", "Country Name"] <- "Timor"    
  table[table['Country Name'] == "Virgin Islands (U.S.)", "Country Name"] <- "United States Virgin Islands"   
  table[table['Country Name'] == "Venezuela, RB", "Country Name"] <- "Venezuela"  
  table[table['Country Name'] == "Yemen, Rep.", "Country Name"] <- "Yemen"  
  table[table['Country Name'] == "Egypt, Arab Rep.", "Country Name"] <- "Egypt"
  table[table['Country Name'] == "Gambia, The", "Country Name"] <- "Gambia" 
  table[table['Country Name'] == "Kyrgyz Republic", "Country Name"] <- "Kyrgyzstan" 
  table[table['Country Name'] == "Slovak Republic", "Country Name"] <- "Slovakia" 
  
  # Eritrea missing recent population (2011 last)
  # Palestine, Taiwan no population in World Bank 
  
  dbWriteTable(dataBase, name, table)
}

#' run worldBankPreprocessing_ if the table does not exist
worldBankPreprocessing <- function(file_path, dataBase){
  file_name <- tools::file_path_sans_ext(file_path)
  if (!(file_name %in% dbListTables(dataBase))){
    worldBankPreprocessing_(file_path)
  }
}

# pre-processing for all other csv files
addToDB_<-function(filename){
  table<-read.csv(filename)
  name = substring(filename, 1, nchar(filename)-4)
  dbWriteTable(dataBase, name, table)
}

#' run addToDB_ if the table does not exist
addToDB <- function(file_path, dataBase){
  file_name <- tools::file_path_sans_ext(file_path)
  if (!(file_name %in% dbListTables(dataBase))){
    addToDB_(file_path)
  }
}

#add this table first just so we can get country list
file_path<- here("analysis/data/raw/variables_csv/covid-vaccination-policy.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)

#### Entity ####
makeCountriesDict<-paste0("CREATE TABLE AllCountries as select Entity from '", file_name, "' group by Entity")
dbExecute(dataBase, makeCountriesDict)
FeatureDataset<-dbReadTable(dataBase, "AllCountries")

#### iso_code ####
#Table from https://ourworldindata.org/covid-stringency-index. Downloaded 6/25/2022, 3:21 PST
file_path<- here("analysis/data/raw/variables_csv/owid-covid-data.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select iso_code as  iso3c from AllCountries as A left join '", file_name, "' as v on A.Entity=v.location group by A.Entity order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(table,FeatureDataset)

#### Days_Until_All_Vulnerable_Vacc_Elig ####
#table from https://ourworldindata.org/covid-vaccination-policy. Downloaded 6/25/2022, 2:05 PST
file_path<- here("analysis/data/raw/variables_csv/covid-vaccination-policy.csv")
file_name <- tools::file_path_sans_ext(file_path)
query<-paste0("select  julianday(min(Day))-julianday('2020-01-01') as Days_Until_All_Vulnerable_Vacc_Elig
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and vaccination_policy>=3 group by A.Entity order by A.Entity")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Percent_One_Dose_As_Of_Nov_1 ####
#table from https://ourworldindata.org/covid-vaccinations. Downloaded 6/25/2022, 2:25 PST
file_path<- here("analysis/data/raw/variables_csv/share-people-vaccinated-covid.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select  max(people_vaccinated_per_hundred) as Percent_One_Dose_As_Of_Nov_1
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and Day<'2021-11-01'
group by A.Entity
order by A.Entity")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Days_Until_Masks_Recommended ####
#table from https://ourworldindata.org/covid-face-coverings. Downloaded 6/25/2022, 2:40 PST
file_path<- here("analysis/data/raw/variables_csv/face-covering-policies-covid.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select julianday(min(Day))-julianday('2020-01-01') as Days_Until_Masks_Recommended
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and facial_coverings>=1
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Days_Until_Masks_Required ####
#Same as above
query<-paste0("select A.Entity, julianday(min(Day))-julianday('2020-01-01') as Days_Until_Masks_Required
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and facial_coverings>=3 
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Masks_Required_Public #### 
#Same as above 
query<-paste0("select A.Entity,sum(case when facial_coverings is not null then 1 else 0 end) as Total_Days_Masks_Required_Public
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and facial_coverings>=3 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Masks_At_Least_Recommended #### 
#Same as above 
query<-paste0("select A.Entity, sum(case when facial_coverings is not null then 1 else 0 end) as 	Total_Days_Masks_At_Least_Recommended
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and facial_coverings>=1 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Days_Until_Workplace_Closures_Except_Key ####
#Table from https://ourworldindata.org/covid-school-workplace-closures. Downloaded 6/25/2022, 2:55 PST
file_path<- here("analysis/data/raw/variables_csv/workplace-closures-covid.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select  A.Entity, julianday(min(Day))-julianday('2020-01-01') as Days_Until_Workplace_Closures_Except_Key
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and workplace_closures>=3
  group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Workplace_Closures_Except_Key ####
#same as above
query<-paste0("select  A.Entity ,sum(case when workplace_closures is not null then 1 else 0 end) as  Total_Days_Workplace_Closures_Except_Key
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and workplace_closures>=3 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Workplace_Closures_Recommended ####
#same as above
query<-paste0("select A.Entity, sum(case when workplace_closures is not null then 1 else 0 end) as  Total_Days_Workplace_Closures_Recommended
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and workplace_closures>=1 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Stay_At_Home_Required_Except_Essentials ####
#Table from https://ourworldindata.org/covid-stay-home-restrictions. Downloaded 6/25/2022, 3:08 PST
file_path<- here("analysis/data/raw/variables_csv/stay-at-home-covid.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, sum(case when stay_home_requirements is not null then 1 else 0 end) as  Total_Days_Stay_At_Home_Required_Except_Essentials
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and stay_home_requirements>=2 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Comprehensive_Contact_Tracing ####
#Table from https://ourworldindata.org/covid-testing-contact-tracing. Downloaded 6/25/2022, 3:16 PST
file_path<- here("analysis/data/raw/variables_csv/covid-contact-tracing.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, sum(case when contact_tracing not null then 1 else 0 end) as Total_Days_Comprehensive_Contact_Tracing
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and contact_tracing>=2 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Avg_Gov_Stringency_Index ####
#Table from https://ourworldindata.org/covid-stringency-index. Downloaded 6/25/2022, 3:21 PST
#same as owid dataset
file_path <- here("analysis/data/raw/variables_csv/owid-covid-data.csv")
file_name <- tools::file_path_sans_ext(file_path)
query<-paste0("select A.Entity,  avg(stringency_index) as  Avg_Gov_Stringency_Index
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.location
and date<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Days_Until_Income_Support ####
#table from https://ourworldindata.org/covid-income-support-debt-relief. Downloaded 6/25/2022, 3:27 PST
file_path <- here("analysis/data/raw/variables_csv/income-support-covid.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, julianday(min(Day))-julianday('2020-01-01') as Days_Until_Income_Support
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and income_support>=1
  group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Income_Support ####
#same as above
query<-paste0("select  A.Entity, sum(case when income_support not null then 1 else 0 end) as  Total_Days_Income_Support
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and income_support>=1 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Freeze_Some_Financial_Obligations ####
#other table from https://ourworldindata.org/covid-income-support-debt-relief. Downloaded 6/25/2022, 3:31 PST
file_path <- here("analysis/data/raw/variables_csv/debt-relief-covid.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, sum(case when debt_relief not null then 1 else 0 end) as Total_Freeze_Some_Financial_Obligations
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and debt_relief>=1 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Days_Until_Freeze_Some_Financial_Obligations ####
#same as above
query<-paste0("select A.Entity, julianday(min(Day))-julianday('2020-01-01') as Days_Until_Freeze_Some_Financial_Obligations
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and debt_relief>=1
  group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Days_Until_Testing_Key_Groups ####
#other table from https://ourworldindata.org/covid-testing-contact-tracing. Downloaded 6/25/2022, 3:56 PST
file_path <- here("analysis/data/raw/variables_csv/covid-19-testing-policy.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, julianday(min(Day))-julianday('2020-01-01') as Days_Until_Testing_Key_Groups
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and testing_policy>=1
  group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Open_Public_Testing ####
#same as above
query<-paste0("select A.Entity, sum(case when testing_policy not null then 1 else 0 end) as Total_Days_Open_Public_Testing
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and testing_policy>=3 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Days_Over_1_Test_Per_Thousand ####
#table from https://ourworldindata.org/coronavirus-testing. Downloaded 6/25/2022, 4:00 PST
file_path <- here("analysis/data/raw/variables_csv/daily-tests-per-thousand-people-smoothed-7-day.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, sum(case when new_tests_per_thousand_7day_smoothed not null then 1 else 0 end) as Total_Days_Over_1_Test_Per_Thousand
from AllCountries as A left join '", file_name,"' as v on A.Entity=v.Entity
and new_tests_per_thousand_7day_smoothed>=1 and Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Average_Tests_Per_Thousand_Per_Day ####
#same as above
query<-paste0("select A.Entity, avg(new_tests_per_thousand_7day_smoothed) *max(670-(julianday(min(Day))-julianday('2020-01-01')) ,0)/670 as Average_Tests_Per_Thousand_Per_Day
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.Entity
and d.Day<'2021-11-01'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Obese_Adult_Percentage ####
#Table from https://ourworldindata.org/obesity. Downloaded 6/25/2022,9:36 PST
file_path <- here("analysis/data/raw/variables_csv/share-of-adults-defined-as-obese.csv")
file_name <- tools::file_path_sans_ext(file_path)
addToDB(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, d.* /*as Obese_Adult_Percentage*/
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.Entity
and Year=2016
group by A.Entity
order by A.Entity;")
#name threw error in r/sql. select all and then filter for this feature
table<-dbGetQuery(dataBase, query)
Obese_Adult_Percentage<-table[,5]
FeatureDataset<-cbind(FeatureDataset, Obese_Adult_Percentage)

#------------------- World Bank data ----------------------------#

#### Urban_Pop_Percentage ####
#Table from https://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS. Downloaded 6/25/2022,9:06 PST
file_path <- here("analysis/data/raw/variables_csv/API_SP.URB.TOTL.IN.ZS_DS2_en_csv_v2_4151278.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select  A.Entity, d.'2020' as Urban_Pop_Percentage
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Hospital_Beds_Per_1000 ####
#Table from https://data.worldbank.org/indicator/SH.MED.BEDS.ZS. Downloaded 6/25/2022,9:54 PST
file_path <- here("analysis/data/raw/variables_csv/API_SH.MED.BEDS.ZS_DS2_en_csv_v2_4157707.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
#select most recent non-null value for each country (only go back to 2010, as over 10 years may not reflect present-day levels)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Hospital_Beds_Per_1000
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Nurses_And_Midwives_Per_1000 ####
#Table from https://data.worldbank.org/indicator/SH.MED.NUMW.P3. Downloaded 6/25/2022,10:04 PST
file_path <- here("analysis/data/raw/variables_csv/API_SH.MED.NUMW.P3_DS2_en_csv_v2_4157709.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Nurses_And_Midwives_Per_1000
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Percent_Using_Internet ####
#Table from https://data.worldbank.org/indicator/IT.NET.USER.ZS. Downloaded 6/25/2022,10:09 PST
file_path <- here("analysis/data/raw/variables_csv/API_IT.NET.USER.ZS_DS2_en_csv_v2_4150946.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as	Percent_Using_Internet
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Population ####
#Table from https://data.worldbank.org/indicator/SP.POP.TOTL. Downloaded 6/25/2022,10:15 PST
file_path <- here("analysis/data/raw/variables_csv/API_SP.POP.TOTL_DS2_en_csv_v2_4218816.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, d.'2020' as Population
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### People_Per_Sq_Km_of_Land ####
#Table from https://data.worldbank.org/indicator/EN.POP.DNST. Downloaded 6/25/2022,10:16 PST
file_path <- here("analysis/data/raw/variables_csv/API_EN.POP.DNST_DS2_en_csv_v2_4151312.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as	People_Per_Sq_Km_of_Land
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Adult_Literacy_Rate ####
#Table from https://data.worldbank.org/indicator/SE.ADT.LITR.ZS. Downloaded 6/25/2022,10:23 PST
file_path <- here("analysis/data/raw/variables_csv/API_SE.ADT.LITR.ZS_DS2_en_csv_v2_4150679.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select  A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Adult_Literacy_Rate
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Percent_Ppl_Poor_Air_Quality ####
#Table from https://data.worldbank.org/indicator/EN.ATM.PM25.MC.ZS. Downloaded 6/25/2022,10:26 PST
file_path <- here("analysis/data/raw/variables_csv/API_EN.ATM.PM25.MC.ZS_DS2_en_csv_v2_4157060.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Percent_Ppl_Poor_Air_Quality
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### GDP_Per_Capita ####
#Table from https://data.worldbank.org/indicator/NY.GDP.PCAP.CD. Downloaded 6/25/2022,10:26 PST
file_path <- here("analysis/data/raw/variables_csv/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4150786.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as GDP_Per_Capita
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Health_Expenditure_Per_Capita ####
#Table from https://data.worldbank.org/indicator/SH.XPD.CHEX.PC.CD. Downloaded 6/25/2022,10:32 PST
file_path <- here("analysis/data/raw/variables_csv/API_SH.XPD.CHEX.PC.CD_DS2_en_csv_v2_4150816.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Health_Expenditure_Per_Capita
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Percent_Health_Expenditure_Private ####
#Table from https://data.worldbank.org/indicator/SH.XPD.PVTD.CH.ZS. Downloaded 6/25/2022,10:35 PST
file_path <- here("analysis/data/raw/variables_csv/API_SH.XPD.PVTD.CH.ZS_DS2_en_csv_v2_4153798.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select  A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Percent_Health_Expenditure_Private
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Total_Cases_Per_1000_People ####
#Table from https://ourworldindata.org/covid-cases. Downloaded 6/25/2022,3:35 PST
file_path <- here("analysis/data/raw/variables_csv/owid-covid-data.csv")
file_name <- tools::file_path_sans_ext(file_path)
query<-paste0("select A.Entity, d.total_cases_per_million / 1000 as Total_Cases_Per_1000_People
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.location
and d.date='2021-10-31'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Age_65_Older_Percent ####
file_path <- here("analysis/data/raw/variables_csv/API_SP.POP.65UP.TO.ZS_DS2_en_csv_v2_4154737.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
#Table from https://data.worldbank.org/indicator/SP.POP.65UP.TO.ZS. Downloaded 6/25/2022,11:55 PST
query<-paste0("select A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Age_65_Older_Percent
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Ages_15_To_64_Percent ####
#Table from https://data.worldbank.org/indicator/SP.POP.1564.TO.ZS Downloaded 6/26/2022,12:03 PST
file_path <- here("analysis/data/raw/variables_csv/API_SP.POP.1564.TO.ZS_DS2_en_csv_v2_4150831.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select  A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Ages_15_To_64_Percent
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### Ages_0_To_14_Percent ####
#Table from https://data.worldbank.org/indicator/SP.POP.1564.TO.ZS Downloaded 6/26/2022,12:03 PST
file_path <- here("analysis/data/raw/variables_csv/API_SP.POP.0014.TO.ZS_DS2_en_csv_v2_4153151.csv")
file_name <- tools::file_path_sans_ext(file_path)
worldBankPreprocessing(file_path, dataBase=dataBase)
query<-paste0("select  A.Entity, COALESCE(d.'2020', d.'2019',d.'2018',d.'2017',d.'2016',d.'2015',d.'2014',d.'2013',d.'2012',d.'2011',d.'2010') as Ages_0_To_14_Percent
from AllCountries as A left join '", file_name,"' as d on A.Entity=d.'Country Name'
group by A.Entity
order by A.Entity;")
table<-dbGetQuery(dataBase, query)
FeatureDataset<-cbind(FeatureDataset, table)

#### CLEAN UP DUPLICATE COLUMNS ####
#remove all the Entity duplicate columns. Then re-arrange to correct order of first 2 columns
FeatureDataset<-cbind(FeatureDataset[,2],FeatureDataset[ , -which(names(FeatureDataset) == 'Entity')])
totalCol<-ncol(FeatureDataset)
FeatureDataset<-FeatureDataset[,c(2,1,3:totalCol)]
names(FeatureDataset)[2]<-"Entity"

dbDisconnect(dataBase)


#######################################
# Add continent / sub-region variable #
#######################################
continent_region <- read.csv(here::here("analysis/data/raw/continent_region.csv"), sep=";")
continent_region <- continent_region %>% 
                      select(ISO.alpha3.Code, Region.Name, Sub.region.Name)
FeatureDataset<-merge(x = FeatureDataset,y = continent_region, by.x="iso3c",by.y ="ISO.alpha3.Code")


#######################################
#               Save File             #
#######################################
path_output = here::here("analysis/data/preprocessed/")
out_file = paste0(path_output,"FeatureDataset.csv")
write.csv(FeatureDataset, out_file, row.names=FALSE)
print(paste0("File saved to: ",out_file))

print("****************** 3_get_features.R completed ******************")

