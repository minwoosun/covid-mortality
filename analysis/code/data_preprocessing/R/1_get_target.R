#!/usr/bin/env Rscript

############################################################################################
# ::DESCRIPTION::
#     - downloaded csvs from https://www.who.int/data/sets/global-excess-deaths-associated-with-covid-19-modelled-estimates
#     - downloaded at 7/2/2022 10:50am PST
#     - Script to process and generate excess death response variable.  
#     - Excess death estimates from the World Health Organization
#
# @ INPUT: [csv] export_country_cumulative.csv: cumulative excess death from the Economist
# @ INPUT: [csv] iso.csv: csv of iso3 with country names
# @ OUTPUT: [csv] target_variable.csv 
############################################################################################

here::i_am("analysis//code/data_preprocessing/R/1_get_target.R")
library(here)
library(dplyr)
library(readxl)

path_input = here::here("analysis/data/raw/")
path_output = here::here("analysis/data/preprocessed/")

# load data
df <- read_excel(paste0(path_input, "WHO_excess_death/WHO_COVID_Excess_Deaths_EstimatesByCountry.xlsx"), 
                 sheet = "Country by year and month", 
                 skip = 11)
df <- head(df, 4656) # remove NA rows
iso = read.csv(paste0(path_input, "iso.csv"))

# create time variable to represent year-month combo
time = rep(1:24, 194) 
df$time = time

# select date range of interest
time.from = 1
time.to = 23
date.range = (df$time >= time.from & df$time <= time.to)
df = df[date.range, ]

# groupby country and aggregate expected mean
df.target <-  df %>% 
                dplyr::select(country, iso3, excess.mean) %>% 
                dplyr::group_by(country, iso3) %>% 
                dplyr::summarise(excess_death = sum(excess.mean)) %>% 
                data.frame

# output
out_file = paste0(path_output,"target_WHO.csv")
write.csv(df.target, out_file, row.names = FALSE)
print(paste0("File saved to: ",out_file))

print("****************** 1_get_target.R completed ******************")
