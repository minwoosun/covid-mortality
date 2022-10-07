#!/usr/bin/env Rscript

############################################################################################
# ::DESCRIPTION::
#     - Runs all preprocessing scripts
#
# @ OUTPUT: [csv] XY_{region_filter}.csv: dataframe with (X, X.groupings, Y)
############################################################################################
here::i_am("analysis/code/data_preprocessing/main.R")

library(here)

source(here("analysis/code/data_preprocessing/R/1_get_target.R"))
source(here("analysis/code/data_preprocessing/R/2_get_features_trust.R"))
source(here("analysis/code/data_preprocessing/R/3_get_features.R"))
source(here("analysis/code/data_preprocessing/R/4_process_XY.R"))

print(sessionInfo())

