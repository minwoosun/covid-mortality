# Do Public Health Efforts Matter? Explaining Cross-Country Heterogeneity in Excess Death During the COVID-19 Pandemic

Paper: https://www.nature.com/articles/s41598-023-43407-0

The code used to produce all the results and figures in the paper can be found in `analysis/code/` and the data can be found in `analysis/data/`. 

To reproduce the main results in the paper, please run the following:
1. Unzip `analysis/data/raw/variable_csv.zip` and unzip `analysis/data/raw/WHO_excess_death.zip`
2. If you want to run from scratch including the data pre-processing step, then run `analysis/code/data_preprocessing/main.R` which will populate the preprocessed folder. This is not necessary as the preprocessed dataset used in the paper are already in `analysis/data/preprocessed/`.
3. Gradient Boosting and Random Forest models: `analysis/code/analysis_word_GBMandRF.Rmd`. 
4. Linear models: `analysis/code/analysis_word_linearModels.Rmd`. 
5. For the boostrap confidence intervals of delta run `analysis/code/bootstrap_delta.R`.
6. For the bootstrap hypothesis test run `analysis/code/hypothesis_test.R`.
