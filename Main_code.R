# This series of scripts reproduces the results and sensitivity analyses of the paper: 
# Neyret et al. (2021). Assessing the effects of landscape management on grassland multifunctionality in Germany. Ecosystem Services.

# This is the main script, which calls all others.

library(lme4)
library(MuMIn)
library(mice)
library(data.table)
library(vegan)
library(rsq)
library(BiodiversityR)
library(reshape2)
library(dplyr)
library(tidyr)
library(sp)
library(readr)
library(RColorBrewer)
library(ade4)
library(geometry)
library(readxl)
library(car)
library(emmeans)
library(Hmisc)
library(ggcorrplot)
library(cowplot)
library(psych)
library(readxl)
library(readr)
library(data.table)
library(multcomp)
library(ggpubr)
library(rstatix)
library(gridExtra)
set.seed(101)

# Rerun data preparation from raw datasets (TRUE/FALSE)
rerun_prep = F

# Save all plots? TRUE/FALSE
saveplots = T

col2 <- colorRampPalette(c('#a50026',
  '#d73027',
  '#f46d43',
  '#fdae61',
  '#fee090',
  '#ffffbf',
  '#e0f3f8',
  '#abd9e9',
  '#74add1',
  '#4575b4',
  '#313695'))

rerun_prep = T

setwd('')
source(file = 'Functions.R')

# This creates the table in which model results (equivalent to figure 6 and Table C2)
write.table('', file = "Sensitivity_analyses/Result_data.csv")

## This loop builds the results for all parameters. 
# Default parameters (as presented in the main text) are shown first in the vectors
# The "if" statements insure that only one parameter deviates from the default to limit the number of iterations.

for (lui_class_method in c('quantile_30' , 'quantile_20')){ # lui_class_method: Is the classification made on 33% quantiles or 20% percentiles?
  for (environmental_correction in c(1, 0)) { # environmental_correction: Is the service data corrected for environmental variables?

    if (lui_class_method == "quantile_30"){options_plots =  c(10, 7,  13)}
    if (lui_class_method == 'quantile_20'){options_plots = 7} # (Note: if using only 20% quantiles then not enough plots to have more than 7 per landscape)

    for (no_plots in options_plots) {  # no_plots: How many plots per landscape?
      if (lui_class_method == "quantile_30" & no_plots == 10){options_method_wt_ls =  c('mean', 'max')} # options_method_wt_ls: Is the landscape-scale service calculated as the mean or max of the plot service values?
      else {options_method_wt_ls = 'mean'}
      
      for (method_within_landscape in options_method_wt_ls) {
        if (lui_class_method == "quantile_30" & no_plots == 10 & method_within_landscape == 'mean'){
                   options_method =  c('Threshold', 'Average', 'minimise_trade_offs')}
        else {options_method = 'Threshold'} 
        
        for (method in options_method){ # method: Is multifunctionality calculated based on thresholds, average values, or compromise (see paper for details)?

          if (method == 'Threshold'){
           if (method_within_landscape == 'mean' & no_plots == 10 & lui_class_method == "quantile_30"){
             options_threshold =  c( 0.5, 0.4,  0.6) } #
           else {options_threshold = 0.5}}
          if (method == 'minimise_trade_offs'){ 
           if (method_within_landscape == 'mean' & no_plots == 10 &  lui_class_method == "quantile_30"){
            options_threshold =  c(0.15, 
              0.25, 0.35 ) } else {
              options_threshold = 0.25}} 
          if (method == 'Average'){
            options_threshold = NA} 
          for (threshold_to_use in options_threshold) { # threshold_to_use: Threshold used in the threshold and compromise methods

            
#    Sources each step of the analysis. The 3 first steps cannot be reproduced without access to raw data.
#    The simulated datasets are provided and can be used for the last steps.
            
#source(file = 'Data_preparation.R')
#source(file = 'Plot_scale.R')
#source(file = 'Create_landscape_sim.R')
source(file = 'Calculate_MF.R')
source(file = 'All_plots.R')

   }}}}}}

