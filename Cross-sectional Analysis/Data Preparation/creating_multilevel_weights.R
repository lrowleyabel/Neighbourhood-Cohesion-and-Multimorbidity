#################################
#                               #
#  CREATING MULTILEVEL WEIGHTS  #
#                               #
#################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 11/04/23

# DESCRIPTION: This file extracts separate weights for the PSU and individual based on the final weights provided by Understanding Society

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readstata13)
library(foreign)

# Clear environment
rm(list = ls())

# Set path to repository root directory (change this to wherever you have saved the repository)
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Cross-sectional Analysis/Data Preparation"))

# Load in the analysis dataset
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Cross_Sectional_Analysis_Dataset.Rda")

# Individual-level weights will be calculated as the W10 final individual weight / W1 design weight

# Read in the a_indall file containing the W1 design weight
a_indall<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/a_indall.dta",
                      convert.factors = T,
                      generate.factors = T,
                      nonint.factors = T)

# Select person ID and W1 design weight
a_indall<- a_indall%>%
  select(pidp, a_psnenus_xd)

# Join the W1 design weight to the main df
main_df<- left_join(main_df, a_indall, by = "pidp")

# Calculate the individual-level weight
main_df<- main_df%>%
  mutate(individual_level_weight = j_indscus_lw/a_psnenus_xd)

# The PSU-level weight will be calculated as the smallest W1 design weight within each PSU

# Get the smallest W1 design weight for each PSU
small_psu_design_weights<- main_df%>%
  group_by(j_psu)%>%
  slice_min(a_psnenus_xd, n = 1, with_ties = F)%>%
  mutate(psu_weight = a_psnenus_xd)%>%
  select(j_psu, psu_weight)

# Join the smallest W1 design weight for each PSU to the main df
main_df<- left_join(main_df, small_psu_design_weights, by = "j_psu")

# Assign a value of 1 as the LSOA-level weight
main_df<- main_df%>%
  mutate(census_area_level_weight = 1)

# Save as Rda and dta file
save(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Cross_Sectional_Analysis_Dataset_With_Multilevel_Weights.Rda")
write.dta(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Cross_Sectional_Analysis_Dataset_With_Multilevel_Weights.dta")

