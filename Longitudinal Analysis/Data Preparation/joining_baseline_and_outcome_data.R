#######################################
#                                     #
#  JOINING BASELINE AND OUTCOME DATA  #
#                                     #
#######################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This file joins the health and independent variables measured at baseline (Wave 1) to the outcome variable at Wave 10.

library(dplyr)

# Clear environment
rm(list = ls())

# Set path to repository root directory
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Longitudinal Analysis/Data Preparation"))

# Read in baseline data
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1_With_Baseline_Health_And_Independent_Variables.Rda")

# Read in outcomes data
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Longitudinal_Wave_10_With_Outcome_Variable.Rda")

# Join the baseline variables to the outcomes data
main_df<- left_join(df_w10, main_df, by = "pidp")

# Save the data as the analysis dataset
save(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Longitudinal_Analysis_Dataset.Rda")
write.dta(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Longitudinal_Analysis_Dataset.dta")

