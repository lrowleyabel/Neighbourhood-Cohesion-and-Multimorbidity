#############################################
#                                           #
#  CREATING BASELINE INDEPENDENT VARIABLES  #
#                                           #
#############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This file recodes the independent baseline variables used in the analysis.

library(dplyr)
library(tidyr)
library(stringr)
library(readstata13)

# Clear environment
rm(list = ls())

# Set path to repository root directory
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Longitudinal Analysis/Data Preparation"))

# Load in Wave 1 with the health variables
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1_With_Baseline_Health_Variables.Rda")


#### Step 1: Demographic Variables ####


# Create numeric age variable
main_df<- mutate(df_w1, age = a_age_dv%>%
                   as.character()%>%
                   as.integer())


# Recode sex variable
main_df<- main_df%>%
  mutate(sex = case_when(a_sex == "male" ~ "male",
                         a_sex == "female" ~ "female",
                         T ~ NA_character_)%>%
           factor())


#### Step 2: Neighbourhood Cohesion Variable ####


# Convert neighbourhood cohesion variable to numeric
main_df<- main_df%>%
  mutate(nb_cohesion = case_when(a_nbrsnci_dv == "lowest cohesion" ~ 1,
                                 a_nbrsnci_dv == "highest cohesion" ~ 5,
                                 T ~ as.numeric(as.character(a_nbrsnci_dv))))

# Create cohesion quintile variable
main_df<- main_df%>%
  mutate(nb_cohesion_quint = case_when(!is.na(nb_cohesion) ~ paste0("q", ntile(nb_cohesion, 5)),
                                       T ~ NA_character_)%>%
           factor())

# Save data
save(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1_With_Baseline_Health_And_Independent_Variables.Rda")
