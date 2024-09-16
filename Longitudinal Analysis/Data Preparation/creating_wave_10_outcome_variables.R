###############################################
#                                             #
#  CREATING WAVE 10 MULTIMORBIDITY VARIABLES  #
#                                             #
###############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This file creates the outcome variable in the Wave 10 dataset for the longitudinal analysis. This is an indicator of whether they are multimorbid based
# on conditions that have also been tracked at Wave 1. The variable is only calculated for continuing respondents.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Clear environment
rm(list = ls())

# Set path to repository root directory
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Longitudinal Analysis/Data Preparation"))

# Load in Wave 10 data
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_10.Rda")


#### Step 1: Calculate count of conditions ####


# Filter for continuing respondents and select variables relating to health conditions (ie: containing 'hcond')
cont_df<- df_w10%>%
  filter(j_ff_ivlolw == "individual interview (incl tel)" | j_ff_everint == "interviewed previously")%>%
  select(pidp, contains("j_hcond"))

# For continuing respondents, the variables relating to health conditions are as below (where x is the condition number, and z is the index in a list of 1 to n, where n is the total number of conditions a respondent has)

# +-----------------+-----------------------------------------------------------------------------------------------------------------------------------------------------+------------------------------------------+---------------------------------------------------+
# |  Variable name  |                                                                     Description                                                                     |            Substantive values            |                  Missing Values                   |
# +-----------------+-----------------------------------------------------------------------------------------------------------------------------------------------------+------------------------------------------+---------------------------------------------------+
# | j_hcondever[x]  | Has respondent ever been diagnosed with condition x (from general conditions list)?                                                                 | 1 = Yes mentioned, 0 = Not mentioned     | inapplicable, proxy, refusal, don't know          |
# | j_hcondncode[x] | Has respondent ever been diagnosed with condition x (from detailed conditions list)?                                                                | 1 = Yes mentioned, 0 = Not mentioned     | inapplicable, proxy                               |
# | j_hcondno[z]    | What is the condition number for condition z? Where z is the index in a list of 1 to n, where n is the total number of conditions a respondent has. | Condition numbers (value labels missing) | inapplicable, proxy                               |
# | j_hcondna[z]    | What age was the respondent diagnosed with the condition that is recorded in j_hcondno[z]?                                                          | Numeric                                  | missing, inapplicable, proxy, refusal, don't know |
# | j_hcondns[z]    | Does the respondent still have the condition that is recorded in j_hcondno[z]?                                                                      | 1 = Yes, 2 = No                          | missing, inapplicable, proxy, don't know          |
# +-----------------+-----------------------------------------------------------------------------------------------------------------------------------------------------+------------------------------------------+---------------------------------------------------+

# Create long dataframe of condition-respondent combinations indicating whether they have been diagnosed with the condition
cont_df_long<- cont_df%>%
  pivot_longer(cols = matches("hcondncode[0-9][0-9]?"), names_to = "condition", values_to = "mentioned")%>%
  select(pidp, condition, mentioned)%>%
  mutate(condition_number = str_extract(condition, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))

# Create long dataframe of condition position-respondent combinations indicating what condition is mentioned
cont_df_long_positions<- cont_df%>%
  pivot_longer(cols = matches("hcondno[0-9][0-9]?"), names_to = "condition_position", values_to = "mentioned_condition")%>%
  select(pidp, condition_position, mentioned_condition)%>%
  mutate(condition_position_number = str_extract(condition_position, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))

# Create long dataframe of condition position-respondent combinations indicating whether a condition is still present
cont_df_long_positions_still<- cont_df%>%
  pivot_longer(cols = matches("hcondns[0-9][0-9]?"), names_to = "condition_position", values_to = "still_present")%>%
  select(pidp, condition_position, still_present)%>%
  mutate(condition_position_number = str_extract(condition_position, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))

# Create long dataframe of condition position-respondent combinations indicating at what age they were diagnosed with the condition
cont_df_long_positions_age<- cont_df%>%
  pivot_longer(cols = matches("hcondna[0-9][0-9]?"), names_to = "condition_position", values_to = "age_diagnosed")%>%
  select(pidp, condition_position, age_diagnosed)%>%
  mutate(condition_position_number = str_extract(condition_position, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))



# Merge the positions, positions_still and positions_age dataframes based on the position
cont_df_long_positions<- left_join(cont_df_long_positions, cont_df_long_positions_still, by = c("pidp" = "pidp", "condition_position_number" = "condition_position_number"))%>%
  left_join(cont_df_long_positions_age, by = c("pidp" = "pidp", "condition_position_number" = "condition_position_number"))

# Filter the positions dataframe to only those where a condition has been mentioned
missing_vals_cont_conditions_mentioned<- levels(cont_df_long_positions$mentioned_condition)[1:5]

cont_df_long_positions<- cont_df_long_positions%>%
  filter(!mentioned_condition %in% missing_vals_cont_conditions_mentioned)

# Merge the dataframe with the mentioned conditions, the indicator of whether still present and the age-diagnosed
cont_df_long_positions<- cont_df_long_positions%>%
  mutate(mentioned_condition = str_pad(mentioned_condition, width = 2, side = "left", pad = "0"))%>%
  select(pidp, mentioned_condition, still_present, age_diagnosed)

cont_df_long<- left_join(cont_df_long, cont_df_long_positions, by = c("pidp" = "pidp", "condition_number" = "mentioned_condition"))

# Add variable to long dataframe coded:
# 1 = they have been diagnosed with the condition and it is still present, 
# 0 = they have not had the condition or they had the condition and it is no longer present
# NA = missing data

missing_vals_mentioned_var<- levels(cont_df_long$mentioned)[1:5]
missing_vals_still_var<- levels(cont_df_long$still_present)[1:5]
missing_vals_age_var<- levels(cont_df_long$age_diagnosed)[1:5]

cont_df_long<- cont_df_long%>%
  mutate(present = case_when(mentioned == "Yes mentioned" & still_present == "Yes" & (!age_diagnosed %in% missing_vals_age_var) ~ 1,
                             mentioned == "Yes mentioned" & condition_number %in% c("25", "32") ~ 1,
                             mentioned == "Yes mentioned" & condition_number %in% c("06", "07", "19")  & (!age_diagnosed %in% missing_vals_age_var) ~ 1,
                             mentioned == "Not mentioned" ~ 0,
                             mentioned == "Yes mentioned" & still_present == "No" ~ 0,
                             mentioned == "Yes mentioned" & (still_present %in% missing_vals_still_var | is.na(still_present)) ~ NA_real_,
                             mentioned == "Yes mentioned"  & (age_diagnosed %in% missing_vals_age_var | is.na(age_diagnosed)) ~ NA_real_,
                             mentioned %in% missing_vals_mentioned_var ~ NA_real_))%>% 
  select(pidp, condition_number, present, age_diagnosed)

# Exclude the following conditions, either because they are not counting towards our definition of MM, or because they were not asked about at Wave 1
# - none of these
# - gestational diabetes
# - other long-standing/chronic condition
# - Multiple sclerosis
# - HIV
# - COPD
# - Anxiety
# - Depression
# - Psychosis or schizophrenia
# - Bipolar disorder or manic depression
# - An eating disorder
# - Post-traumatic stress disorder
# - Other emotional, nervous or  psychiatric problem

cont_df_long<- cont_df_long%>%
  filter(!condition_number %in% c("96", "35", "97", "19", "20", "21", "37", "38", "39", "40", "41", "42", "43"))

# The conditions "Other arthritis" and "Other cancer" do not have variables indicating at what age they were diagnosed or whether they still have the condition, so exclude them too
#cont_df_long<- cont_df_long%>%
#  filter(!condition_number %in% c("25", "32"))

# Store respondents with missing data on any condition in separate dataframe and filter them out from the main data. 
cont_df_long_missing<- cont_df_long%>%
  group_by(pidp)%>%
  filter(any(is.na(present)))%>%
  ungroup()

cont_df_long<- cont_df_long%>%
  group_by(pidp)%>%
  filter(!any(is.na(present)))%>%
  ungroup()


# Some conditions recorded in the Understanding Society data should not be considered as separate conditions for
# the sake of measuring multimorbidity as having "2 or more long-term conditions". We therefore need to group some conditions
# together before counting the number of conditions each respondent has.

# Read in lookup table containing information on conditions groupings, physical/mental classification and ICD-10 chapter
conditions<- read.csv("../../Lookups/wave_10_condition_codes.csv")

# Create a lookup between condition numbers, condition names and the condition grouping
conditions_to_grouped_conditions<- conditions%>%
  select(Variable, Label, Grouped_Label, Type, ICD_10_Chapter, Inclusion)%>%
  mutate(condition_code = str_extract(Variable, "(?<=hcondcode)[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"),
         condition_name = Label,
         condition_grouped_name = Grouped_Label)%>%
  select(condition_code, condition_name, condition_grouped_name, ICD_10_Chapter)%>%
  unique()

# Link condition numbers to grouped condition labels and ICD-10 chapters (for both respondents without and with missing data)
cont_df_long<- left_join(cont_df_long, conditions_to_grouped_conditions, by = c("condition_number" = "condition_code"))

cont_df_long_missing<- left_join(cont_df_long_missing, conditions_to_grouped_conditions, by = c("condition_number" = "condition_code"))


# Create dataframe with count of conditions (for both respondents without and with missing data)
cont_df_conditions_count<- cont_df_long%>%
  group_by(pidp, condition_grouped_name)%>%
  summarise(grouped_present = ifelse(sum(present)>0,1,0))%>%
  ungroup()%>%
  group_by(pidp)%>%
  summarise(condition_count_w10 = sum(grouped_present, na.rm = T))%>%
  ungroup()

cont_df_conditions_count_missing<- cont_df_long_missing%>%
  group_by(pidp, condition_grouped_name)%>%
  summarise(grouped_present = ifelse(sum(present)>0,1,0))%>%
  ungroup()%>%
  group_by(pidp)%>%
  summarise(condition_count_w10 = sum(grouped_present, na.rm = T))%>%
  ungroup()


# Create a dataframe with multimorbidity indicator for respondents (for both respondents without and with missing data)
cont_df_mm<- cont_df_conditions_count%>%
  mutate(mm = case_when(condition_count_w10 >= 2 ~ 1,
                        condition_count_w10 < 2 ~ 0,
                        T ~ NA_real_))%>%
  select(-condition_count_w10)

cont_df_mm_missing<- cont_df_conditions_count_missing%>%
  mutate(mm = case_when(condition_count_w10 >= 2 ~ 1,
                        condition_count_w10 < 2 ~ 0,
                        T ~ NA_real_))%>%
  select(-condition_count_w10)


# For those who have missing data, filter to those who are nonetheless classed as multimorbid and join to the dataframe of those with no missing data
cont_df_mm<- cont_df_mm%>%
  rbind(cont_df_mm_missing%>%
          filter(mm == 1))


# Join the three outcome dataframes to the main Wave 10 dataframe
df_w10<- df_w10%>%
  left_join(cont_df_mm, by = "pidp")


# Save the data
save(df_w10, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Longitudinal_Wave_10_With_Outcome_Variable.Rda")
