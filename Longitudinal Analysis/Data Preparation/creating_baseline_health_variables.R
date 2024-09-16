########################################
#                                      #
#  CREATING BASELINE HEALTH VARIABLES  #
#                                      #
########################################

# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This file creates variables counting the number of conditions respondents reported at baselines (Wave 1) and whether they are classed as multimorbid.

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

# Load in Wave 1 data
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1.Rda")

# Create long-format dataframe of respondent-condition combinations indicating whether they had been diagnosed with the condition and whether they still had it at Wave 1
df_long<- df_w1%>%
  pivot_longer(cols = a_hcond1:a_hcond17, names_to = "condition", values_to = "mentioned")%>%
  select(pidp, condition, mentioned)%>%
  mutate(condition_number = str_extract(condition, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))

df_long_still<- df_w1%>%
  pivot_longer(cols = a_hconds01:a_hconds17, names_to = "condition", values_to = "still_present")%>%
  select(pidp, condition, still_present)%>%
  mutate(condition_number = str_extract(condition, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))

df_long<- left_join(df_long, df_long_still, by = c("pidp", "condition_number"))

# Add variable to long dataframe coded:
# 1 = they have been diagnosed with the condition and it is still present, 
# 0 = they have not had the condition or they had the condition and it is no longer present
# NA = missing data

missing_vals_mentioned_var<- levels(df_long$mentioned)[1:5]
missing_vals_still_var<- levels(df_long$still_present)[1:5]

df_long<- df_long%>%
  mutate(mentioned = str_trim(mentioned),
         still_present = str_trim(still_present))%>%
  mutate(present = case_when(mentioned == "Mentioned" & still_present == "yes" ~ 1,
                             mentioned == "Mentioned" & condition_number %in% c("06", "07") ~ 1,
                             mentioned == "not mentioned" ~ 0,
                             mentioned == "Mentioned" & still_present == "no" ~ 0,
                             mentioned == "Mentioned" & still_present %in% missing_vals_still_var ~ NA_real_,
                             mentioned %in% missing_vals_mentioned_var ~ NA_real_))%>%
  select(pidp, condition_number, present)

# Exclude hyperthyroidism and clinical depression as they were not asked about at Wave 10, so are not included as a condition at Wave 1 for this analysis
df_long<- df_long%>%
  filter(!condition_number %in% c("09", "17"))

# Filter out respondents who have missing data for any condition
df_long<- df_long%>%
  group_by(pidp)%>%
  filter(!any(is.na(present)))

# Some conditions recorded in the Understanding Society data should not be considered as separate conditions for
# the sake of measuring multimorbidity as having "2 or more long-term conditions". We therefore need to group some conditions
# together before counting the number of conditions each respondent has.

# Read in lookup table containing information on conditions groupings as they were at Wave 10
conditions<- read.csv("../../Lookups/wave_10_condition_codes.csv")

# Create a lookup between condition numbers, condition names and the condition grouping
conditions_to_grouped_conditions<- conditions%>%
  select(Variable, Label, Grouped_Label, Type, ICD_10_Chapter, Inclusion)%>%
  mutate(condition_code = str_extract(Variable, "(?<=hcondcode)[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"),
         condition_name = Label,
         condition_grouped_name = Grouped_Label)%>%
  select(condition_code, condition_name, condition_grouped_name)%>%
  unique()


# Add arthritis, cancer and diabetes to the lookup (since the lookup has the more detailed codes for sub-conditions within these which were not used in Wave 1)
conditions_to_grouped_conditions<- conditions_to_grouped_conditions%>%
  rbind(data.frame(condition_code = c("02", "13", "14"), condition_name = c("Arthritis", "Cancer", "Diabetes"), condition_grouped_name = c("Arthritis", "Cancer", "Diabetes")))


# Join condition groupings to data recording whether each condition is present for each respondent
df_long<- left_join(df_long, conditions_to_grouped_conditions, by = c("condition_number" = "condition_code"))

# Create a dataframe recording whether each respondent has each grouped condition
df_long<- df_long%>%
  ungroup()%>%
  filter(!is.na(condition_grouped_name))%>%
  group_by(pidp, condition_grouped_name)%>%
  summarise(grouped_present = ifelse(sum(present)>0,1,0))%>%
  ungroup()

# Create dataframe of condition counts by respondent
df_condition_counts<- df_long%>%
  group_by(pidp)%>%
  summarise(condition_count_w1 = sum(grouped_present))

# Create a simple indicator of whether mulitmorbid
df_condition_counts<- df_condition_counts%>%
  mutate(mm_w1 = ifelse(condition_count_w1>=2, 1, 0))

# Join counts and mm variable to main Wave 1 data
df_w1<- left_join(df_w1, df_condition_counts, by = "pidp")

# Save data
save(df_w1, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1_With_Baseline_Health_Variables.Rda")
