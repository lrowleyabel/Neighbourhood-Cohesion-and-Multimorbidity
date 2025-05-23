################################
#                              #
#  CREATING OUTCOME VARIABLES  #
#                              #
################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This file creates variables for Wave 10 indicating whether individuals had (at the time of data collection) each of the
# long-term conditions being tracked in Understanding Society. It then produces a count of the number of long-term conditions that
# each individual had, and also creates binary indicators of whether each individual was multimorbid according to different criteria.
# The variables relating to conditions are different for new and continuing participants in Wave 10, so the indicators are created
# separately for each group.

library(dplyr)
library(tidyr)
library(stringr)
library(readstata13)

# Clear environment
rm(list = ls())

# Set path to repository root directory (change this to wherever you have saved the repository)
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Cross-sectional Analysis/Data Preparation"))

# Read in Wave 10 data
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_10.Rda")


#### Step 1: Create health condition indicators for new respondents ####


# Filter for new respondents and only select variables relating to health conditions (ie: containing 'hcond' in the variable name)
new_df<- df_w10%>%
  filter(j_ff_ivlolw != "individual interview (incl tel)" & j_ff_everint != "interviewed previously")%>%
  select(pidp, contains("hcond"))

# For new respondents, the variables relating to health conditions are as below (where [x] represents the condition numbers):

# +----------------+------------------------------------------------------------------------------------------------------+--------------------------------------+------------------------------------------+
# | Variable name  |                                             Description                                              |          Substantive values          |              Missing values              |
# +----------------+------------------------------------------------------------------------------------------------------+--------------------------------------+------------------------------------------+
# | j_hcond[x]     | Has respondent ever been diagnosed with condition x (from general conditions list)?                  | 1 = Mentioned, 2 = Not mentioned     | inapplicable, proxy, refusal, don't know |
# | j_hcondcode[x] | Has respondent ever been diagnosed with condition x (from detailed conditions list)?                 | 1 = Yes mentioned, 2 = Not mentioned | inapplicable, proxy                      |
# | j_hconda[x]    | For any conditions with j_hcondcode[x] == 1, what age was the respondent diagnosed with condition x? | Numeric                              | inapplicable, proxy, don't know          |
# | j_hconds[x]    | For any conditions with j_hcondcode[x] == 1, does the respondent still have the condition?           | 1 = Yes, 2 = No                      | inapplicable, proxy, don't know          |
# +----------------+------------------------------------------------------------------------------------------------------+--------------------------------------+------------------------------------------+

# Note that presence of "other long-standing/chronic condition" is recorded in variable j_hcond18 and then in j_hcondcode97 (ie: the condition number changes from 18 to 97 for the
# general and specific condition lists respectively). j_hconds97 and j_hconda97 are the relevant variables for whether they still have the condition and at what age they were diagnosed.

# Note that condition 20 (HIV) does not have variables in the normal license version of the data due to confidentiality.
# This is therefore excluded from the conditions. 1 respondent has j_hcond20 == 1 qccording to the online variable search.

# Note that there is no j_hconds[x] for the following conditions, as it does not make sense to
# talk about 'still' having the condition:
# 06 (heart attack)
# 07 (stroke)
# 19 (multiple scelrosis)

# To ascertain what conditions each respondent currently has, for each condition we need to check if j_hcondcode[x] == 1 AND j_hconds[x] == 1

# Create long dataframe of condition-respondent combinations with indicator of whether they've been diagnosed with the condition
new_df_long<- new_df%>%
  pivot_longer(cols = matches("j_hcondcode[0-9][0-9]?"), names_to = "condition", values_to = "mentioned")%>%
  select(pidp, condition, mentioned)%>%
  mutate(condition_number = str_extract(condition, "[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"))

# Create long dataframe of condition-respondent combinations with indicator of whether condition is still present
new_df_long_still<- new_df%>%
  pivot_longer(cols = matches("j_hconds[0-9][0-9]?"), names_to = "condition", values_to = "still_present")%>%
  select(pidp, condition, still_present)%>%
  mutate(condition_number = str_extract(condition, "[0-9][0-9]?"))

# Merge the two long dataframes by both respondent and condition
new_df_long<- left_join(new_df_long, new_df_long_still, by = c("pidp" = "pidp", "condition_number" = "condition_number"))

# Add variable to long dataframe coded:
# 1 = they have been diagnosed with the condition and it is still present, 
# 0 = they have not had the condition or they had the condition and it is no longer present
# NA = missing data

missing_vals_mentioned_var<- levels(new_df_long$mentioned)[1:5]
missing_vals_still_var<- levels(new_df_long$still_present)[1:5]

new_df_long<- new_df_long%>%
  mutate(present = case_when(mentioned == "Yes mentioned" & still_present == "Yes" ~ 1,
                             mentioned == "Yes mentioned" & condition_number %in% c("06", "07", "19") ~ 1,
                             mentioned == "Not mentioned" ~ 0,
                             mentioned == "Yes mentioned" & still_present == "No" ~ 0,
                             mentioned == "Yes mentioned" & still_present %in% missing_vals_still_var ~ NA_real_,
                             mentioned %in% missing_vals_mentioned_var ~ NA_real_))%>%
  select(pidp, condition_number, present)

# Exclude the conditions "none of these" and "gestational diabetes"
new_df_long<- new_df_long%>%
  filter(!condition_number %in% c("96", "35"))

# Exclude the conditions "other arthritis", "other cancer", "other diabetes", "other emotional, nervous or psychiatric problem", and "other long-term/chronic condition" as we do not have data on whether they still have the condition for continuing respondents
new_df_long<- new_df_long%>%
  filter(!condition_number %in% c("25", "32", "36", "43", "97"))

# Filter out any respondents for whom there is missing data on any condition
new_df_long<- new_df_long%>%
  group_by(pidp)%>%
  filter(!any(is.na(present)))


#### Step 2: Creating Multimorbidity Indicators for New Respondents #### 


# Some conditions recorded in the Understanding Society data should not be considered as separate conditions for
# the sake of measuring multimorbidity as having "2 or more long-term conditions". We therefore need to group some conditions
# together before counting the number of conditions each respondent has.

# Read in lookup table containing information on conditions groupings, physical/mental classification and ICD-10 chapter
conditions<- read.csv("../Lookups/wave_10_condition_codes.csv")

# Create a lookup between condition numbers, condition names and the condition grouping
conditions_to_grouped_conditions<- conditions%>%
  select(Variable, Label, Grouped_Label, Type, ICD_10_Chapter, Inclusion)%>%
  mutate(condition_code = str_extract(Variable, "(?<=hcondcode)[0-9][0-9]?")%>%
           str_pad(width = 2, side = "left", pad = "0"),
         condition_name = Label,
         condition_grouped_name = Grouped_Label)%>%
  select(condition_code, condition_name, condition_grouped_name)%>%
  unique()

# Create a lookup between condition group and ICD-10 chapter
grouped_conditions<- conditions%>%
  select(Variable, Label, Grouped_Label, Type, ICD_10_Chapter, Inclusion)%>%
  mutate(condition_grouped_name = Grouped_Label,
         condition_type = Type,
         condition_icd_10 = ICD_10_Chapter)%>%
  select(condition_grouped_name, condition_type, condition_icd_10)%>%
  unique()


# Join condition groupings to data recording whether each condition is present for each respondent
new_df_long<- left_join(new_df_long, conditions_to_grouped_conditions, by = c("condition_number" = "condition_code"))

# Create a dataframe recording whether each respondent has each grouped condition
new_df_long<- new_df_long%>%
  group_by(pidp, condition_grouped_name)%>%
  summarise(grouped_present = ifelse(sum(present)>0,1,0))%>%
  ungroup()

# Join grouped conditions classifications to the dataframe
new_df_long<- left_join(new_df_long, grouped_conditions, by = "condition_grouped_name")


# Count number of grouped conditions each respondent has
new_condition_count<- new_df_long%>%
  group_by(pidp)%>%
  summarise(condition_count = sum(grouped_present))

# Count conditions by physical / mental
new_physical_count<- new_df_long%>%
  filter(condition_type == "Physical")%>%
  group_by(pidp)%>%
  summarise(physical_count = sum(grouped_present))

new_mental_count<- new_df_long%>%
  filter(condition_type == "Mental")%>%
  group_by(pidp)%>%
  summarise(mental_count = sum(grouped_present))

# Count conditions by ICD-10 chapter
new_icd_count<- new_df_long%>%
  group_by(pidp, condition_icd_10)%>%
  summarise(condition_count = sum(grouped_present))%>%
  ungroup()%>%
  mutate(icd_present = ifelse(condition_count>0,1,0))%>%
  group_by(pidp)%>%
  summarise(distinct_icds = sum(icd_present))

# Join together all the different type of counts
new_counts<- left_join(new_physical_count, new_mental_count, by = "pidp")%>%
  left_join(new_condition_count, by = "pidp")%>%
  left_join(new_icd_count, by = "pidp")

# Create indicators of multimorbidity using different cut-offs and criteria, as below:
# mm = two or more of any condition
# p_mm = two or more physical conditions
# m_mm = two or more mental conditions
# pm_mm = two or more conditions including at least one physical and one mental
# bs2_mm = two or more conditions from at least two distinct body systems
# mm3 = three or more conditions
# p_mm3 = three or more physical conditions
# m_mm3 = three or more mental conditions
# pm_mm3 = three or more conditions including at least one physical and one mental
# bs3_mm = three or more conditions from three or more body systems

new_counts<- new_counts%>%
  mutate(mm = ifelse(condition_count>=2,1,0),
         p_mm = ifelse(physical_count>=2,1,0),
         m_mm = ifelse(mental_count>=2,1,0),
         pm_mm = ifelse(physical_count>=1 & mental_count>=1,1,0),
         bs2_mm = ifelse(distinct_icds>=2,1,0),
         mm3 = ifelse(condition_count>=3,1,0),
         p_mm3 = ifelse(physical_count>=3,1,0),
         m_mm3 = ifelse(mental_count>=3,1,0),
         pm_mm3 = ifelse(physical_count>=1 & mental_count>=1 & (physical_count + mental_count)>=3,1,0),
         bs3_mm = ifelse(distinct_icds>=3,1,0))


# Create wide format dataframe with indicators of whether respondent has each of the grouped conditions
new_individual_conditions<- new_df_long%>%
  pivot_wider(id_cols = pidp, names_from = condition_grouped_name, values_from = grouped_present)

colnames(new_individual_conditions)<- colnames(new_individual_conditions)%>%
  str_replace_all("[:punct:]", "_")%>%
  str_replace_all(" ", "_")%>%
  str_to_lower()


#### Step 3: Create Condition Indicators for Continuing Respondents ####


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

# Merge the positions and positions_still dataframes based on the position
cont_df_long_positions<- left_join(cont_df_long_positions, cont_df_long_positions_still, by = c("pidp" = "pidp", "condition_position_number" = "condition_position_number"))

# Filter the positions dataframe to only those where a condition has been mentioned
missing_vals_cont_conditions_mentioned<- levels(cont_df_long_positions$mentioned_condition)[1:5]

cont_df_long_positions<- cont_df_long_positions%>%
  filter(!mentioned_condition %in% missing_vals_cont_conditions_mentioned)

# Merge the dataframe with the mentioned conditions and the indicator of whether still present
cont_df_long_positions<- cont_df_long_positions%>%
  mutate(mentioned_condition = str_pad(mentioned_condition, width = 2, side = "left", pad = "0"))%>%
  select(pidp, mentioned_condition, still_present)

cont_df_long<- left_join(cont_df_long, cont_df_long_positions, by = c("pidp" = "pidp", "condition_number" = "mentioned_condition"))

# Add variable to long dataframe coded:
# 1 = they have been diagnosed with the condition and it is still present, 
# 0 = they have not had the condition or they had the condition and it is no longer present
# NA = missing data

missing_vals_mentioned_var<- levels(cont_df_long$mentioned)[1:5]
missing_vals_still_var<- levels(cont_df_long$still_present)[1:5]

cont_df_long<- cont_df_long%>%
  mutate(present = case_when(mentioned == "Yes mentioned" & still_present == "Yes" ~ 1,
                             mentioned == "Yes mentioned" & condition_number %in% c("06", "07", "19") ~ 1,
                             mentioned == "Not mentioned" ~ 0,
                             mentioned == "Yes mentioned" & still_present == "No" ~ 0,
                             mentioned == "Yes mentioned" & (still_present %in% missing_vals_still_var | is.na(still_present)) ~ NA_real_,
                             mentioned %in% missing_vals_mentioned_var ~ NA_real_))%>%
  select(pidp, condition_number, present)

# Exclude the conditions "none of these" and "gestational diabetes"
cont_df_long<- cont_df_long%>%
  filter(!condition_number %in% c("96", "35"))


# Exclude the conditions "other arthritis", "other cancer", "other diabetes", "other emotional, nervous or psychiatric problem", and "other long-term/chronic condition" as we do not have data on whether they still have the condition for continuing respondents
cont_df_long<- cont_df_long%>%
  filter(!condition_number %in% c("25", "32", "36", "43", "97"))

# Filter out any respondents for whom there is missing data on any condition
cont_df_long<- cont_df_long%>%
  group_by(pidp)%>%
  filter(!any(is.na(present)))

# As with the new respondents, Link condition numbers to grouped condition labels, physical/mental classification and ICD-10 chapter
cont_df_long<- left_join(cont_df_long, conditions_to_grouped_conditions, by = c("condition_number" = "condition_code"))

# Create a variable recording whether each respondent has each grouped condition or not
cont_df_long<- cont_df_long%>%
  group_by(pidp, condition_grouped_name)%>%
  summarise(grouped_present = ifelse(sum(present)>0,1,0))%>%
  ungroup()

# Join the grouped condition classifications and ICD-10 chapters
cont_df_long<- left_join(cont_df_long, grouped_conditions, by = "condition_grouped_name")



#### Step 4: Creating Multimorbidity Indicators for Continuing Respondents ####


# Count number of grouped conditions for each continuing respondent
cont_condition_count<- cont_df_long%>%
  group_by(pidp)%>%
  summarise(condition_count = sum(grouped_present))

# Count conditions by physical / mental
cont_physical_count<- cont_df_long%>%
  filter(condition_type == "Physical")%>%
  group_by(pidp)%>%
  summarise(physical_count = sum(grouped_present))

cont_mental_count<- cont_df_long%>%
  filter(condition_type == "Mental")%>%
  group_by(pidp)%>%
  summarise(mental_count = sum(grouped_present))

# Count conditions by ICD-10 chapter
cont_icd_count<- cont_df_long%>%
  group_by(pidp, condition_icd_10)%>%
  summarise(condition_count = sum(grouped_present))%>%
  ungroup()%>%
  mutate(icd_present = ifelse(condition_count>0,1,0))%>%
  group_by(pidp)%>%
  summarise(distinct_icds = sum(icd_present))

# Join the different counts together
cont_counts<- left_join(cont_physical_count, cont_mental_count, by = "pidp")%>%
  left_join(cont_condition_count, by = "pidp")%>%
  left_join(cont_icd_count, by = "pidp")


# Create indicators of multimorbidity using different cut-offs and criteria, as below:
# mm = two or more of any condition
# p_mm = two or more physical conditions
# m_mm = two or more mental conditions
# pm_mm = two or more conditions including at least one physical and one mental
# bs2_mm = two or more conditions from at least two distinct body systems
# mm3 = three or more conditions
# p_mm3 = three or more physical conditions
# m_mm3 = three or more mental conditions
# pm_mm3 = three or more conditions including at least one physical and one mental
# bs3_mm = three or more conditions from three or more body systems

cont_counts<- cont_counts%>%
  mutate(mm = ifelse(condition_count>=2,1,0),
         p_mm = ifelse(physical_count>=2,1,0),
         m_mm = ifelse(mental_count>=2,1,0),
         pm_mm = ifelse(physical_count>=1 & mental_count>=1,1,0),
         bs2_mm = ifelse(distinct_icds>=2,1,0),
         mm3 = ifelse(condition_count>=3,1,0),
         p_mm3 = ifelse(physical_count>=3,1,0),
         m_mm3 = ifelse(mental_count>=3,1,0),
         pm_mm3 = ifelse(physical_count>=1 & mental_count>=1 & (physical_count + mental_count)>=3,1,0),
         bs3_mm = ifelse(distinct_icds>=3,1,0))


# Create wide-format dataframe with variables indicating whether each condition is present for each respondent
cont_individual_conditions<- cont_df_long%>%
  pivot_wider(id_cols = pidp, names_from = condition_grouped_name, values_from = grouped_present)

colnames(cont_individual_conditions)<- colnames(cont_individual_conditions)%>%
  str_replace_all("[:punct:]", "_")%>%
  str_replace_all(" ", "_")%>%
  str_to_lower()

## Join calculated variables for new and continuing respondents
mm_vars<- rbind(new_counts, cont_counts)
condition_vars<- rbind(new_individual_conditions, cont_individual_conditions)

## Merge new variables into main dataframe
df_w10<- left_join(df_w10, mm_vars, by = "pidp")
df_w10<- left_join(df_w10, condition_vars, by = "pidp")

## Save new dataframe
save(df_w10, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Cross_Sectional_Wave_10_With_Outcome_Variables.Rda")
