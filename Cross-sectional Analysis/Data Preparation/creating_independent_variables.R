####################################
#                                  #
#  CREATING INDEPENDENT VARIABLES  #
#                                  #
####################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This file recodes the independent variables used in the analysis.

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

# Load in Wave 10 with the outcome variables
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Cross_Sectional_Wave_10_With_Outcome_Variables.Rda")


#### Step 1: Demographic Variables ####


# Create numeric age variable
main_df<- mutate(df_w10, age = j_age_dv%>%
                   as.character()%>%
                   as.integer())


# Recode sex variable
main_df<- main_df%>%
  mutate(sex = case_when(j_sex == "male" ~ "male",
                         j_sex == "female" ~ "female",
                         T ~ NA_character_)%>%
           factor())



#### Step 2: Socio-economic Variables ####


# Recode education variable

main_df<- main_df%>%
  mutate(edu = case_when(j_hiqual_dv %in% c("Other higher degree", "Other qualification", "No qualification") ~ "Other",
                         j_hiqual_dv %in% c("missing", "inapplicable") ~ NA_character_,
                         T ~ as.character(j_hiqual_dv))%>%
           factor()%>%
           relevel("Degree"))

# Join household data to main dataframe so that equivalised household income can be calculated
hh_df<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/j_hhresp.dta", generate.factors = T, convert.factors = T, nonint.factors = T)

cols<- colnames(hh_df)

newcols<- c(cols[!cols %in% colnames(main_df)], "j_hidp")

hh_df<- hh_df%>%
  select(all_of(newcols))

main_df<- main_df%>%
  mutate(j_hidp = j_hidp%>%
           as.character()%>%
           as.numeric())

main_df<- left_join(main_df, hh_df, by = "j_hidp")

# Create equivalised gross household income variable and quintiles
main_df<- main_df%>%
  mutate(hh_inc = as.numeric(as.character(j_fihhmngrs_dv))/as.numeric(as.character(j_ieqmoecd_dv)))

main_df<- main_df%>%
  mutate(hh_inc_quint = case_when(!is.na(hh_inc) ~ paste0("q", as.character(ntile(hh_inc, 5))),
                                  T ~ NA_character_)%>%
           factor())

# Scale and centre the income variable
main_df<- main_df%>%
  mutate(scaled_hh_inc = (hh_inc - mean(hh_inc, na.rm = T))/sd(hh_inc, na.rm = T))


# Create a variable recording NS-SEC class or unemployment or not in labour market
nssec_levels<- levels(main_df$j_jbnssec5_dv)
jbstat_levels<- levels(main_df$j_jbstat)

main_df<- main_df%>%
  mutate(occupational_class = case_when(j_jbnssec5_dv %in% nssec_levels[6:10] ~ j_jbnssec5_dv,
                                        j_jbstat == "Unemployed" ~ "Unemployed",
                                        j_jbstat %in% jbstat_levels[8:18] ~ "Not in labour market",
                                        T ~ NA_character_))%>%
  mutate(occupational_class = factor(occupational_class, levels = c(nssec_levels[6:10], "Unemployed", "Not in labour market")))


#### Step 5: Health Behaviour Variables ####


# Read in Wave 9 data, since the alcohol consumption, physical activity and neighbourhood cohesion variables are not in Wave 10
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_9.Rda")


# Select the relevant variables from Wave 9
df_w9<- df_w9%>%
  select(pidp,
         i_auditc1,
         i_auditc2,
         i_auditc3,
         i_auditc4,
         i_auditc5,
         i_vday,
         i_vdhrs,
         i_vdmin,
         i_mday,
         i_mdhrs,
         i_mdmin,
         i_wday,
         i_wdhrs,
         i_wdmin,
         i_wkfruit,
         i_wkvege,
         i_nbrsnci_dv)

# Join Wave 9 to the main data
main_df<- left_join(main_df, df_w9, by = "pidp")

# Create Audit-c variable for measuring alcohol use

# Create a score for Audit-C questions 1 - 3
auditc3_levels<- levels(main_df$i_auditc3)
auditc4_levels<- levels(main_df$i_auditc4)
auditc5_levels<- levels(main_df$i_auditc5)

main_df<- main_df%>%
  mutate(auditq1 = case_when(i_auditc3 == auditc3_levels[6]~ 0,
                             i_auditc3 == auditc3_levels[7]~ 1,
                             i_auditc3 == auditc3_levels[8]~ 2,
                             i_auditc3 == auditc3_levels[9]~ 3,
                             i_auditc3 == auditc3_levels[10]~ 4,
                             i_auditc3 %in% auditc3_levels[1:5] ~ NA_real_),
         auditq2 = case_when(i_auditc4 == auditc4_levels[6]~ 0,
                             i_auditc4 == auditc4_levels[7]~ 1,
                             i_auditc4 == auditc4_levels[8]~ 2,
                             i_auditc4 == auditc4_levels[9]~ 3,
                             i_auditc4 == auditc4_levels[10]~ 4,
                             i_auditc4 %in% auditc4_levels[1:5] ~ NA_real_),
         auditq3 = case_when(i_auditc5 == auditc5_levels[6]~ 0,
                             i_auditc5 == auditc5_levels[7]~ 1,
                             i_auditc5 == auditc5_levels[8]~ 2,
                             i_auditc5 == auditc5_levels[9]~ 3,
                             i_auditc5 == auditc5_levels[10]~ 4,
                             i_auditc5 %in% auditc5_levels[1:5] ~ NA_real_))

# Sum the score from Audit-C questions 1 - 3 to get an overall score
main_df<- main_df%>%
  rowwise()%>%
  mutate(auditc_score = sum(auditq1, auditq2, auditq3))

# Classify the level of alcohol dependency risk
main_df<- main_df%>%
  mutate(alcohol = case_when(i_auditc1 == "No" ~ "None",
                             auditc_score < 3 ~ "Low",
                             auditc_score >= 3 & auditc_score <= 5 & j_sex_dv == "Female" ~ "Moderate",
                             auditc_score >= 4 & auditc_score <= 5 & j_sex_dv == "Male" ~ "Moderate",
                             auditc_score >= 6 & auditc_score <= 7 ~ "High",
                             auditc_score >= 8 & auditc_score <= 12 ~ "Severe",
                             T ~ NA_character_)%>%
           factor(levels = c("None", "Low", "Moderate", "High", "Severe")))

# Create smoking variable
main_df<- main_df%>%
  mutate(smoker = case_when(j_smoker %in% levels(main_df$j_smoker)[1:5] ~ NA_character_,
                            T ~ as.character(j_smoker))%>%
           factor(levels = c("No", "Yes")))


# Create physical activity variable

# Convert variables measuring frequency of vigorous/moderate/walk activity from a factor (with numbers for labels) to a numeric variable
main_df$i_vday<- as.numeric(levels(main_df$i_vday))[main_df$i_vday]
main_df$i_vdhrs<- as.numeric(levels(main_df$i_vdhrs))[main_df$i_vdhrs]
main_df$i_vdmin<- as.numeric(levels(main_df$i_vdmin))[main_df$i_vdmin]

main_df$i_mday<- as.numeric(levels(main_df$i_mday))[main_df$i_mday]
main_df$i_mdhrs<- as.numeric(levels(main_df$i_mdhrs))[main_df$i_mdhrs]
main_df$i_mdmin<- as.numeric(levels(main_df$i_mdmin))[main_df$i_mdmin]

main_df$i_wday<- as.numeric(levels(main_df$i_wday))[main_df$i_wday]
main_df$i_wdhrs<- as.numeric(levels(main_df$i_wdhrs))[main_df$i_wdhrs]
main_df$i_wdmin<- as.numeric(levels(main_df$i_wdmin))[main_df$i_wdmin]

# Create variable indicating whether respondent does some vigorous activity, some moderate activity, or no moderate/vigorous activity
main_df<- main_df%>%
  mutate(activity = case_when(i_vday>0 ~ "Some vigorous activity",
                              i_mday>0 & (i_vday == 0 | is.na(i_vday)) ~ "Some moderate activity",
                              i_vday == 0 & i_mday == 0 ~ "No activity",
                              T ~ NA_character_)%>%
           factor(levels = c("No activity", "Some moderate activity", "Some vigorous activity")))


#  Create variable indicating whether respondent eats fruit/vegetables daily, less than daily or never
main_df<- main_df%>%
  mutate(fruit = case_when(i_wkfruit == "Every day" ~ "Daily",
                           i_wkfruit %in% c("1 - 3 Days", "4 - 6 Days") ~ "Weekly",
                           i_wkfruit == "Never" ~ "Never",
                           T ~ NA_character_))%>%
  mutate(vegetables = case_when(i_wkvege == "Every day" ~ "Daily",
                           i_wkvege %in% c("1 - 3 Days", "4 - 6 Days") ~ "Weekly",
                           i_wkvege == "Never" ~ "Never",
                           T ~ NA_character_))%>%
  mutate(fruit_or_vegetables = case_when(fruit == "Daily" | vegetables == "Daily" ~ "Daily",
                                         fruit == "Weekly" | vegetables == "Weekly" ~ "Weekly",
                                         fruit == "Never" & vegetables == "Never" ~ "Never",
                                         T ~ NA_character_))%>%
  mutate(fruit_or_vegetables = factor(fruit_or_vegetables, levels = c("Daily", "Weekly", "Never")))



#### Step 6: Psycho-social Variables ####


## Create loneliness variable

sclonely_levels<- levels(main_df$j_sclonely)

main_df<- main_df%>%
  mutate(lonely = case_when(j_sclonely %in% sclonely_levels[1:5] ~ NA_character_,
                            T ~ as.character(j_sclonely)))

main_df<- main_df%>%
  mutate(lonely = factor(lonely, levels = c("Hardly ever or never", "Some of the time", "Often")))


#### Step 7: Neighbourhood Cohesion Variable ####


# Recode neighbourhood cohesion variable
main_df<- main_df%>%
  mutate(nb_cohesion = case_when(i_nbrsnci_dv == "lowest cohesion" ~ 1,
                                 i_nbrsnci_dv == "highest cohesion" ~ 5,
                                 T ~ as.numeric(as.character(i_nbrsnci_dv))))

# Create cohesion quintile variable
main_df$nb_cohesion_quint<- ntile(main_df$nb_cohesion, 5)

main_df<- main_df%>%
  mutate(nb_cohesion_quint = case_when(!is.na(nb_cohesion_quint) ~ paste0("q", nb_cohesion_quint),
                                       T ~ NA_character_)%>%
           factor(levels = paste0("q", 1:5)))



#### Step 8: Neighbourhood Deprivation Variables ####

# Read in LSOA codes for each household
lsoas<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-7248-stata/stata/stata13/ukhls/j_lsoa11_protect.dta",
                   convert.factors = T,
                   generate.factors = T,
                   nonint.factors = T)

# Join LSOA codes to data
main_df<- left_join(main_df, lsoas, by = "j_hidp")

# Read in GB-wide income deprivation scores
gb_imd<- read.csv("../../Data/LSOA Data/LSOA_GB_IMD_Data.csv")

# Select just the income deprivation score and LSOA codes
gb_imd<- gb_imd%>%
  select(lsoa, income_score)

# Rename the income deprivation variable
gb_imd<- gb_imd%>%
  rename(nb_lsoa_gb_deprivation_score = income_score)

# Join the income deprivation score to the main data
main_df<- left_join(main_df, gb_imd, by = c("j_lsoa11" = "lsoa"))

# Create income deprivation quintile
main_df$nb_lsoa_gb_deprivation_quint<- ntile(main_df$nb_lsoa_gb_deprivation_score, 5)

main_df<- main_df%>%
  mutate(nb_lsoa_gb_deprivation_quint = case_when(!is.na(nb_lsoa_gb_deprivation_quint) ~ paste0("q", nb_lsoa_gb_deprivation_quint),
                                                  T ~ NA_character_)%>%
           factor(paste0("q", 1:5)))


# Read in lookup between LSOAs and MSOAs and select relevant variables
msoa_lsoa_lookup<- read.csv("../../Lookups/OA11 LSOA11 MSOA11 LAD11 Lookup.csv")%>%
  select(LSOA11CD, MSOA11CD)%>%
  unique()

# Read in lookup between Data Zones and Intermediate Zones in Scotland and join to the LSOA-MSOA lookup
dz_iz_lookup<- read.csv("../../Lookups/Scotland Data Zone Intermediate Zone Lookup.csv")

dz_iz_lookup<- dz_iz_lookup%>%
  rename(LSOA11CD = DZcode,
         MSOA11CD = IZcode)

msoa_lsoa_lookup<- rbind(msoa_lsoa_lookup, dz_iz_lookup)


# Create dataframe of LSOA-MSOA combinations with the relevant income deprivation score for each LSOA
gb_msoa_imd<- left_join(msoa_lsoa_lookup, gb_imd, by = c("LSOA11CD" = "lsoa"))

# Read in the population size used in the IMD calculation for each country (for Wales this has to be calculated)

eng_lsoa_imd<- read.csv("../../Data/LSOA Data/LSOA_IMD_2019_Detailed_Data.csv")

eng_imd_population<- eng_lsoa_imd%>%
  select(LSOA11CD, IMD_Population)

scot_imd_population<- read.csv("../../Data/LSOA Data/Data Zone IMD Population.csv")

scot_imd_population<- scot_imd_population%>%
  rename(IMD_Population = IMD_population,
         LSOA11CD = DZcode)

wales_income_deprived_count<- read.csv("../../Data/LSOA Data/LSOA Wales Income Deprived Counts.csv")

wales_income_deprived_count<- left_join(wales_income_deprived_count, gb_imd, by = c("LSOA11CD" = "lsoa"))

wales_imd_population<- wales_income_deprived_count%>%
  mutate(IMD_Population = (Income_Deprived_Count/nb_lsoa_gb_deprivation_score)*100)%>%
  select(LSOA11CD, IMD_Population)

gb_imd_population<- rbind(eng_imd_population, wales_imd_population)%>%
  rbind(scot_imd_population)

# Join the IMD population to the LSOA-MSOA data
gb_msoa_imd<- left_join(gb_msoa_imd, gb_imd_population, by = "LSOA11CD")

# Calculate a population-weighted income deprivation score for each MSOA
gb_msoa_imd<- gb_msoa_imd%>%
  mutate(scaled_deprivation_score = nb_lsoa_gb_deprivation_score*IMD_Population)%>%
  group_by(MSOA11CD)%>%
  summarise(msoa_scaled_deprivation_score = sum(scaled_deprivation_score),
            msoa_imd_population = sum(IMD_Population))%>%
  ungroup()%>%
  mutate(nb_gb_msoa_deprivation_score = msoa_scaled_deprivation_score/msoa_imd_population)%>%
  select(MSOA11CD, nb_gb_msoa_deprivation_score)


# Join MSOA codes to main df
main_df<- left_join(main_df, msoa_lsoa_lookup, by = c("j_lsoa11" = "LSOA11CD"))

# Join the population-weighted MSOA income deprivation score to the main df
main_df<- left_join(main_df, gb_msoa_imd, by = "MSOA11CD")

# Create GB MSOA deprivation quintile
main_df$nb_gb_msoa_deprivation_quintile<- ntile(main_df$nb_gb_msoa_deprivation_score, 5)

main_df<- main_df%>%
  mutate(nb_gb_msoa_deprivation_quintile = case_when(!is.na(nb_gb_msoa_deprivation_quintile) ~ paste0("q", nb_gb_msoa_deprivation_quintile),
                                                     T ~ NA_character_)%>%
           factor(levels = paste0("q", 1:5)))



#### Step 9: Neighbourhood Health Environment Variables ####


# Read in AHAH data
ahah<- read.csv("../../Data/LSOA Data/AHAH_V3_0.csv")

# Select relevant AHAH variables
ahah<- ahah%>%
  rename(air_quality = ah3e,
         retail_environment = ah3r,
         health_access = ah3h,
         green_blue_space = ah3g,
         no2 = ah3no2,
         so2 = ah3so2,
         pm10 = ah3pm10)%>%
  select(lsoa11, air_quality, retail_environment, health_access, green_blue_space, no2, so2, pm10)

# Join the LSOA AHAH scores to the main data
ahah_lsoas<- ahah%>%
  rename_with(.cols = air_quality:pm10, ~paste0("lsoa_", .x))

main_df<- left_join(main_df, ahah_lsoas, by = c("j_lsoa11" = "lsoa11"))

# Create dataframe with LSOA-MSOA combinations and each LSOA population (based on the IMD population)
ahah_lsoas_msoas<- left_join(msoa_lsoa_lookup, gb_imd_population, by = "LSOA11CD")

# Join AHAH variables to the LSOA-MSOA combinations
ahah_lsoas_msoas<- left_join(ahah_lsoas_msoas, ahah, by = c("LSOA11CD" = "lsoa11"))

# Create population-weighted AHAH scores for each MSOA
ahah_lsoas_msoas<- ahah_lsoas_msoas%>%
  mutate(across(air_quality:pm10, ~.x*IMD_Population, .names = "population_scaled_{col}"))

ahah_msoas<- ahah_lsoas_msoas%>%
  group_by(MSOA11CD)%>%
  summarise(across(population_scaled_air_quality:population_scaled_pm10, ~sum(.x), .names = "summed_{col}"),
            summed_population = sum(IMD_Population))%>%
  ungroup()

ahah_msoas<- ahah_msoas%>%
  mutate(across(summed_population_scaled_air_quality:summed_population_scaled_pm10, ~.x/summed_population, .names = "population_weighted_{col}"))%>%
  select(MSOA11CD, contains("weighted"))%>%
  rename_with(~str_remove(.x, "summed_population_scaled_"))%>%
  rename_with(~str_replace(.x, "population_weighted_", "msoa_"))

# Join the population-weighted AHAH scores to the main data
main_df<- left_join(main_df, ahah_msoas, by = "MSOA11CD")

# Reverse the green/blue space measures so that highger values equates to more green/blue space
main_df<- main_df%>%
  mutate(msoa_green_blue_space = -1*msoa_green_blue_space)


# Save data
save(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Cross_Sectional_Analysis_Dataset.Rda")
