################################################################
#                                                              #
#  CREATING WAVE 10 VARIABLES FOR NEIGHBOURHOOD MOVE ANALYSIS  #
#                                                              #
################################################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 14/05/24

# DESCRIPTION: This file creates indicators of whether individuals moved neighbourhoods between Wave 1 and Wave 10 and whether they experienced a decrease in neighbourhood cohesion in that period.

library(dplyr)
library(tidyr)

# Clear environment
rm(list = ls())

# Set path to repository root directory
root_dir<- "Z:/Neighbourhood and Multimorbidity Special License"

# Set working directory
setwd(paste0(root_dir, "/Longitudinal Analysis/Data Preparation"))

# Load in analysis dataset
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Analysis_Dataset.Rda")

# Load data containing individuals' neighbourhood at Wave 10 and join to data
lsoas_w10<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-7248-stata/stata/stata13/ukhls/j_lsoa11_protect.dta",
                   convert.factors = T,
                   generate.factors = T,
                   nonint.factors = T)

main_df<- left_join(main_df, lsoas_w10, by = "j_hidp")

# Create indicator of whether individuals live in a different LSOA at Wave 10 to the one they lived in at Wave 1
main_df<- main_df%>%
  mutate(moved_lsoa = ifelse(a_lsoa11 != j_lsoa11 & !is.na(a_lsoa11) & !is.na(j_lsoa11), 1, 0))

# Look at the number of individuals who moved LSOA
table(main_df$moved_lsoa)

# Load data containing the Wave 9 neighbourhood cohesion variable and join to main data
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_9.Rda")

main_df<- left_join(main_df, df_w9%>%
                      select(pidp, i_nbrsnci_dv), by = "pidp")

# Create variable with neighbourhood cohesion quintile at Wave 9
main_df<- main_df%>%
  mutate(w9_nb_cohesion = case_when(i_nbrsnci_dv == "lowest cohesion" ~ 1,
                                 i_nbrsnci_dv == "highest cohesion" ~ 5,
                                 T ~ as.numeric(as.character(i_nbrsnci_dv))))

main_df$w9_nb_cohesion_quint<- ntile(main_df$w9_nb_cohesion, 5)

main_df<- main_df%>%
  mutate(w9_nb_cohesion_quint = case_when(!is.na(w9_nb_cohesion_quint) ~ paste0("q", w9_nb_cohesion_quint),
                                       T ~ NA_character_)%>%
           factor(levels = paste0("q", 1:5)))

# Create an indicator of having experienced a decrease in neighbourhood cohesion quintile
main_df<- main_df%>%
  mutate(cohesion_w1 = str_extract(nb_cohesion_quint, "[0-9]"),
         cohesion_w9 = str_extract(w9_nb_cohesion_quint, "[0-9]"))%>%
  mutate(cohesion_decrease = case_when(cohesion_w9 < cohesion_w1 ~ 1,
                                       cohesion_w9 >= cohesion_w1 ~ 0,
                                       !is.na(cohesion_w1) & !is.na(cohesion_w9) ~ NA_integer_))

# Look at the number of individuals that experienced a decrease in cohesion, separated by whether they moved or not
main_df%>%
  with(table(cohesion_decrease, moved_lsoa))

# Save the dataset
save(main_df, file = "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Analysis_Dataset.Rda")
