########################################
#                                      #
#  CREATING RDA FILES FOR WAVES 1 9 10 #
#                                      #
########################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This reads in the raw .dta files for waves 1, 9 and 10 of Understanding Society and saves them as .Rda files


# Load libraries
library(readstata13)

# Clear environment
rm(list = ls())

# Set path to repository root directory (change this to wherever you have saved the repository)
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Cross-sectional Analysis/Data Preparation"))

# Read in Waves 1, 9 and 10 from the .dta files
df_w1<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/a_indresp.dta",
                   convert.factors = T,
                   generate.factors = T,
                   nonint.factors = T)

df_w9<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/i_indresp.dta",
                         convert.factors = T,
                         generate.factors = T,
                         nonint.factors = T)

df_w10<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/j_indresp.dta",
                         convert.factors = T,
                         generate.factors = T,
                         nonint.factors = T)

# Save Waves 1, 9 and 10 in Rda format
save(df_w1, file =  "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1.Rda")

save(df_w9, file =  "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_9.Rda")

save(df_w10, file =  "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_10.Rda")
