######################################
#                                    #
#  JOINING INDRESP AND INDALL FILES  #
#                                    #
######################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH

# DESCRIPTION: This reads in the household grid data for each individual at each wave (w_indall files), which contain cross-wave identifiers for PSU and strata. These variables are then joined
# to the main substantive datasets for Waves 1, 9 and 10.


library(readstata13)
library(dplyr)

# Clear environment
rm(list = ls())

# Set path to repository root directory (change this to wherever you have saved the repository)
root_dir<- "Y:/Neighbourhood Cohesion and Multimorbidity"

# Set working directory
setwd(paste0(root_dir, "/Cross-sectional Analysis/Data Preparation"))

# Read in indall files for Waves 1, 9 and 10
indall_w1<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/a_indall.dta",
                       convert.factors = T,
                       generate.factors = T,
                       nonint.factors = T)


indall_w9<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/i_indall.dta",
                   convert.factors = T,
                   generate.factors = T,
                   nonint.factors = T)

indall_w10<- read.dta13("../../Data/Understanding Society Data/Raw Data/UKDA-6614-stata/stata/stata13_se/ukhls/j_indall.dta",
                    convert.factors = T,
                    generate.factors = T,
                    nonint.factors = T)

# Read in the main datasets for each wave, already saved in .Rda format
load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1.Rda")

load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_9.Rda")

load("../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_10.Rda")

# For the indall data, select only the variables which are not in the main dataset, plus the PIDP variable (person identifier)
cols_w1<- colnames(indall_w1)[!colnames(indall_w1) %in% colnames(df_w1)]

indall_w1<- indall_w1%>%
  select(all_of(c(cols_w1, "pidp")))

cols_w9<- colnames(indall_w9)[!colnames(indall_w9) %in% colnames(df_w9)]

indall_w9<- indall_w9%>%
  select(all_of(c(cols_w9, "pidp")))

cols_w10<- colnames(indall_w10)[!colnames(indall_w10) %in% colnames(df_w10)]

indall_w10<- indall_w10%>%
  select(all_of(c(cols_w10, "pidp")))


# Join indall data to main data
df_w1<- left_join(df_w1, indall_w1, by = "pidp")

df_w9<- left_join(df_w9, indall_w9, by = "pidp")

df_w10<- left_join(df_w10, indall_w10, by = "pidp")


# Save updated data
save(df_w1, file =  "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_1.Rda")

save(df_w9, file =  "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_9.Rda")

save(df_w10, file =  "../../Data/Understanding Society Data/Processed Data/Understanding_Society_Wave_10.Rda")
