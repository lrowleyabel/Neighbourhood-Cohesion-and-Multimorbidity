/*

##################################
#                                #
#  LONGITUDINAL MULTILEVEL ODEL  #
#                                #
################################

Laurence Rowley-Abel, University of Edinburgh

Desciption: This file runs multilevel logistic regressions to model whether healthy respondents become multimorbid over the nine-year follow-up.

.
*/

// Set working directory
cd "Y:\Neighbourhood Cohesion and Multimorbidity\Longitudinal Analysis\Analysis"

// Read in data
use "..\..\Data\Understanding Society Data\Processed Data\Understanding_Society_Longitudinal_Analysis_Dataset.dta", clear

// Create a constant weight for census areas
gen census_area_level_weight = 1

// Set up data as survey data with complex sample
svyset MSOA11CD, weight(census_area_level_weight) || pidp, weight(j_indscui_lw) , clear

// Create variable indicating no baseline health conditions
recode condition_count_w1 (0 = 1) (nonm = 0), generate(no_baseline_conditions)

// Re-label the no_baseline_conditions variable
label var no_baseline_conditions "Has no baseline conditions"

// Check recoding
tab condition_count_w1 no_baseline_conditions

// Create variable indicating complete case
gen complete_case = 0
replace complete_case = 1 if age != . & sex != . & nb_cohesion_quint != .

// Look at number of those with no baseline conditions who became multimorbid
tab mm no_baseline_conditions if j_indscui_lw != 0 & j_indscui_lw != . & complete_case == 1

// Create variable indicating those to be included in the models
gen no_baseline_conditions_complete = 0
replace no_baseline_conditions_complete = 1 if no_baseline_conditions == 1 & complete_case == 1


// Generate an age-squared variable
gen age_sqrd = age*age


// Run multilevel logit model
svy, subpop(no_baseline_conditions_complete) : melogit mm age age_sqrd i.sex i.nb_cohesion_quint || MSOA11CD: , or

estat icc

capture drop ystar*
predict ystar, eta
gen ystar_odds = exp(ystar)
gen ystar_prob = ystar_odds / (1+ystar_odds)
roctab mm ystar_prob, graph

