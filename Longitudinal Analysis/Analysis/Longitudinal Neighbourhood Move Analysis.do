/*

##############################################
#                                            #
#  LONGITUDINAL NEIGHBOURHOOD MOVE ANALYSIS  #
#                                            #
##############################################

Laurence Rowley-Abel, University of Edinburgh

Desciption: This file analyses indidivuals with no health conditions at baseline who then moved neighbourhood. It analyses whether, within this subgroup, those who experienced a decrease in neighbourhood cohesion developed multimorbidity compared to those who didn't experience a decrease in neighbourhood cohesion.

.
*/


// Set working directory
cd "Y:\Neighbourhood Cohesion and Multimorbidity\Longitudinal Analysis\Analysis"

// Read in data
use "..\..\Data\Understanding Society Data\Processed Data\Understanding_Society_Longitudinal_Analysis_Dataset.dta", clear

// Set up data as survey data with complex sample
svyset pidp, weight(j_indscui_lw) , clear

// Create variable indicating no baseline health conditions
recode condition_count_w1 (0 = 1) (nonm = 0), generate(no_baseline_conditions)

// Re-label the no_baseline_conditions variable
label var no_baseline_conditions "Has no baseline conditions"

// Check recoding
tab condition_count_w1 no_baseline_conditions

// Create indicator of subpopulation to anlyse (ie: those with no baseline conditions who moved neighbourhoods between Wave 1 and Wave 10)
gen no_baseline_conditions_moved = 0
replace no_baseline_conditions_moved = 1 if no_baseline_conditions == 1 & moved_lsoa == 1

// Look at the number of individuals with no baseline conditions who move, by whether they became mm
tab mm no_baseline_conditions_moved

// Run a design-corrected Chi-Squared test of whether experience a decrease in cohesion between basleine and outcome was associated with greater risk of becoming multimorbid
svy, subpop(no_baseline_conditions_moved): tab mm cohesion_decrease, col
tab mm cohesion_decrease if no_baseline_conditions_moved == 1 & j_indscui_lw != . & j_indscui_lw > 0

// Run a logistic regression model to calculate the odds of becoming multimorbid if you experienced a decrease in cohesion
svy, subpop(no_baseline_conditions_moved): logit mm i.cohesion_decrease, or