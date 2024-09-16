/*

/////////////////////////////////
// TWO-LEVEL MULTILEVEL MODELS //
/////////////////////////////////

Laurence Rowley-Abel

Description: This file runs two-level mutlilevel models to estimate the relationship between multimorbidity and neighbourhood cohesion, controlling for other variables.

*/

clear all

// Set working directory
cd "Y:\Neighbourhood Cohesion and Multimorbidity\Cross-sectional Analysis\Analysis"

//Load in dataset
use "..\..\Data\Understanding Society Data\Processed Data\Understanding_Society_Cross_Sectional_Analysis_Dataset_With_Multilevel_Weights", clear

// Set up data as survey data with complex sample
svyset MSOA11CD, weight(census_area_level_weight) || pidp, weight(j_indscui_lw) 

// Check coding
codebook mm age sex edu hh_inc alcohol smoker activity lonely nb_cohesion_quint nb_gb_msoa_deprivation_quint 


// Create variable indicating observations with complete values for all relevant variables
egen n_complete = rownonmiss(mm age sex nb_gb_msoa_deprivation_quint hh_inc_quint edu occupational_class nb_cohesion_quint msoa_air msoa_green msoa_retail alcohol smoker activity fruit_or_vegetables lonely)

gen complete_case = 1 if n_complete == 16 & MSOA11CD != "NA"
replace complete_case = 0 if n_complete < 16 | MSOA11CD == "NA" 


// Run model with just neighbourhood cohesion and controls for age and sex
svy, subpop(complete_case): melogit mm c.age##c.age i.sex i.nb_cohesion_quint || MSOA11CD: , or
estimates store m1

// Test the difference between the fourth and fifth quintile of neighbourhood cohesion 
test 4.nb_cohesion_quint = 5.nb_cohesion_quint

estat icc

capture drop ystar*
predict ystar, eta
gen ystar_odds = exp(ystar)
gen ystar_prob = ystar_odds / (1+ystar_odds)
roctab mm ystar_prob, graph

// Add loneliness to model
subpop(complete_case): melogit mm c.age##c.age i.sex i.nb_cohesion_quint i.lonely || MSOA11CD: , or
estimates store m2

estat icc

capture drop ystar*
predict ystar, eta
gen ystar_odds = exp(ystar)
gen ystar_prob = ystar_odds / (1+ystar_odds)
roctab mm ystar_prob, graph


// Add all control variables
svy, subpop(complete_case): melogit mm c.age##c.age i.sex i.nb_cohesion_quint i.lonely i.nb_gb_msoa_deprivation_quint msoa_air msoa_green msoa_retail i.j_urban_dv i.hh_inc_quint i.edu i.occupational_class ib3.alcohol i.smoker i.activity i.fruit_or_veg || MSOA11CD: , or
estimates store m3

estat icc

capture drop ystar*
predict ystar, eta
gen ystar_odds = exp(ystar)
gen ystar_prob = ystar_odds / (1+ystar_odds)
roctab mm ystar_prob, graph


