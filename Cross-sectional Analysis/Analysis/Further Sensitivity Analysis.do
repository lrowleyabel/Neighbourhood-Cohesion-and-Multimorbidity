/*

//////////////////////////////////
// FURTHER SENSITIVITY ANALYSIS //
//////////////////////////////////

Laurence Rowley-Abel

Description: This file runs further sensitivity analyses, including:
- a bivariate cross-tab of loneliness and neighbourhood cohesion to check that individuals in less cohesive neighbourhoods are more lonely
- a mediation model to formally test whether the effect of neighbourhood cohesion is mediated through loneliness
- further models that change/remove certain variables

*/

clear all

// Set working directory
cd "Y:\Neighbourhood Cohesion and Multimorbidity\Cross-sectional Analysis\Analysis"

//Load in dataset
use "..\..\Data\Understanding Society Data\Processed Data\Understanding_Society_Cross_Sectional_Analysis_Dataset_With_Multilevel_Weights", clear



// Set up data as survey data with complex sample
svyset MSOA11CD, weight(census_area_level_weight) || pidp, weight(j_indscui_lw) 

// Create variable indicating observations with complete values for all relevant variables
egen n_complete = rownonmiss(mm age sex nb_gb_msoa_deprivation_quint hh_inc_quint edu occupational_class nb_cohesion_quint msoa_air msoa_green msoa_retail alcohol smoker activity fruit_or_vegetables lonely)

gen complete_case = 1 if n_complete == 16 & MSOA11CD != "NA"
replace complete_case = 0 if n_complete < 16 | MSOA11CD == "NA" 



//// Loneliness and Neighbourhood Cohesion Bivariate Check ////


// Run a cross-tab of loneliness and cohesion
svy, subpop(complete_case): tab lonely nb_cohesion_quint, col



//// Mediation analysis ////


// Recode loneliness as a binary variable
recode lonely (1 = 0 "No") (2 3 = 1 "Yes"), gen("bi_lonely")

// Run the model using structural equation modelling
svy, subpop(complete_case): gsem (mm <- nb_cohesion bi_lonely age age_sqrd i.sex M1[MSOA11CD]) (bi_lonely <- nb_cohesion age age_sqrd i.sex M2[MSOA11CD]), logit

// Calculate the indirect effect
nlcom _b[mm:bi_lonely]*_b[bi_lonely:nb_cohesion]

mat x = r(b)

global indirect_effect = x[1,1]

// Calculate the total effect
nlcom _b[mm:nb_cohesion] + _b[mm:bi_lonely]*_b[bi_lonely:nb_cohesion]

mat x = r(b)

global total_effect = x[1,1]

// Calculate proportion of total effect that is mediated through loneliness
di $indirect_effect/$total_effect



//// Further Models ////

// Run model without education and occupational class controls
svy, subpop(complete_case): melogit mm c.age##c.age i.sex i.nb_cohesion_quint i.lonely i.nb_gb_msoa_deprivation_quint msoa_air msoa_green msoa_retail i.j_urban_dv i.hh_inc_quint ib3.alcohol i.smoker i.activity i.fruit_or_veg || MSOA11CD: , or


// Run models with individual pollutants
svy, subpop(complete_case): melogit mm c.age##c.age i.sex i.hh_inc_quint msoa_so2 || MSOA11CD: , or

svy, subpop(complete_case): melogit mm c.age##c.age i.sex i.hh_inc_quint msoa_so2 msoa_green_blue_space || MSOA11CD: , or

svy, subpop(complete_case): melogit mm c.age##c.age i.sex i.hh_inc_quint msoa_so2 i.nb_gb_msoa_deprivation_quint || MSOA11CD: , or


