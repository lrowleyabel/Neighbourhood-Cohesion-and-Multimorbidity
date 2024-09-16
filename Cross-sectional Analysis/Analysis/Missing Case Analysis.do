/*

///////////////////////////
// MISSING CASE ANALYSIS //
///////////////////////////

Laurence Rowley-Abel

Description: This file looks at how the analytic sample and the full sample differ in terms of key demographics and model variables.

*/


clear all

// Set working directory
cd "Y:\Neighbourhood Cohesion and Multimorbidity\Cross-sectional Analysis\Analysis"

//Load in dataset
use "..\..\Data\Understanding Society Data\Processed Data\Understanding_Society_Cross_Sectional_Analysis_Dataset_With_Multilevel_Weights", clear


// Create variable indicating observations with complete values for all relevant variables
egen n_complete = rownonmiss(mm age sex nb_gb_msoa_deprivation_quint hh_inc_quint edu occupational_class nb_cohesion_quint msoa_air msoa_green msoa_retail alcohol smoker activity fruit_or_vegetables lonely)

gen complete_case = 1 if n_complete == 16 & MSOA11CD != "NA" & j_indscui_lw != 0
replace complete_case = 0 if n_complete < 16 | MSOA11CD == "NA" | j_indscui_lw == 0


// Create a table of the proportion of the full sample that is in the analytic sample
tab complete_case

// Look at the number missing due to having a zero-value for the longitudinal weight
gen zero_wt = j_indscui_lw == 0
tab zero_wt
// 14,315 missing due to zero weight


// Look at the number missing due to having missing values on a Wave 10 variable (of those who have a non-zero value for the weight)
egen n_complete_w10 = rownonmiss(mm age sex hh_inc_quint edu occupational_class )
gen missing_w10_variables = n_complete_w10 < 6
tab missing_w10_variables if zero_wt == 0
// Further 1,765 missing due to missing Wave 10 variables


// Look at the number missing due to having missing values on a Wave 9 variable (of those who have a non-zero value for the weight and complete Wave 10 variables)
egen n_complete_w9 = rownonmiss(nb_cohesion_quint alcohol smoker activity fruit_or_vegetables lonely)
gen missing_w9_variables = n_complete_w9 < 6
tab missing_w9_variables if zero_wt == 0 & missing_w10_variables == 0
// Further 1,628 missing due to missing Wave 9 variables


// Look at the number missing due to having missing values on the linked neighbourhood variables
egen n_complete_nb = rownonmiss(nb_gb_msoa_deprivation_quint msoa_air msoa_green msoa_retail)
gen missing_nb_variables = n_complete_nb < 4
tab missing_nb_variables if zero_wt == 0 & missing_w10_variables == 0 & missing_w9_variables == 0
tab missing_nb_variables j_country if zero_wt == 0 & missing_w10_variables == 0 & missing_w9_variables == 0
// Further 1,088 missing due to missing linked neighbourhood variables

// Show total number missing and check this matches the complete_case variable
di 14315 + 1765 + 1628 + 1088
tab complete_case


// Look at the proportions/means of the main variables in the full sample, without weighting

collect clear
capture: vl drop categorical_vars
vl create categorical_vars = (mm sex nb_gb_msoa_deprivation_quint hh_inc_quint nb_cohesion_quint lonely)

foreach v of varlist $categorical_vars {
	table (var), statistic(fvpercent `v') append
}

collect dims

collect layout ($categorical_vars) (result)
collect export "Full Sample Unweighted Proportions.docx", replace

collect clear
capture: vl drop continuous_vars
vl create continuous_vars = (age msoa_air msoa_green msoa_retail)

foreach v of varlist $continuous_vars {
	table (var), statistic(mean `v') append
}


collect layout (var) (result)
collect export "Full Sample Unweighted Means.docx", replace



// Look at the proportions/means of the main variables in the full sample with cross-sectional weights
svyset [pweight = j_indscui_xw]
collect clear

collect clear
capture: vl drop categorical_vars
vl create categorical_vars = (mm sex nb_gb_msoa_deprivation_quint hh_inc_quint nb_cohesion_quint lonely)

foreach v of varlist $categorical_vars {
	collect, tag(variable[`v']): svy: tab `v'
}


collect layout (variable#colname) (result[_r_b])
collect export "Full Sample Weighted Proportions.docx", replace

collect clear
capture: vl drop continuous_vars
vl create continuous_vars = (age msoa_air msoa_green msoa_retail)

foreach v of varlist $continuous_vars {
	collect, tag(variable[`v']): svy: mean `v'
}


collect layout (variable) (result[_r_b])
collect export "Full Sample Weighted Means.docx", replace


// Look at the proportions/means of the main variables in the full sample with longitudinal weights
svyset [pweight = j_indscui_lw]
collect clear

collect clear
capture: vl drop categorical_vars
vl create categorical_vars = (mm sex nb_gb_msoa_deprivation_quint hh_inc_quint nb_cohesion_quint lonely)

foreach v of varlist $categorical_vars {
	collect, tag(variable[`v']): svy: tab `v'
}


collect layout (variable#colname) (result[_r_b])
collect export "Full Sample Longitudinally Weighted Proportions.docx", replace

collect clear
capture: vl drop continuous_vars
vl create continuous_vars = (age msoa_air msoa_green msoa_retail)

foreach v of varlist $continuous_vars {
	collect, tag(variable[`v']): svy: mean `v'
}


collect layout (variable) (result[_r_b])
collect export "Full Sample Longitudinally Weighted Means.docx", replace

// Look at the proportions/means of the main variables in the analytic sample with longitudinal weights
svyset [pweight = j_indscui_lw]

collect clear
capture: vl drop categorical_vars
vl create categorical_vars = (mm sex nb_gb_msoa_deprivation_quint hh_inc_quint nb_cohesion_quint lonely)

foreach v of varlist $categorical_vars {
	collect, tag(variable[`v']): svy, subpop(complete_case): tab `v', ci percent
}

collect layout (variable#colname) (result[_r_b _r_ci])



collect export "Analytic Sample Weighted Proportions.docx", replace

collect clear
capture: vl drop continuous_vars
vl create continuous_vars = (age msoa_air msoa_green msoa_retail)

foreach v of varlist $continuous_vars {
	collect, tag(variable[`v']): svy, subpop(complete_case): mean `v'
}


collect layout (variable) (result[_r_b _r_ci])
collect export "Analytic Sample Weighted Means.docx", replace



