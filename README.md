## Neighbourhood Cohesion and Multimorbidity

This repository contains the code for the research paper _Neighbourhood social cohesion, loneliness and multimorbidity: evidence from a UK longitudinal panel study_, published in Health & Place:

> Rowley-Abel et al., _Neighbourhood social cohesion, loneliness and multimorbidity: Evidence from a UK longitudinal panel study_, Health & Place, Volume 91, 2025, https://doi.org/10.1016/j.healthplace.2025.103414.

This study models the relationship between multimorbidity risk and neighbourhood cohesion. Additionally, it considers how this relationship may be linked to individual loneliness. It conducts both cross-sectional and longitudinal analysis using a large panel study from the UK - [the Understanding Society dataset](https://www.understandingsociety.ac.uk/).

The non-geographic data used in this project can be downloaded from the UK Data Service ([Series ID 2000053](https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000053)). Within the Understanding Society series, the dataset used is _Understanding Society: Waves 1-12, 2009-2021 and Harmonised BHPS: Waves 1-18, 1991-2009_ (Study number: 6614, [DOI: 10.5255/UKDA-SN-6614-18](https://doi.org/10.5255/UKDA-SN-6614-18)). The data was linked to neighbourhood-level variables using census area identifiers (LSOAs), available in a Special License version of the data: _Understanding Society: Waves 1-12, 2009-2021: Special Licence Access, Census 2011 Lower Layer Super Output Areas_ (Study number: 7248, [DOI: 10.5255/UKDA-SN-7248-12](https://doi.org/10.5255/UKDA-SN-7248-12))

Data preparation and bivariate analysis is conducted in R, while multivariate models are conducted in Stata in order to run multilevel models while accounting for the weighting and clustered survey design of Understanding Society.

### Workflow:

#### Setup:
- Download the main Understanding Society dataset (Study number 6614), extract the files and save the folder called _6614_ under Data > Understanding Society Data > Raw Data
- Download the Special License Understanding Society dataset with geogrpahic indicators (Study Number 7248), extract the files and save the folder called _7248_ under Data > Understanding Society Data > Raw Data

#### Cross-Sectional Analysis:
- Data Preparation:
  - [Create R data files for Waves 1, 9 and 10](./Cross-sectional%20Analysis/Data%20Preparation/creating_Rda_files_for_waves_1_9_10.R)
  - [Join indresp and indall files](./Cross-sectional%20Analysis/Data%20Preparation/joining_indresp_and_indall_files.R)
  - [Create outcome variables](./Cross-sectional%20Analysis/Data%20Preparation/creating_outcome_variables.R)
  - [Create independent variables](./Cross-sectional%20Analysis/Data%20Preparation/creating_independent_variables.R)
  - [Create multilevel weights](./Cross-sectional%20Analysis/Data%20Preparation/creating_multilevel_weights.R)
- Main Analysis:
  - [Run Missing Case Analysis](./Cross-sectional%20Analysis/Data%20Preparation/Missing%20Case%20Analysis.do)
  - [Run Cross-sectional Multilevel Models](./Cross-sectional%20Analysis/Analysis/Two-Level%20Multilevel%20Models.do)
- Sensitivity Analysis:
  - [Run Cross-sectional Multilevel Models (Three-Level Version)](./Cross-sectional%20Analysis/Analysis/Three-Level%20Multilevel%20Models.do)
  - [Run Further Sensitivity Analysis](./Cross-sectional%20Analysis/Analysis/Further%20Sensitivity%20Analysis.do)

#### Longitudinal Analysis:
- Data Preparation:
  - [Create follow-up outcome variables](./Longitudinal%20Analysis/Data%20Preparation/creating_wave_10_outcome_variables.R)
  - [Create baseline health variables](./Longitudinal%20Analysis/Data%20Preparation/creating_baseline_health_variables.R)
  - [Create baseline independent variables](./Longitudinal%20Analysis/Data%20Preparation/creating_baseline_independent_variables.R)
  - [Join baseline variables to follow-up outcomes](./Longitudinal%20Analysis/Data%20Preparation/joining_baseline_and_outcome_data.R)
- Analysis:
  - [Longitudinal Multilevel Model](./Longitudinal%20Analysis/Analysis/Longitudinal%20Model%20MSOA-Version.do)
  - [Longitudinal Neighbourhood Move Analysis](./Longitudinal%20Analysis/Analysis/Longitudinal%20Neighbourhood%20Move%20Analysis.do)
