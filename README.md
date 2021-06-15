# Data preparation for a prognostic management tool

Data preparation for a prognostic management tool for the prediction of artificial insemination success in dairy cows. The five R scripts are used to prepare the necessary dataset. The data is be selected, cleaned, features are generated and the data is standardized with these skripts. The scripts use standardized data exports from ArgusQ (QualitasAG) according the RindviehCH data interface. Hence, all necessary source data must be exported from ArgusQ first.

## Instructions

The R scripts must be run one after the other.

**1. MBA_Datenbereitstellung_MLP&ZW.R:** Milk Recording Data and Breeding values are selected, cleaned, features are generated and the data is standardized.

**2. MBA_Datenbereitstelung_FBK&GAL.R:** Fertility data is selected, cleaned, features are generated and the data is standardized.

**3. MBA_Datenbereitstellung_Besamungen.R:** Insemination data is selected, cleaned, features are generated and the data is standardized.

**4. MBA_JoinBesamungnMP.R:** With this script the previous milk sample is searched for each insemination. This is done with a programming loop, for performance reasons no more than 100'000 insemaintons records should be processed at once. 

**5. MBA_Zusammfügen&Balancieren.R:** Finally, the data is merged and balanced.

This final dataset can then be further used to generate a ML model to predict artificial insemination success, for example with the Modulos Auto ML Platform. 
