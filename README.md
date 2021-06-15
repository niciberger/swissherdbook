# Data preparation for a prognostic management tool

Data preparation for a prognostic management tool for the prediction of artificial insemination success in dairy cows. The five R scripts are used to prepare the necessary dataset. The data is be selected, cleaned, features are generated and the data is standardized with these skripts. The scripts use standardized data exports from ArgusQ (QualitasAG) according the RindviehCH data interface. Hence, all necessary source data must be exported from ArgusQ first.

## Explanations of the R scripts

* 1_MBA_Datenbereitstellung_MLP&ZW.R:

Milk Recording Data and Breeding values are selected, cleaned, features are generated and the data is standardized.

* 2_MBA_Datenbereitstelung_FBK&GAL.R:

Fertility data is selected, cleaned, features are generated and the data is standardized.

* 3_MBA_Datenbereitstellung_Besamungen.R:

Insemination data is selected, cleaned, features are generated and the data is standardized.

* 4_MBA_JoinBesamungnMP.R:

With this script the previous milk sample is searched for each insemination. This is done with a programming loop, for performance reasons no more than 100'000 insemaintons records should be processed at once. 

* 5_MBA_Zusammfügen&Balancieren.R:
*
Finally, the data is merged and balanced.

This dataset can then be further used to generate a ML model to predict artificial insemination success, for example with the Modulos Auto ML Platofrm. 
