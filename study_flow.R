
#set the source and output so you can read the files from any computer
library(dplyr)
library(car)
library(stats)
library(ggplot2)
library(scales)
library(arm)
library(stats)


library(tidyverse)



###### Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/revisions_frontiers/added_education_cov/"
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"



###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_comparison_weight.R", sep=""))


###### sourcing the analysis for adjusted models 
source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison_weight.R", sep=""))


###### sourcing the analysis for cross-national comparison stratified by SES
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison.R", sep=""))

###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison_weight.R", sep=""))


##### sourcing the analysis for cross-naitonal comparison stritified by SES (adjusted models)
source(paste(SOURCE_ROOT, "SES_adjusted_cros_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_adjusted_cros_nat_comparison_weight.R", sep=""))



###### within country comparison of low SES to high SES 
source(paste(SOURCE_ROOT, "Wcountry_unadjusted_SES.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Wcountry_unadjusted_SES_weight.R", sep=""))


source(paste(SOURCE_ROOT, "Wcountry_adjusted_SES.R", sep=""))
source(paste(SOURCE_ROOT, "Wcountry_adjusted_SES_weight.R", sep=""))


###### sourcing code for the anlaysis of SES x country interaction 
source(paste(SOURCE_ROOT, "SESxCountry_interaction.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SESxCountry_interaction_weight.R", sep=""))


########### source file that runs analysis for cross-national comparison between England and USA in numbers of people who experienced discrimination in each of the five situations (eg, less respect), 
# the analysis is restricted to the relevent subset and those who said that they experienced discriminaiton attributed to a type (eg., sex discrim)

source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_Situations.R", sep=""))
##### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_Situations_weight.R", sep=""))


source(paste(SOURCE_ROOT, "Adjusted_cross_nat_Situations.R", sep=""))
source(paste(SOURCE_ROOT, "Adjusted_cross_nat_Situations_weight.R", sep=""))





###### read data files for ELSA and HRS
ELSAdiscrimination_data_wave5_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep=""))
HRS2010_discrimination_dataset_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv", sep=""))

nrow(HRS2010_discrimination_dataset_before_subsetting)

###### subset HRS and ELSA dataset to those who are 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 
HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)

###### subet to those who responded to the discrimination items: 
ELSAdiscrimination_data_wave5_before_subsetting = subset(ELSAdiscrimination_data_wave5_age50, ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 0 | ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 1)

###### drop rows where the responses across all situations are all NAs in the HRS study 
HRS2010_discrimination_dataset_before_subsetting = subset(HRS2010_discrimination_dataset_age50,HRS2010_discrimination_dataset_age50$HRS2010_discrim_lessrespect!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_harassed!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_poorerservice!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_notclever!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_medical!= "NA")