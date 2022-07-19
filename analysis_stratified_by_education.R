
#set the source and output so you can read the files from any computer
library(dplyr)
library(car)
library(stats)
library(ggplot2)
library(scales)
library(arm)
library(stats)
library(epitools)
library(epiDisplay)
library(tidyverse)



###### Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/revisions_frontiers/running_previously_submited-result/SES_as_education_level/"
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"




###### sourcing the analysis for cross-national comparison stratified by SES
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison.R", sep=""))

###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison_weight.R", sep=""))


##### sourcing the analysis for cross-naitonal comparison stritified by SES (adjusted models)
source(paste(SOURCE_ROOT, "SES_adjusted_cros_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_adjusted_cros_nat_comparison_weight.R", sep=""))





###### read data files for ELSA and HRS
ELSAdiscrimination_data_wave5_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep=""))
HRS2010_discrimination_dataset_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv", sep=""))



###### subset HRS and ELSA dataset to those who are 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 
HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)

###### subet to those who responded to the discrimination items: 
ELSAdiscrimination_data_wave5_before_subsetting = subset(ELSAdiscrimination_data_wave5_age50, ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 0 | ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 1)

###### drop rows where the responses across all situations are all NAs in the HRS study 
HRS2010_discrimination_dataset_before_subsetting = subset(HRS2010_discrimination_dataset_age50,HRS2010_discrimination_dataset_age50$HRS2010_discrim_lessrespect!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_harassed!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_poorerservice!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_notclever!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_medical!= "NA")
ELSAdiscrimination_data_wave5_before_subsetting$w5age = as.integer(ELSAdiscrimination_data_wave5_before_subsetting$w5age)



######  dummy code the countries 
ELSAdiscrimination_data_wave5_before_subsetting$country = rep(1, times = nrow(ELSAdiscrimination_data_wave5_before_subsetting))
HRS2010_discrimination_dataset_before_subsetting$country = rep(0, times = nrow(HRS2010_discrimination_dataset_before_subsetting))



###### to be able to subset to low ses and high ses creaate new var: 

ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds = ELSAdiscrimination_data_wave5_before_subsetting$w5wealth
#median in ELSA: £239600  OR $324211.1 

median(ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds, na.rm = TRUE)

######### for the plots and ajusted analysis convert wealth to dollars, however when creating binary SES var it is irrelevent as it is a relative wealth within the sample that matters for that analysis 
ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds * 1.353135
median(ELSAdiscrimination_data_wave5_before_subsetting$wealth, na.rm = TRUE)


HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010

########################################################

ELSAdiscrimination_data_wave5_before_subsetting$medianWealth_ELSA = median(ELSAdiscrimination_data_wave5_before_subsetting$w5wealth)
#ELSA median: 652000
#creating a new binary variable: 
ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA = case_when(ELSAdiscrimination_data_wave5_before_subsetting$w5wealth >= 652000 ~'2',
                                                                                   ELSAdiscrimination_data_wave5_before_subsetting$w5wealth < 652000 ~ '1')

#average national wealth excluding pension for age >52 for year 2021: 410100
#average national wealth for the entire nation median = 238,500

HRS2010_discrimination_dataset_before_subsetting$medianWealth_HRS = median(HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010)
#HRS median: 787500
#creating a new binary variable: 
HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS = case_when(HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010 >=787500 ~ '2', 
                                                                                   HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010 < 787500 ~ '1')



#rename the covariates so the names are consistant between ELSA and HRS
# the new names are: 
#age 
#sex 
#education 
#employment 
#wealth 
#wealth_quantiles
#married 
#marital_statusELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age

ELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age
HRS2010_discrimination_dataset_before_subsetting$age  = HRS2010_discrimination_dataset_before_subsetting$continious_age


ELSAdiscrimination_data_wave5_before_subsetting$sex = ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0
HRS2010_discrimination_dataset_before_subsetting$sex = HRS2010_discrimination_dataset_before_subsetting$sex_1_0

ELSAdiscrimination_data_wave5_before_subsetting$education = ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Education
HRS2010_discrimination_dataset_before_subsetting$education = HRS2010_discrimination_dataset_before_subsetting$education_levels

unique(ELSAdiscrimination_data_wave5_before_subsetting$education)
unique(HRS2010_discrimination_dataset_before_subsetting$education)

HRS2010_discrimination_dataset_before_subsetting$education = case_when(HRS2010_discrimination_dataset_before_subsetting$education == 0 ~ 0, 
                                                                       HRS2010_discrimination_dataset_before_subsetting$education == 1 ~ 1, 
                                                                       HRS2010_discrimination_dataset_before_subsetting$education == 2 ~ 2,
                                                                       HRS2010_discrimination_dataset_before_subsetting$education == 3 ~ 3)

ELSAdiscrimination_data_wave5_before_subsetting$employment = ELSAdiscrimination_data_wave5_before_subsetting$employment
HRS2010_discrimination_dataset_before_subsetting$employment =  HRS2010_discrimination_dataset_before_subsetting$employment_allCategories


ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$wealth
HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth

ELSA_wealth = (ELSAdiscrimination_data_wave5_before_subsetting$wealth)
sd(ELSA_wealth)

sd(HRS2010_discrimination_dataset_before_subsetting$wealth)

ELSAdiscrimination_data_wave5_before_subsetting$wealth_quantiles = ELSAdiscrimination_data_wave5_before_subsetting$w5wealthq
HRS2010_discrimination_dataset_before_subsetting$wealth_quantiles =  HRS2010_discrimination_dataset_before_subsetting$Percentile_wealth_noIRA_HRS2010

ELSAdiscrimination_data_wave5_before_subsetting$married = ELSAdiscrimination_data_wave5_before_subsetting$w5married
HRS2010_discrimination_dataset_before_subsetting$married =  HRS2010_discrimination_dataset_before_subsetting$married_bin

#covariates pooled from ELSA and HRS  (make sure the order as above)
#Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
ELSAdiscrimination_data_wave5_before_subsetting$marital_status = ELSAdiscrimination_data_wave5_before_subsetting$w5married4
HRS2010_discrimination_dataset_before_subsetting$marital_status = HRS2010_discrimination_dataset_before_subsetting$marital_status




###########################
#ELSA: 0 = no education, 1 = GCSE or below o-levels, 2 = A-Levels, o-Levels, or equivalent, 3 = higher degree (undergraduate and postgraduate; those with higher education below a degree through to higher degrees). 
#HRS:  0 = no formal education, 1 = less than high school, 2 = high school completed, 3 = college and above (college graduate, postgraduate, other graduate degree)


unique(ELSAdiscrimination_data_wave5_before_subsetting$education)
unique(HRS2010_discrimination_dataset_before_subsetting$education)

ELSAdiscrimination_data_wave5_before_subsetting$SES = case_when(ELSAdiscrimination_data_wave5_before_subsetting$education == '3' ~ '2',
                                                                ELSAdiscrimination_data_wave5_before_subsetting$education == '2' ~ '1',
                                                                ELSAdiscrimination_data_wave5_before_subsetting$education == '1' ~ '1',
                                                                ELSAdiscrimination_data_wave5_before_subsetting$education == '0' ~ '1')


HRS2010_discrimination_dataset_before_subsetting$SES = case_when(HRS2010_discrimination_dataset_before_subsetting$education == '3' ~ '2',
                                                                 HRS2010_discrimination_dataset_before_subsetting$education == '2' ~ '1',
                                                                 HRS2010_discrimination_dataset_before_subsetting$education == '1' ~ '1',
                                                                 HRS2010_discrimination_dataset_before_subsetting$education == '0' ~ '1')





#adjusted model for disability, subsettign further to those who have a physical lim. 

SES_unadjusted_results = data.frame()

# SES_unadjusted_cross_nat_disability_results = c(NA, 
#                                                NA,
#                                                NA, 
#                                                NA,
#                                                NA,
#                                                NA, 
#                                                NA, 
#                                                NA, 
#                                                NA, 
#                                                NA,
#                                                NA)

# SES_unadjusted_cross_nat_disability_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                   
#                                                                                   
#                                                                                   analysis_variable_name = "disability_discrimination",
#                                                                                   
#                                                                                   subsetting_VAR1_ELSA = "w5limill", 
#                                                                                   
#                                                                                   subsetting_VAR1_HRS = "limiting_condition_bin",
#                                                                                   
#                                                                                   
#                                                                                   SES_level = 1, 
#                                                                                   
#                                                                                   #has physical limitation 
#                                                                                   ELSA_var1_value = 1, 
#                                                                                   HRS_var1_value = 1, 
#                                                                                   
#                                                                                   
#                                                                                   subsetting_VAR2_ELSA =  "NA",  
#                                                                                   subsetting_VAR2_HRS =   "NA", 
#                                                                                   
#                                                                                   
#                                                                                   ELSA_var2_value = "NA", 
#                                                                                   HRS_var2_value = "NA", 
#                                                                                   
#                                                                                   discrimination_VAR_elsa = "w5disabilitydiscrimination2",
#                                                                                   discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

#SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_disability_results) 

# SEShigh_unadjusted_cross_nat_disability_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                       
#                                                                                       
#                                                                                       analysis_variable_name = "disability",
#                                                                                       
#                                                                                       subsetting_VAR1_ELSA = "w5limill", 
#                                                                                       subsetting_VAR1_HRS = "limiting_condition_bin",
#                                                                                       
#                                                                                       
#                                                                                        
#                                                                                       SES_level = 2,
# 
#                                                                                       #has physical limitation
#                                                                                       ELSA_var1_value = 1,
#                                                                                       HRS_var1_value = 1,
# 
# 
#                                                                                       subsetting_VAR2_ELSA =  "NA",
#                                                                                       subsetting_VAR2_HRS =   "NA",
# 
# 
#                                                                                       ELSA_var2_value = "NA",
#                                                                                       HRS_var2_value = "NA",
# 
#                                                                                       discrimination_VAR_elsa = "w5disabilitydiscrimination2",
#                                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")



#SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_disability_results)


SES_unadjusted_cross_nat_financial_all_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                     
                                                                                     
                                                                                     analysis_variable_name = "financial_all",
                                                                                     
                                                                                     SES_level = 1,
                                                                                     
                                                                                     subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                     subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                     
                                                                                     
                                                                                     ELSA_var1_value = 1,
                                                                                     HRS_var1_value = 1,
                                                                                     
                                                                                     
                                                                                     subsetting_VAR2_ELSA =  "NA",  
                                                                                     subsetting_VAR2_HRS =   "NA", 
                                                                                     
                                                                                     
                                                                                     ELSA_var2_value = "NA", 
                                                                                     HRS_var2_value = "NA", 
                                                                                     
                                                                                     discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_financial_all_results) 



SEShigh_unadjusted_cross_nat_financial_all_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                         
                                                                                         
                                                                                         analysis_variable_name = "financial_all",
                                                                                         
                                                                                         
                                                                                         
                                                                                         SES_level = 2, 
                                                                                         
                                                                                         subsetting_VAR1_ELSA = "NA", 
                                                                                         subsetting_VAR1_HRS = "NA",
                                                                                         
                                                                                         ELSA_var1_value = "NA",
                                                                                         HRS_var1_value = "NA", 
                                                                                         
                                                                                         subsetting_VAR2_ELSA =  "NA",  
                                                                                         subsetting_VAR2_HRS =   "NA", 
                                                                                         
                                                                                         
                                                                                         ELSA_var2_value = "NA", 
                                                                                         HRS_var2_value = "NA", 
                                                                                         
                                                                                         discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_all_results) 

# 
# SES_unadjusted_cross_nat_financial_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                         
#                                                                                         
#                                                                                         analysis_variable_name = "financial_female",
#                                                                                         
#                                                                                         
#                                                                                         SES_level = 1, 
#                                                                                         
#                                                                                         subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
#                                                                                         subsetting_VAR1_HRS = "median_wealth_bin_HRS",
#                                                                                         
#                                                                                   
#                                                                                 
#                                                                                         
#                                                                                         ELSA_var1_value = 1, 
#                                                                                         HRS_var1_value = 1,
#                                                                                         
#                                                                                         
#                                                                                         subsetting_VAR2_ELSA = "sex",
#                                                                                         subsetting_VAR2_HRS = "sex",
#                                                                                         
#                                                                                         
#                                                                                         ELSA_var2_value = 0, 
#                                                                                         HRS_var2_value = 0,
#                                                                                         
#                                                                                         discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# 
# SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_financial_female_results) 
# 


# SEShigh_unadjusted_cross_nat_financial_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                             
#                                                                                             
#                                                                                             analysis_variable_name = "financial_female",
#                                                                                             
#                                                                                             
#                                                                                             
#                                                                                             SES_level = 2, 
#                                                                                             
#                                                                                             
#                                                                                             subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
#                                                                                             subsetting_VAR1_HRS = "median_wealth_bin_HRS",
#                                                                                             
#                                                                                             
#                                                                                             
#                                                                                             
#                                                                                             ELSA_var1_value = 1, 
#                                                                                             HRS_var1_value = 1,
#                                                                                             
#                                                                                             
#                                                                                             subsetting_VAR2_ELSA = "sex",
#                                                                                             subsetting_VAR2_HRS = "sex",
#                                                                                             
#                                                                                             
#                                                                                             ELSA_var2_value = 0, 
#                                                                                             HRS_var2_value = 0,
#                                                                                             
#                                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# 
# SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_female_results) 
# 
# 
# 
# 
# SES_unadjusted_cross_nat_financial_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                       
#                                                                                       
#                                                                                       analysis_variable_name = "financial_male",
#                                                                                       
#                                                                                       SES_level = 1, 
#                                                                                       
#                                                                                       subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
#                                                                                       subsetting_VAR1_HRS = "median_wealth_bin_HRS",
#                                                                                       
#                                                                                       
#                                                                                       
#                                                                                       
#                                                                                       ELSA_var1_value = 1, 
#                                                                                       HRS_var1_value = 1,
#                                                                                       
#                                                                                       
#                                                                                       subsetting_VAR2_ELSA = "sex",
#                                                                                       subsetting_VAR2_HRS = "sex",
#                                                                                       
#                                                                                       
#                                                                                       ELSA_var2_value = 1, 
#                                                                                       HRS_var2_value = 1,
#                                                                                       
#                                                                                       discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# 
# SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_financial_male_results) 
# 


# 
# SEShigh_unadjusted_cross_nat_financial_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                           
#                                                                                           
#                                                                                           analysis_variable_name = "financial_male",
#                                                                                           
#                                                                                           SES_level = 2, 
#                                                                                           
#                                                                                           subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
#                                                                                           subsetting_VAR1_HRS = "median_wealth_bin_HRS",
#                                                                                           
#                                                                                           
#                                                                                           
#                                                                                           
#                                                                                           ELSA_var1_value = 1, 
#                                                                                           HRS_var1_value = 1,
#                                                                                           
#                                                                                           
#                                                                                           subsetting_VAR2_ELSA = "sex",
#                                                                                           subsetting_VAR2_HRS = "sex",
#                                                                                           
#                                                                                           
#                                                                                           ELSA_var2_value = 1, 
#                                                                                           HRS_var2_value = 1,
#                                                                                           
#                                                                                           
#                                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_male_results) 


SES_unadjusted_cross_nat_sex_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "sex_discrimination_female",
                                                                                  
                                                                                  SES_level = 1, 
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "sex", 
                                                                                  subsetting_VAR1_HRS = "sex",
                                                                                  
                                                                                  
                                                                                  
                                                                                  
                                                                                  ELSA_var1_value = 0, 
                                                                                  HRS_var1_value = 0, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_sex_female_results) 


SEShigh_unadjusted_cross_nat_sex_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      
                                                                                      
                                                                                      analysis_variable_name = "sex_discrimination_female",
                                                                                      
                                                                                      SES_level = 2, 
                                                                                      
                                                                                      subsetting_VAR1_ELSA = "sex", 
                                                                                      subsetting_VAR1_HRS = "sex",
                                                                                      
                                                                                      
                                                                                      
                                                                                      ELSA_var1_value = 0, 
                                                                                      HRS_var1_value = 0, 
                                                                                      
                                                                                      
                                                                                      subsetting_VAR2_ELSA =  "NA",  
                                                                                      subsetting_VAR2_HRS =   "NA", 
                                                                                      
                                                                                      
                                                                                      ELSA_var2_value = "NA", 
                                                                                      HRS_var2_value = "NA", 
                                                                                      
                                                                                      discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                      discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_sex_female_results) 

# 
# 
# SES_unadjusted_cross_nat_sex_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                 
#                                                                                 
#                                                                                 analysis_variable_name = "sex_discrimination_male",
#                                                                                 
#                                                                                 
#                                                                                 
#                                                                                 SES_level = 1, 
#                                                                                 
#                                                                                 subsetting_VAR1_ELSA = "sex", 
#                                                                                 subsetting_VAR1_HRS = "sex",
#                                                                                 
#                                                                                 
#                                                                                 ELSA_var1_value = 1, 
#                                                                                 HRS_var1_value = 1, 
#                                                                                 
#                                                                                 
#                                                                                 subsetting_VAR2_ELSA =  "NA",  
#                                                                                 subsetting_VAR2_HRS =   "NA", 
#                                                                                 
#                                                                                 
#                                                                                 ELSA_var2_value = "NA", 
#                                                                                 HRS_var2_value = "NA", 
#                                                                                 
#                                                                                 discrimination_VAR_elsa = "w5sexdiscrimination2",
#                                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")
# 
# SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_sex_male_results) 
# 
# 
# SEShigh_unadjusted_cross_nat_sex_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                     
#                                                                                     
#                                                                                     analysis_variable_name = "sex_discrimination_male",
#                                                                                     
#                                                                                     
#                                                                                     
#                                                                                     SES_level = 2, 
#                                                                                     
#                                                                                     subsetting_VAR1_ELSA = "sex", 
#                                                                                     subsetting_VAR1_HRS = "sex",
#                                                                                     
#                                                                                     
#                                                                                     ELSA_var1_value = 1, 
#                                                                                     HRS_var1_value = 1, 
#                                                                                     
#                                                                                     
#                                                                                     subsetting_VAR2_ELSA =  "NA",  
#                                                                                     subsetting_VAR2_HRS =   "NA", 
#                                                                                     
#                                                                                     
#                                                                                     ELSA_var2_value = "NA", 
#                                                                                     HRS_var2_value = "NA", 
#                                                                                     
#                                                                                     discrimination_VAR_elsa = "w5sexdiscrimination2",
#                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")
# 
# SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_sex_male_results) 



SES_unadjusted_cross_nat_race_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                            data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                            
                                                                            
                                                                            analysis_variable_name = "race",
                                                                            
                                                                            
                                                                            SES_level = 1, 
                                                                            
                                                                            subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                            subsetting_VAR1_HRS =   "HRS2010_race_nonwhite",  
                                                                            
                                                                            
                                                                            
                                                                            
                                                                            
                                                                            ELSA_var1_value = 2, 
                                                                            HRS_var1_value = 1, 
                                                                            
                                                                            
                                                                            subsetting_VAR2_ELSA =  "NA",  
                                                                            subsetting_VAR2_HRS =   "NA", 
                                                                            
                                                                            
                                                                            ELSA_var2_value = "NA", 
                                                                            HRS_var2_value = "NA", 
                                                                            
                                                                            discrimination_VAR_elsa = "w5racediscrimination2",
                                                                            discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_race_results) 


SEShigh_unadjusted_cross_nat_race_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                
                                                                                analysis_variable_name = "race",
                                                                                
                                                                                
                                                                                SES_level = 2, 
                                                                                
                                                                                subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                                subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                                
                                                                                
                                                                                
                                                                                ELSA_var1_value = 2, 
                                                                                HRS_var1_value = 1, 
                                                                                
                                                                                
                                                                                subsetting_VAR2_ELSA =  "NA",  
                                                                                subsetting_VAR2_HRS =   "NA", 
                                                                                
                                                                                
                                                                                ELSA_var2_value = "NA", 
                                                                                HRS_var2_value = "NA", 
                                                                                
                                                                                discrimination_VAR_elsa = "w5racediscrimination2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
SES_unadjusted_cross_nat_sexuality_results = c(NA, 
                                               NA,
                                               NA, 
                                               NA,
                                               NA,
                                               NA, 
                                               NA, 
                                               NA, 
                                               NA, 
                                               NA,
                                               NA)


SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_sexuality_results) 


SES_unadjusted_cross_nat_weight_29_9_results = SES_unadjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                          
                                                                                          analysis_variable_name = "weight_discrimination_BMI_29.9",
                                                                                          
                                                                                          SES_level = 1, 
                                                                                          
                                                                                          
                                                                                          subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                          subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                          
                                                                                          
                                                                                          ELSA_var1_value = 29.9, 
                                                                                          HRS_var1_value  = 29.9,  
                                                                                          
                                                                                          subsetting_VAR2_ELSA = "NA", 
                                                                                          subsetting_VAR2_HRS = "NA",
                                                                                          
                                                                                          #low SES
                                                                                          ELSA_var2_value = "NA", 
                                                                                          HRS_var2_value = "NA", 
                                                                                          
                                                                                          
                                                                                          discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                          discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")

SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_weight_29_9_results) 

SEShigh_unadjusted_cross_nat_weight_29_9_results = SES_unadjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                              data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                              
                                                                                              analysis_variable_name = "weight_discrimination_BMI_29.9",
                                                                                              
                                                                                              SES_level = 2, 
                                                                                              
                                                                                              
                                                                                              subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                              subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                              
                                                                                              
                                                                                              ELSA_var1_value = 29.9, 
                                                                                              HRS_var1_value  = 29.9,  
                                                                                              
                                                                                              subsetting_VAR2_ELSA = "NA", 
                                                                                              subsetting_VAR2_HRS = "NA",
                                                                                              
                                                                                              #low SES
                                                                                              ELSA_var2_value = "NA", 
                                                                                              HRS_var2_value = "NA", 
                                                                                              
                                                                                              
                                                                                              discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                              discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_weight_29_9_results) 



SES_unadjusted_cross_nat_weight_25_results = SES_unadjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                        
                                                                                        analysis_variable_name = "weight_discrimination_BMI_25_29.9",
                                                                                        
                                                                                        SES_level = 1, 
                                                                                        
                                                                                        subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                        subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                        
                                                                                        ELSA_var1_value = 25.0, 
                                                                                        HRS_var1_value  = 25.0, 
                                                                                        
                                                                                        
                                                                                        
                                                                                        subsetting_VAR2_ELSA = "w4bmi_clean", 
                                                                                        subsetting_VAR2_HRS = "HRS2010_BMI",
                                                                                        
                                                                                        ELSA_var2_value = 29.9, 
                                                                                        HRS_var2_value = 29.9,  
                                                                                        
                                                                                        
                                                                                        
                                                                                        discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                        discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_weight_25_results) 



SEShigh_unadjusted_cross_nat_weight_25_results = SES_unadjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                            data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                            
                                                                                            analysis_variable_name = "weight_discrimination_BMI>25.0_29.9",
                                                                                            
                                                                                            SES_level = 2, 
                                                                                            
                                                                                            subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                            subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                            
                                                                                            ELSA_var1_value = 25.0, 
                                                                                            HRS_var1_value  = 25.0, 
                                                                                            
                                                                                            
                                                                                            
                                                                                            subsetting_VAR2_ELSA = "w4bmi_clean", 
                                                                                            subsetting_VAR2_HRS = "HRS2010_BMI",
                                                                                            
                                                                                            ELSA_var2_value = 29.9, 
                                                                                            HRS_var2_value = 29.9,  
                                                                                            
                                                                                            
                                                                                            
                                                                                            discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                            discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_weight_25_results) 



SES_unadjusted_cross_nat_weight_both_results = SES_unadjusted_cros_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                         
                                                                                         analysis_variable_name = "weight_discrimination_BMI_25.0",
                                                                                         
                                                                                         SES_level = 1, 
                                                                                         
                                                                                         subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                         subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                         
                                                                                         
                                                                                         ELSA_var1_value = 25.0, 
                                                                                         HRS_var1_value = 25.0,  
                                                                                         
                                                                                         subsetting_VAR2_ELSA = "NA",
                                                                                         subsetting_VAR2_HRS = "NA",
                                                                                         
                                                                                         #low SES
                                                                                         ELSA_var2_value = "NA",
                                                                                         HRS_var2_value  = "NA",
                                                                                         
                                                                                         
                                                                                         
                                                                                         discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_weight_both_results) 




SEShigh_unadjusted_cross_nat_weight_both_results = SES_unadjusted_cros_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                             
                                                                                             analysis_variable_name = "weight_discrimination_BMI>25.0",
                                                                                             
                                                                                             SES_level = 2, 
                                                                                             
                                                                                             subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                             subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                             
                                                                                             
                                                                                             ELSA_var1_value = 25.0, 
                                                                                             HRS_var1_value = 25.0,  
                                                                                             
                                                                                             subsetting_VAR2_ELSA = "NA",
                                                                                             subsetting_VAR2_HRS = "NA",
                                                                                             
                                                                                             #low SES
                                                                                             ELSA_var2_value = "NA",
                                                                                             HRS_var2_value  = "NA",
                                                                                             
                                                                                             
                                                                                             
                                                                                             discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_weight_both_results) 



###########################################################
# SES cross-national comparison (adjusted) 


#adjusted model for disability, subsettign further to those who have a physical lim. 
SES_adjusted_results = data.frame()
# 
# SES_adjusted_cross_nat_disability_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                               
#                                                                               SES_level = 1, 
#                                                                               
#                                                                               
#                                                                               analysis_variable_name = "disability_discrimination",
#                                                                               
#                                                                               subsetting_VAR1_ELSA = "w5limill", 
#                                                                               subsetting_VAR1_HRS = "limiting_condition_bin",
#                                                                               
#                                                                               
#                                                                               
#                                                                               #has physical limitation 
#                                                                               ELSA_var1_value = 1, 
#                                                                               HRS_var1_value = 1, 
#                                                                               
#                                                                               
#                                                                               subsetting_VAR2_ELSA =  "NA",  
#                                                                               subsetting_VAR2_HRS =   "NA", 
#                                                                               
#                                                                               
#                                                                               ELSA_var2_value = "NA", 
#                                                                               HRS_var2_value = "NA", 
#                                                                               
#                                                                               
#                                                                               
#                                                                               covariate1 = "age",
#                                                                               covariate2 = "sex",
#                                                                               covariate3 = "NA",
#                                                                               covariate4 = "NA",
#                                                                               
#                                                                               
#                                                                               discrimination_VAR_elsa = "w5disabilitydiscrimination2",
#                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_disability_results) 
# 
# 
# SEShigh_adjusted_cross_nat_disability_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                   
#                                                                                   SES_level = 2, 
#                                                                                   
#                                                                                   
#                                                                                   analysis_variable_name = "disability_discrimination",
#                                                                                   
#                                                                                   subsetting_VAR1_ELSA = "w5limill", 
#                                                                                   subsetting_VAR1_HRS = "limiting_condition_bin",
#                                                                                   
#                                                                                   
#                                                                                   
#                                                                                   #has physical limitation 
#                                                                                   ELSA_var1_value = 1, 
#                                                                                   HRS_var1_value = 1, 
#                                                                                   
#                                                                                   
#                                                                                   subsetting_VAR2_ELSA =  "NA",  
#                                                                                   subsetting_VAR2_HRS =   "NA", 
#                                                                                   
#                                                                                   
#                                                                                   ELSA_var2_value = "NA", 
#                                                                                   HRS_var2_value = "NA", 
#                                                                                   
#                                                                                   
#                                                                                   
#                                                                                   covariate1 = "age",
#                                                                                   covariate2 = "sex",
#                                                                                   covariate3 = "NA",
#                                                                                   covariate4 = "NA",
#                                                                                   
#                                                                                   
#                                                                                   discrimination_VAR_elsa = "w5disabilitydiscrimination2",
#                                                                                   discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_disability_results) 

#adjusted model for financial discrimination both genders 

SES_adjusted_cross_nat_financial_bothSexes_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                       
                                                                                       analysis_variable_name = "financial_discrimination",
                                                                                       
                                                                                       SES_level = 1,
                                                                                       
                                                                                       subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                       subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                       
                                                                                       ELSA_var1_value = 1, 
                                                                                       HRS_var1_value = 1, 
                                                                                       
                                                                                       
                                                                                       subsetting_VAR2_ELSA =  "NA",  
                                                                                       subsetting_VAR2_HRS =   "NA", 
                                                                                       
                                                                                       
                                                                                       ELSA_var2_value = "NA", 
                                                                                       HRS_var2_value = "NA", 
                                                                                       
                                                                                       covariate1 = "age",
                                                                                       covariate2 = "sex",   
                                                                                       covariate4 = "NA",
                                                                                       covariate3 = "NA",
                                                                                       
                                                                                       
                                                                                       discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_financial_bothSexes_results) 



SEShigh_adjusted_cross_nat_financial_bothSexes_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                           
                                                                                           analysis_variable_name = "financial_discrimination",
                                                                                           
                                                                                           SES_level = 2,
                                                                                           
                                                                                           subsetting_VAR1_ELSA = "NA", 
                                                                                           subsetting_VAR1_HRS = "NA",
                                                                                           
                                                                                           ELSA_var1_value = 2, 
                                                                                           HRS_var1_value = 2, 
                                                                                           
                                                                                           
                                                                                           subsetting_VAR2_ELSA =  "NA",  
                                                                                           subsetting_VAR2_HRS =   "NA", 
                                                                                           
                                                                                           
                                                                                           ELSA_var2_value = "NA", 
                                                                                           HRS_var2_value = "NA", 
                                                                                           
                                                                                           covariate1 = "age",
                                                                                           covariate2 = "sex",  
                                                                                           covariate4 = "NA",
                                                                                           covariate3 = "NA",
                                                                                           
                                                                                           
                                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_financial_bothSexes_results) 


# ##############
# #############
# SES_adjusted_cross_nat_financial_female_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                     
#                                                                                     analysis_variable_name = "financial_discrimination_female",
#                                                                                     
#                                                                                     
#                                                                                     
#                                                                                     SES_level = 1,
#                                                                                     
#                                                                                     
#                                                                                     
#                                                                                     subsetting_VAR1_ELSA =  "sex",  
#                                                                                     subsetting_VAR1_HRS =   "sex", 
#                                                                                     
#                                                                                     
#                                                                                     ELSA_var1_value = 0, 
#                                                                                     HRS_var1_value = 0, 
#                                                                                     
#                                                                                     subsetting_VAR2_ELSA = "NA", 
#                                                                                     subsetting_VAR2_HRS = "NA",
#                                                                                     
#                                                                                     ELSA_var2_value = "NA", 
#                                                                                     HRS_var2_value = "NA",
#                                                                                     
#                                                                                     
#                                                                                     covariate1 = "age",
#                                                                                     covariate2 = "NA",   
#                                                                                     covariate4 = "NA",
#                                                                                     covariate3 = "NA",
#                                                                                     
#                                                                                     
#                                                                                     discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_financial_female_results) 
# 
# 
# SEShigh_adjusted_cross_nat_financial_female_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                         
#                                                                                         analysis_variable_name = "financial_discrimination_female",
#                                                                                         
#                                                                                         
#                                                                                         
#                                                                                         SES_level = 2,
#                                                                                         
#                                                                                         subsetting_VAR1_ELSA =  "sex",  
#                                                                                         subsetting_VAR1_HRS =   "sex", 
#                                                                                         
#                                                                                         
#                                                                                         ELSA_var1_value = 0, 
#                                                                                         HRS_var1_value = 0, 
#                                                                                         
#                                                                                         subsetting_VAR2_ELSA = "NA", 
#                                                                                         subsetting_VAR2_HRS = "NA",
#                                                                                         
#                                                                                         ELSA_var2_value = "NA", 
#                                                                                         HRS_var2_value = "NA",
#                                                                                         
#                                                                                         
#                                                                                         covariate1 = "age",
#                                                                                         covariate2 = "NA",    
#                                                                                         covariate4 = "NA",
#                                                                                         covariate3 = "NA",
#                                                                                         
#                                                                                         
#                                                                                         discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_financial_female_results) 

# #####
# SES_adjusted_cross_nat_financial_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                   
#                                                                                   analysis_variable_name = "financial_discrimination_male",
#                                                                                   
#                                                                                   SES_level = 1, 
#                                                                                   
#                                                                                   
#                                                                                   subsetting_VAR1_ELSA =  "sex",  
#                                                                                   subsetting_VAR1_HRS =   "sex", 
#                                                                                   
#                                                                                   
#                                                                                   ELSA_var1_value = 1, 
#                                                                                   HRS_var1_value = 1, 
#                                                                                   
#                                                                                   subsetting_VAR2_ELSA = "NA", 
#                                                                                   subsetting_VAR2_HRS = "NA",
#                                                                                   
#                                                                                   ELSA_var2_value = "NA", 
#                                                                                   HRS_var2_value = "NA",
#                                                                                   
#                                                                                   covariate1 = "age",
#                                                                                   covariate2 = "NA",    
#                                                                                   covariate4 = "NA",
#                                                                                   covariate3 = "NA",
#                                                                                   
#                                                                                   
#                                                                                   discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                   discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_financial_male_results) 
# 
# 
# SEShigh_adjusted_cross_nat_financial_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                       
#                                                                                       analysis_variable_name = "financial_discrimination_male",
#                                                                                       
#                                                                                       SES_level = 2, 
#                                                                                       
#                                                                                       subsetting_VAR1_ELSA =  "sex",  
#                                                                                       subsetting_VAR1_HRS =   "sex", 
#                                                                                       
#                                                                                       
#                                                                                       ELSA_var1_value = 1, 
#                                                                                       HRS_var1_value = 1, 
#                                                                                       
#                                                                                       subsetting_VAR2_ELSA = "NA", 
#                                                                                       subsetting_VAR2_HRS = "NA",
#                                                                                       
#                                                                                       ELSA_var2_value = "NA", 
#                                                                                       HRS_var2_value = "NA",
#                                                                                       
#                                                                                       
#                                                                                       covariate1 = "age",
#                                                                                       covariate2 = "NA",    
#                                                                                       covariate4 = "NA",
#                                                                                       covariate3 = "NA",
#                                                                                       
#                                                                                       
#                                                                                       discrimination_VAR_elsa = "w5discrim_financial2",
#                                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_financial_male_results) 
#####
SES_adjusted_cross_nat_sex_female_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                              data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                              
                                                                              analysis_variable_name = "sex_discrimination_female",
                                                                              
                                                                              SES_level = 1, 
                                                                              
                                                                              
                                                                              subsetting_VAR1_ELSA =  "sex",  
                                                                              subsetting_VAR1_HRS =   "sex", 
                                                                              
                                                                              
                                                                              ELSA_var1_value = 0, 
                                                                              HRS_var1_value = 0, 
                                                                              
                                                                              subsetting_VAR2_ELSA = "NA", 
                                                                              subsetting_VAR2_HRS = "NA",
                                                                              
                                                                              #low SES
                                                                              ELSA_var2_value = "NA", 
                                                                              HRS_var2_value = "NA", 
                                                                              
                                                                              
                                                                              covariate1 = "age",
                                                                              covariate2 = "NA",
                                                                              covariate3 = "NA",
                                                                              covariate4 = "NA", 
                                                                              
                                                                              
                                                                              discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                              discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")



SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_sex_female_results) 


SEShigh_adjusted_cross_nat_sex_female_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  analysis_variable_name = "sex_discrimination_female",
                                                                                  
                                                                                  SES_level = 2, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR1_ELSA =  "sex",  
                                                                                  subsetting_VAR1_HRS =   "sex", 
                                                                                  
                                                                                  
                                                                                  ELSA_var1_value = 0, 
                                                                                  HRS_var1_value = 0, 
                                                                                  
                                                                                  subsetting_VAR2_ELSA = "NA", 
                                                                                  subsetting_VAR2_HRS = "NA",
                                                                                  
                                                                                  #low SES
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  
                                                                                  covariate1 = "age",
                                                                                  covariate2 = "NA",
                                                                                  covariate3 = "NA",
                                                                                  covariate4 = "NA", 
                                                                                  
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")



SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_sex_female_results) 

# #####
# SES_adjusted_cross_nat_sex_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                             
#                                                                             analysis_variable_name = "sex_discrimination_male",
#                                                                             
#                                                                             
#                                                                             SES_level = 1, 
#                                                                             
#                                                                             
#                                                                             subsetting_VAR1_ELSA =  "sex",  
#                                                                             subsetting_VAR1_HRS =   "sex", 
#                                                                             
#                                                                             
#                                                                             ELSA_var1_value = 1, 
#                                                                             HRS_var1_value = 1, 
#                                                                             
#                                                                             subsetting_VAR2_ELSA = "NA", 
#                                                                             subsetting_VAR2_HRS = "NA",
#                                                                             
#                                                                             #low SES
#                                                                             ELSA_var2_value = "NA", 
#                                                                             HRS_var2_value = "NA", 
#                                                                             
#                                                                             
#                                                                             covariate1 = "age",
#                                                                             covariate2 = "NA",
#                                                                             covariate3 = "NA",
#                                                                             covariate4 = "NA", 
#                                                                             
#                                                                             
#                                                                             discrimination_VAR_elsa = "w5sexdiscrimination2",
#                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_sex_male_results) 
# 
# 
# 
# SEShigh_adjusted_cross_nat_sex_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
#                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
#                                                                                 
#                                                                                 analysis_variable_name = "sex_discrimination_male",
#                                                                                 
#                                                                                 
#                                                                                 SES_level = 2, 
#                                                                                 
#                                                                                 
#                                                                                 subsetting_VAR1_ELSA =  "sex",  
#                                                                                 subsetting_VAR1_HRS =   "sex", 
#                                                                                 
#                                                                                 
#                                                                                 ELSA_var1_value = 1, 
#                                                                                 HRS_var1_value = 1, 
#                                                                                 
#                                                                                 subsetting_VAR2_ELSA = "NA", 
#                                                                                 subsetting_VAR2_HRS = "NA",
#                                                                                 
#                                                                                 #low SES
#                                                                                 ELSA_var2_value = "NA", 
#                                                                                 HRS_var2_value = "NA", 
#                                                                                 
#                                                                                 
#                                                                                 covariate1 = "age",
#                                                                                 covariate2 = "NA",
#                                                                                 covariate3 = "NA",
#                                                                                 covariate4 = "NA", 
#                                                                                 
#                                                                                 
#                                                                                 discrimination_VAR_elsa = "w5sexdiscrimination2",
#                                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")
# 
# SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_sex_male_results) 
# 

#####
SES_adjusted_cross_nat_race_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                        
                                                                        analysis_variable_name = "race_discrimination",
                                                                        
                                                                        SES_level = 1, 
                                                                        
                                                                        
                                                                        subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                        subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                        
                                                                        
                                                                        ELSA_var1_value = 2, 
                                                                        HRS_var1_value = 1, 
                                                                        
                                                                        subsetting_VAR2_ELSA = "NA", 
                                                                        subsetting_VAR2_HRS = "NA",
                                                                        
                                                                        #low SES
                                                                        ELSA_var2_value = "NA", 
                                                                        HRS_var2_value = "NA", 
                                                                        
                                                                        
                                                                        covariate1 = "age",
                                                                        covariate2 = "sex",
                                                                        covariate3 = "NA",
                                                                        covariate4 = "NA", 
                                                                        
                                                                        
                                                                        discrimination_VAR_elsa = "w5racediscrimination2",
                                                                        discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")


SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_race_results) 


SEShigh_adjusted_cross_nat_race_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                            data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                            
                                                                            analysis_variable_name = "race_discrimination",
                                                                            
                                                                            SES_level = 2, 
                                                                            
                                                                            
                                                                            subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                            subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                            
                                                                            
                                                                            ELSA_var1_value = 2, 
                                                                            HRS_var1_value = 1, 
                                                                            
                                                                            subsetting_VAR2_ELSA = "NA", 
                                                                            subsetting_VAR2_HRS = "NA",
                                                                            
                                                                            #low SES
                                                                            ELSA_var2_value = "NA", 
                                                                            HRS_var2_value = "NA", 
                                                                            
                                                                            
                                                                            covariate1 = "age",
                                                                            covariate2 = "sex",
                                                                            covariate3 = "NA",
                                                                            covariate4 = "NA", 
                                                                            
                                                                            
                                                                            discrimination_VAR_elsa = "w5racediscrimination2",
                                                                            discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")


SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
adjusted_cross_nat_sexuality_results = c(NA, 
                                         NA,
                                         NA, 
                                         NA,
                                         NA,
                                         NA, 
                                         NA, 
                                         NA, 
                                         NA, 
                                         NA,
                                         NA)


SES_adjusted_results = rbind(SES_adjusted_results, adjusted_cross_nat_sexuality_results) 


SES_adjusted_cross_nat_weight_29_9_results = SES_adjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      
                                                                                      analysis_variable_name = "weight_discrimination_BMImore29.9",
                                                                                      
                                                                                      SES_level = 1, 
                                                                                      
                                                                                      
                                                                                      subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                      subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                      
                                                                                      
                                                                                      ELSA_var1_value = 29.9, 
                                                                                      HRS_var1_value  = 29.9,  
                                                                                      
                                                                                      subsetting_VAR2_ELSA = "NA", 
                                                                                      subsetting_VAR2_HRS = "NA",
                                                                                      
                                                                                      #low SES
                                                                                      ELSA_var2_value = "NA", 
                                                                                      HRS_var2_value = "NA", 
                                                                                      
                                                                                      
                                                                                      covariate1 = "age", 
                                                                                      covariate2 = "sex",     
                                                                                      covariate4 = "NA",
                                                                                      covariate3 = "NA",
                                                                                      
                                                                                      discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                      discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")



SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_weight_29_9_results) 


SEShigh_adjusted_cross_nat_weight_29_9_results = SES_adjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                          
                                                                                          analysis_variable_name = "weight_discrimination_BMImore29.9",
                                                                                          
                                                                                          SES_level = 2, 
                                                                                          
                                                                                          
                                                                                          subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                          subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                          
                                                                                          
                                                                                          ELSA_var1_value = 29.9, 
                                                                                          HRS_var1_value  = 29.9,  
                                                                                          
                                                                                          subsetting_VAR2_ELSA = "NA", 
                                                                                          subsetting_VAR2_HRS = "NA",
                                                                                          
                                                                                          #low SES
                                                                                          ELSA_var2_value = "NA", 
                                                                                          HRS_var2_value = "NA", 
                                                                                          
                                                                                          
                                                                                          covariate1 = "age", 
                                                                                          covariate2 = "sex",     
                                                                                          covariate4 = "NA",
                                                                                          covariate3 = "NA",
                                                                                          
                                                                                          discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                          discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")



SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_weight_29_9_results) 


SES_adjusted_cross_nat_weight_25_results = SES_adjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                    data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                    
                                                                                    analysis_variable_name = "weight_discrimination_BMImore25.0_less29.9",
                                                                                    
                                                                                    
                                                                                    SES_level = 1, 
                                                                                    
                                                                                    
                                                                                    subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                    subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                    
                                                                                    ELSA_var1_value = 25.0, 
                                                                                    HRS_var1_value  = 25.0, 
                                                                                    
                                                                                    
                                                                                    
                                                                                    subsetting_VAR2_ELSA = "w4bmi_clean", 
                                                                                    subsetting_VAR2_HRS = "HRS2010_BMI",
                                                                                    
                                                                                    ELSA_var2_value = 29.9, 
                                                                                    HRS_var2_value = 29.9,  
                                                                                    
                                                                                    covariate1 = "age", 
                                                                                    covariate2 = "sex",     
                                                                                    covariate4 = "NA",
                                                                                    covariate3 = "NA",
                                                                                    
                                                                                    
                                                                                    discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                    discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_weight_25_results) 

SEShigh_adjusted_cross_nat_weight_25_results = SES_adjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                        
                                                                                        analysis_variable_name = "weight_discrimination_BMI>25.0_29.9",
                                                                                        
                                                                                        
                                                                                        SES_level = 2, 
                                                                                        
                                                                                        
                                                                                        subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                        subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                        
                                                                                        ELSA_var1_value = 25.0, 
                                                                                        HRS_var1_value  = 25.0, 
                                                                                        
                                                                                        
                                                                                        
                                                                                        subsetting_VAR2_ELSA = "w4bmi_clean", 
                                                                                        subsetting_VAR2_HRS = "HRS2010_BMI",
                                                                                        
                                                                                        ELSA_var2_value = 29.9, 
                                                                                        HRS_var2_value = 29.9,  
                                                                                        
                                                                                        covariate1 = "age", 
                                                                                        covariate2 = "sex",    
                                                                                        covariate4 = "NA",
                                                                                        covariate3 = "NA",
                                                                                        
                                                                                        discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                        discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_weight_25_results) 


SES_adjusted_cross_nat_weight_both_results = SES_adjusted_cros_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                     
                                                                                     analysis_variable_name = "weight_discrimination_BMI",
                                                                                     
                                                                                     SES_level = 1, 
                                                                                     
                                                                                     
                                                                                     subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                     subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                     
                                                                                     
                                                                                     ELSA_var1_value = 25.0, 
                                                                                     HRS_var1_value = 25.0,  
                                                                                     
                                                                                     subsetting_VAR2_ELSA = "NA",
                                                                                     subsetting_VAR2_HRS = "NA",
                                                                                     
                                                                                     #low SES
                                                                                     ELSA_var2_value = "NA",
                                                                                     HRS_var2_value  = "NA",
                                                                                     
                                                                                     
                                                                                     covariate1 = "age", 
                                                                                     covariate2 = "sex",    
                                                                                     covariate4 = "NA",
                                                                                     covariate3 = "NA",
                                                                                     
                                                                                     discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_weight_both_results) 



SEShigh_adjusted_cross_nat_weight_both_results = SES_adjusted_cros_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                         
                                                                                         analysis_variable_name = "weight_discrimination_BMImore25.0",
                                                                                         
                                                                                         SES_level = 2, 
                                                                                         
                                                                                         
                                                                                         subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                         subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                         
                                                                                         
                                                                                         ELSA_var1_value = 25.0, 
                                                                                         HRS_var1_value = 25.0,  
                                                                                         
                                                                                         subsetting_VAR2_ELSA = "NA",
                                                                                         subsetting_VAR2_HRS = "NA",
                                                                                         
                                                                                         #low SES
                                                                                         ELSA_var2_value = "NA",
                                                                                         HRS_var2_value  = "NA",
                                                                                         
                                                                                         
                                                                                         covariate1 = "age", 
                                                                                         covariate2 = "sex",    
                                                                                         covariate4 = "NA",
                                                                                         covariate3 = "NA",
                                                                                         
                                                                                         discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_weight_both_results) 


SES_stratified_results = cbind(SES_unadjusted_results, SES_adjusted_results) 
#disability_discrimination

write.csv(SES_stratified_results, file = paste(OUTPUT_ROOT, "SESeducation_stratified_result.csv", sep=""))
