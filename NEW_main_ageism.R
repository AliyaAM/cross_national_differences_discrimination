
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
######  Set the root location on the user's local machine to save output files.s
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/revisions_frontiers/ageism/"
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



###### subset HRS and ELSA dataset to those who are 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 
HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)

###### subet to those who responded to the discrimination items: 
ELSAdiscrimination_data_wave5_before_subsetting = subset(ELSAdiscrimination_data_wave5_age50, ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 0 | ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 1)

###### drop rows where the responses across all situations are all NAs in the HRS study 
HRS2010_discrimination_dataset_before_subsetting = subset(HRS2010_discrimination_dataset_age50,HRS2010_discrimination_dataset_age50$HRS2010_discrim_lessrespect!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_harassed!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_poorerservice!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_notclever!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_medical!= "NA")
ELSAdiscrimination_data_wave5_before_subsetting$w5age = as.integer(ELSAdiscrimination_data_wave5_before_subsetting$w5age)

###### check these are correct answers in the bin var: 0 or 1
Total_N_ELSA = nrow(ELSAdiscrimination_data_wave5_before_subsetting)
mean_age_ELSA = mean(ELSAdiscrimination_data_wave5_before_subsetting$w5age)
sd_age_ELSA = sd(ELSAdiscrimination_data_wave5_before_subsetting$w5age)

N_women_ELSA = nrow(subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0 == 0))

Total_N_HRS = nrow(HRS2010_discrimination_dataset_before_subsetting)
mean_age_HRS = mean(HRS2010_discrimination_dataset_before_subsetting$continious_age)
sd_age_HRS = sd(HRS2010_discrimination_dataset_before_subsetting$continious_age)
N_women_HRS = nrow(subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$sex_1_0 == 0))



#######################


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

HRS2010_discrimination_dataset_before_subsetting$education = as.factor(HRS2010_discrimination_dataset_before_subsetting$education)
ELSAdiscrimination_data_wave5_before_subsetting$education = as.factor(ELSAdiscrimination_data_wave5_before_subsetting$education)


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

unique(ELSAdiscrimination_data_wave5_before_subsetting$w5agediscrimination2)
unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_reason_discrim1_reason_age)

ELSAdiscrimination_data_wave5_before_subsetting$age_discrim =  ELSAdiscrimination_data_wave5_before_subsetting$w5agediscrimination2 
HRS2010_discrimination_dataset_before_subsetting$age_discrim = HRS2010_discrimination_dataset_before_subsetting$HRS2010_reason_discrim1_reason_age

age_discrim_both_cohorts = c(ELSAdiscrimination_data_wave5_before_subsetting$age_discrim,
                HRS2010_discrimination_dataset_before_subsetting$age_discrim) 



######  dummy code the countries 
ELSAdiscrimination_data_wave5_before_subsetting$country = rep(1, times = nrow(ELSAdiscrimination_data_wave5_before_subsetting))
HRS2010_discrimination_dataset_before_subsetting$country = rep(0, times = nrow(HRS2010_discrimination_dataset_before_subsetting))


##########
##########
##########
##########

Unadjusted_cross_nat_ageism_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           analysis_variable_name = "age_discrimination",
                                                                           plot_title_name = "age discrimination",
                                                                           subsetting_VAR1_ELSA = "NA", 
                                                                           subsetting_VAR1_HRS = "NA",
                                                                           #has physical limitation
                                                                           ELSA_var1_value = 1,
                                                                           HRS_var1_value = 1,
                                                                           subsetting_VAR2_ELSA =  "NA",
                                                                           subsetting_VAR2_HRS =   "NA", 
                                                                           ELSA_var2_value = "NA",
                                                                           HRS_var2_value = "NA", 
                                                                           discrimination_VAR_elsa = "age_discrim",
                                                                           discrimination_VAR_hrs = "age_discrim") 


#######
#######
#######


adjusted_cross_nat_ageism_results_no_education_cov = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                    data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                    
                                                                                    
                                                                                    analysis_variable_name = "age_discrimination",

                                                                                    subsetting_VAR1_ELSA = "NA", 
                                                                                    subsetting_VAR1_HRS = "NA",
                                                                                    
                                                                                    
                                                                                    
                                                                                    #has physical limitation 
                                                                                    ELSA_var1_value = 1, 
                                                                                    HRS_var1_value = 1, 
                                                                                    
                                                                                    
                                                                                    subsetting_VAR2_ELSA =  "NA",  
                                                                                    subsetting_VAR2_HRS =   "NA", 
                                                                                    
                                                                                    
                                                                                    ELSA_var2_value = "NA", 
                                                                                    HRS_var2_value = "NA", 
                                                                                    
                                                                                    
                                                                                    
                                                                                    covariate1 = "age",
                                                                                    covariate2 = "sex",
                                                                                    covariate3 = "wealth",
                                                                                    covariate4 = "NA", 
                                                                                    
                                                                                    wealth_gradient_cov1 = "age", 
                                                                                    wealth_gradient_cov2 = "employment",
                                                                                    wealth_gradient_cov3 = "NA", 
                                                                                    
                                                                                    discrimination_VAR_elsa = "w5agediscrimination2",
                                                                                    discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")


#######
#######

Results_cross_nat = cbind(Unadjusted_cross_nat_ageism_results, adjusted_cross_nat_ageism_results_no_education_cov)


write.csv(Results_cross_nat, file = paste(OUTPUT_ROOT, "Results_cross_nat_ageism_basic_adjustment.csv", sep=""))


adjusted_cross_nat_ageism_results_education_cov = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "age_discrimination",

                                                                                  subsetting_VAR1_ELSA = "NA", 
                                                                                  subsetting_VAR1_HRS = "NA",
                                                                                  
                                                                                  
                                                                                  
                                                                                  #has physical limitation 
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  
                                                                                  
                                                                                  covariate1 = "age",
                                                                                  covariate2 = "sex",
                                                                                  covariate3 = "wealth",
                                                                                  covariate4 = "education", 
                                                                                  
                                                                                  wealth_gradient_cov1 = "age", 
                                                                                  wealth_gradient_cov2 = "employment",
                                                                                  wealth_gradient_cov3 = "NA", 
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5agediscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")


write.csv(adjusted_cross_nat_ageism_results_education_cov, file = paste(OUTPUT_ROOT, "adjusted_cross_nat_ageism_results_education_cov.csv", sep=""))


#######
#######
#######

ELSAdiscrimination_data_wave5_before_subsetting$SES = case_when(ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == '2' ~ '2',
                                                                ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == '1' ~ '1')

HRS2010_discrimination_dataset_before_subsetting$SES = case_when(HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS == '2' ~ '2', 
                                                                 HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS == '1' ~ '1')




#adjusted model for disability, subsettign further to those who have a physical lim. 
SES_unadjusted_cross_nat_ageism_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "age_discrimination",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "NA", 
                                                                                  
                                                                                  subsetting_VAR1_HRS = "NA",
                                                                                  
                                                                                  
                                                                                  SES_level = 1, 
                                                                                  
                                                                                  #has physical limitation 
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                              
                                                                              discrimination_VAR_elsa = "w5agediscrimination2",
                                                                              discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")




#######

#adjusted model for disability, subsettign further to those who have a physical lim. 


SEShigh_unadjusted_cross_nat_ageism_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "age_discrimination",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "NA", 
                                                                                  
                                                                                  subsetting_VAR1_HRS = "NA",
                                                                                  
                                                                                  
                                                                                  SES_level = 2, 
                                                                                  
                                                                                  #has physical limitation 
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5agediscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")


###################
###################


SES_stratified_results_unadjusted = rbind(SES_unadjusted_cross_nat_ageism_results, SEShigh_unadjusted_cross_nat_ageism_results)
  
SES_adjusted_cross_nat_ageism_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                              data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                              
                                                                              SES_level = 1, 
                                                                              
                                                                              
                                                                              analysis_variable_name = "age_discrimination",
                                                                              
                                                                              subsetting_VAR1_ELSA = "NA", 
                                                                              subsetting_VAR1_HRS = "NA",
                                                                              
                                                                              
                                                                              
                                                                              #has physical limitation 
                                                                              ELSA_var1_value = 1, 
                                                                              HRS_var1_value = 1, 
                                                                              
                                                                              
                                                                              subsetting_VAR2_ELSA =  "NA",  
                                                                              subsetting_VAR2_HRS =   "NA", 
                                                                              
                                                                              
                                                                              ELSA_var2_value = "NA", 
                                                                              HRS_var2_value = "NA", 
                                                                              
                                                                              
                                                                              
                                                                              covariate1 = "age",
                                                                              covariate2 = "sex",
                                                                              covariate3 = "NA",
                                                                              covariate4 = "NA",
                                                                              
                                                                          discrimination_VAR_elsa = "w5agediscrimination2",
                                                                          discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")



SEShigh_adjusted_cross_nat_ageism_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  SES_level = 2, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "age_discrimination",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "NA", 
                                                                                  subsetting_VAR1_HRS = "NA",
                                                                                  
                                                                                  
                                                                                  
                                                                                  #has physical limitation 
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  
                                                                                  
                                                                                  covariate1 = "age",
                                                                                  covariate2 = "sex",
                                                                                  covariate3 = "NA",
                                                                                  covariate4 = "NA",
                                                                              
                                                                              discrimination_VAR_elsa = "w5agediscrimination2",
                                                                              discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")




SES_stratified_results_adjusted = rbind(SES_adjusted_cross_nat_ageism_results, SEShigh_adjusted_cross_nat_ageism_results) 

SES_stratified_results = cbind(SES_stratified_results_unadjusted, SES_stratified_results_adjusted)

write.csv(SES_stratified_results, file = paste(OUTPUT_ROOT, "SES_stratified_results_ageism.csv", sep=""))


#############
#############



Wcountry_unadjusted_SES_ageism_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                      analysis_variable_name = "age_discrimination",
                                                                      country = "England", 
                                                                      subsetting_VAR1 = "NA", 
                                                                      #has physical limitation
                                                                      var1_value = 1,
                                                                      subsetting_VAR2 =  "NA",
                                                                      var2_value = "NA",
                                                                      discrimination_VAR = "w5agediscrimination2") 


Wcountry_adjusted_SES_ageism_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                  analysis_variable_name = "ageism",
                                                                  country = "England", 
                                                                  subsetting_VAR1 = "NA", 
                                                                  #has physical limitation
                                                                  var1_value = 1,
                                                                  subsetting_VAR2 =  "NA",
                                                                  var2_value = "NA",
                                                                  
                                                                  covariate1 = "age",
                                                                  covariate2 = "sex",
                                                                  covariate3 = "NA",
                                                                  covariate4 = "NA", 
                                                              
                                                                  discrimination_VAR = "w5agediscrimination2") 

Wcountry_results_ELSA = cbind(Wcountry_unadjusted_SES_ageism_results, Wcountry_adjusted_SES_ageism_results)

write.csv(Wcountry_results_ELSA, file = paste(OUTPUT_ROOT, "Wcountry_results_ageism_ELSA.csv", sep=""))


############

################### HRS:  Wcountry_unadjusted_SES

Wcountry_unadjusted_SES_ageism_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         analysis_variable_name = "ageism ",
                                                                         
                                                                         country = "USA", 
                                                                         subsetting_VAR1 = "NA",
                                                                         var1_value = 1,
                                                                         subsetting_VAR2 =   "NA", 
                                                                         var2_value = "NA", 
                                                                         discrimination_VAR = "HRS2010_reason_discrim1_reason_age")




################### HRS:  Wcountry_adjusted_SES


Wcountry_adjusted_SES_ageism_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     analysis_variable_name = "ageism ",
                                                                     
                                                                     country = "USA", 
                                                                     subsetting_VAR1 = "NA",
                                                                     var1_value = 1,
                                                                     subsetting_VAR2 =   "NA", 
                                                                     var2_value = "NA", 
                                                                     
                                                                     covariate1 = "age",
                                                                     covariate2 = "sex",
                                                                     covariate3 = "NA",
                                                                     covariate4 = "NA",
                                                                     
                                                                     discrimination_VAR = "HRS2010_reason_discrim1_reason_age")

Wcountry_results_HRS = cbind(Wcountry_unadjusted_SES_ageism_HRS_results, Wcountry_adjusted_SES_ageism_HRS_results) 

write.csv(Wcountry_results_HRS, file = paste(OUTPUT_ROOT, "Wcountry_results_ageism_HRS.csv", sep=""))

############


SESxCountry_interaction_ageism = SESxCountry_interaction(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting,
                                                             
                                                             analysis_variable_name = "age_discrimination", 
                                                             subsetting_VAR1_ELSA = "NA", 
                                                             subsetting_VAR1_HRS = "NA",
                                                             ELSA_var1_value = 1, 
                                                             HRS_var1_value = 1, 
                                                             subsetting_VAR2_ELSA =  "NA",  
                                                             subsetting_VAR2_HRS =   "NA", 
                                                             ELSA_var2_value = "NA", 
                                                             HRS_var2_value = "NA", 
                                                             
                                                         discrimination_VAR_elsa = "w5agediscrimination2",
                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")


write.csv(SESxCountry_interaction_ageism, file = paste(OUTPUT_ROOT, "SESxCountry_interaction_ageism.csv", sep=""))

#######



Unadjusted_cross_nat_Situations_ageism_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      analysis_variable_name = "age_discrimination",
                                                                                      subsetting_VAR1_ELSA = "NA", 
                                                                                      subsetting_VAR1_HRS = "NA",
                                                                                      #has physical limitation
                                                                                      ELSA_var1_value = 1,
                                                                                      HRS_var1_value = 1,
                                                                                      subsetting_VAR2_ELSA =  "NA",
                                                                                      subsetting_VAR2_HRS =   "NA", 
                                                                                      ELSA_var2_value = "NA",
                                                                                      HRS_var2_value = "NA", 
                                                                                  discrimination_VAR_elsa = "w5agediscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age") 

######
######





Adjusted_cross_nat_Situations_ageism_results_no_education_cov = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 analysis_variable_name = "age_discrimination",
                                                                                                 
                                                                                                 subsetting_VAR1_ELSA = "NA", 
                                                                                                 subsetting_VAR1_HRS = "NA",
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 #has physical limitation 
                                                                                                 ELSA_var1_value = 1, 
                                                                                                 HRS_var1_value = 1, 
                                                                                                 
                                                                                                 
                                                                                                 subsetting_VAR2_ELSA =  "NA",  
                                                                                                 subsetting_VAR2_HRS =   "NA", 
                                                                                                 
                                                                                                 
                                                                                                 ELSA_var2_value = "NA", 
                                                                                                 HRS_var2_value = "NA", 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 covariate1 = "age",
                                                                                                 covariate2 = "sex",
                                                                                                 covariate3 = "wealth",
                                                                                                 covariate4 = "NA",
                                                                                                 
                                                                                                 
                                                                                               discrimination_VAR_elsa = "w5agediscrimination2",
                                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")





cross_nat_Situations_ageism_results = cbind(Unadjusted_cross_nat_Situations_ageism_results, Adjusted_cross_nat_Situations_ageism_results_no_education_cov)

write.csv(cross_nat_Situations_ageism_results, file = paste(OUTPUT_ROOT, "cross_nat_Situations_ageism_results.csv", sep=""))


##############
##############
##############
##############


Adjusted_cross_nat_Situations_ageism_results_with_education_cov = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 analysis_variable_name = "age_discrimination",
                                                                                                 
                                                                                                 subsetting_VAR1_ELSA = "NA", 
                                                                                                 subsetting_VAR1_HRS = "NA",
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 #has physical limitation 
                                                                                                 ELSA_var1_value = 1, 
                                                                                                 HRS_var1_value = 1, 
                                                                                                 
                                                                                                 
                                                                                                 subsetting_VAR2_ELSA =  "NA",  
                                                                                                 subsetting_VAR2_HRS =   "NA", 
                                                                                                 
                                                                                                 
                                                                                                 ELSA_var2_value = "NA", 
                                                                                                 HRS_var2_value = "NA", 
                                                                                                 
                                                                                                 
                                                                                                 
                                                                                                 covariate1 = "age",
                                                                                                 covariate2 = "sex",
                                                                                                 covariate3 = "wealth",
                                                                                                 covariate4 = "education",
                                                                                                 
                                                                                                 
                                                                                                 discrimination_VAR_elsa = "w5agediscrimination2",
                                                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_age")


write.csv(Adjusted_cross_nat_Situations_ageism_results_with_education_cov, file = paste(OUTPUT_ROOT, "Adjusted_cross_nat_Situations_ageism_results_with_education_cov.csv", sep=""))

