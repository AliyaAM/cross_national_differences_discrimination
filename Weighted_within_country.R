

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
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/revisions_frontiers/weighted/"
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

total_sample = cbind(Total_N_ELSA, mean_age_ELSA, sd_age_ELSA, N_women_ELSA,
                     Total_N_HRS, mean_age_HRS, sd_age_HRS, N_women_HRS)

total_sample



write.csv(total_sample, file = paste(OUTPUT_ROOT, "total_sample_characteristics.csv", sep=""))

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



ELSAdiscrimination_data_wave5_before_subsetting$SES = case_when(ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == '2' ~ '2',
                                                                ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == '1' ~ '1')

HRS2010_discrimination_dataset_before_subsetting$SES = case_when(HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS == '2' ~ '2', 
                                                                 HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS == '1' ~ '1')



 
# # #wealth gradient
# # wealth_discrimination =  summary(glm(discrimination ~ wealth, family = "binomial", data = data_both_countries))
# # wealth_gradient = wealth_discrimination$coefficients
# # wealth_gradient = as.data.frame(wealth_gradient)
# #
# #
# # write.csv(wealth_gradient, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination.csv", sep=""))
# #
# # ########
# # ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
# #
# #
# # wealth_discrimination_ELSA =  summary(glm(discrimination ~ wealth, family = "binomial", data = ELSA_subset_wealth_gradient))
# # wealth_gradient_ELSA = wealth_discrimination_ELSA$coefficients
# # wealth_gradient_ELSA = as.data.frame(wealth_gradient_ELSA)
# #
# #
# # write.csv(wealth_gradient_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA.csv", sep=""))
# #
# #
# # ########
# # HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
# #
# #
# # wealth_discrimination_HRS =  summary(glm(discrimination ~ wealth, family = "binomial", data = HRS_subset_wealth_gradient))
# # wealth_gradient_HRS = wealth_discrimination_HRS$coefficients
# # wealth_gradient_HRS = as.data.frame(wealth_gradient_HRS)
# #
# #
# # write.csv(wealth_gradient_HRS, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS.csv", sep=""))
# #
# # #outputting age gradient results
# #
# # age_discrimination =  summary(glm(discrimination ~ age, family = "binomial", data = data_both_countries))
# # age_gradient = age_discrimination$coefficients
# # age_gradient = as.data.frame(age_gradient)
# #
# # write.csv(age_gradient, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination.csv", sep=""))
# #
# #
# # ########
# # ELSA_subset_age_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
# #
# #
# # age_discrimination_ELSA =  summary(glm(discrimination ~ age, family = "binomial", data = ELSA_subset_age_gradient))
# # age_gradient_ELSA = age_discrimination_ELSA$coefficients
# # age_gradient_ELSA = as.data.frame(age_gradient_ELSA)
# #
# #
# # write.csv(age_gradient_ELSA, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination_ELSA.csv", sep=""))
# #
# #
# # ########
# # HRS_subset_age_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
# #
# #
# # age_discrimination_HRS =  summary(glm(discrimination ~ age, family = "binomial", data = HRS_subset_age_gradient))
# # age_gradient_HRS = age_discrimination_HRS$coefficients
# # age_gradient_HRS = as.data.frame(age_gradient_HRS)
# #
# #
# # write.csv(age_gradient_HRS, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination_HRS.csv", sep=""))
# #
# 

###################### Wcountry_unadjusted_SES.R ############################


Wcountry_unadjusted_SES_results = data.frame()

Wcountry_unadjusted_SES_disability_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                      analysis_variable_name = "disability",
                                                                      country = "England", 
                                                                      subsetting_VAR1 = "w5limill", 
                                                                      #has physical limitation
                                                                      var1_value = 1,
                                                                      subsetting_VAR2 =  "NA",
                                                                      var2_value = "NA",
                                                                      discrimination_VAR = "w5disabilitydiscrimination2") 


Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_disability_results)



#financial discriminaiton both sexes
Wcountry_unadjusted_SES_financial_bothSexes_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               analysis_variable_name = "financial_discrimination",
                                                                               country = "England", 
                                                                               subsetting_VAR1 = "NA", 
                                                                               var1_value = "NA",
                                                                               subsetting_VAR2 =  "NA",  
                                                                               var2_value = "NA", 
                                                                               discrimination_VAR = "w5discrim_financial2")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_financial_bothSexes_results) 


#####


Wcountry_unadjusted_SES_financial_female_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                            analysis_variable_name = "financial_discrimination_female",
                                                                            country = "England", 
                                                                            subsetting_VAR1 = "w5sex_1_0",
                                                                            var1_value = 0, 
                                                                            subsetting_VAR2 =   "NA", 
                                                                            var2_value =  "NA", 
                                                                            discrimination_VAR = "w5discrim_financial2")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_financial_female_results) 


#####
Wcountry_unadjusted_SES_financial_male_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                          analysis_variable_name = "financial discrimination, male",
                                                                          country = "England", 
                                                                          subsetting_VAR1 = "w5sex_1_0",
                                                                          var1_value = 1, 
                                                                          subsetting_VAR2 =   "NA", 
                                                                          var2_value =  "NA", 
                                                                          discrimination_VAR = "w5discrim_financial2")



Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_financial_male_results) 



#####
Wcountry_unadjusted_SES_sex_female_results = Wcountry_unadjusted_SES (data= ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                      analysis_variable_name = "sex discrimination, female",
                                                                      country = "England", 
                                                                      subsetting_VAR1 =  "w5sex_1_0",  
                                                                      var1_value = 0, 
                                                                      subsetting_VAR2 = "NA", 
                                                                      var2_value = "NA", 
                                                                      discrimination_VAR = "w5sexdiscrimination2")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_sex_female_results) 



#####
Wcountry_unadjusted_SES_sex_male_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                    
                                                                    analysis_variable_name = "sex discrimination, male",
                                                                    country = "England", 
                                                                    
                                                                    subsetting_VAR1 =  "w5sex_1_0",  
                                                                    var1_value = 1, 
                                                                    
                                                                    subsetting_VAR2 = "NA", 
                                                                    
                                                                    var2_value = "NA", 
                                                                    discrimination_VAR = "w5sexdiscrimination2")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_sex_male_results) 




#####
Wcountry_unadjusted_SES_race_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                
                                                                analysis_variable_name = "race discrimination",
                                                                
                                                                country = "England", 
                                                                
                                                                subsetting_VAR1 =  "w5ethnicity",  
                                                                
                                                                
                                                                
                                                                var1_value = 2, 
                                                                
                                                                subsetting_VAR2= "NA", 
                                                                
                                                                var2_value = "NA", 
                                                                
                                                                discrimination_VAR = "w5racediscrimination2")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_race_results) 



##### there are no significnat covariates for sexual discrimination orientation 
Wcountry_unadjusted_SES_sexuality_results = Wcountry_unadjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "sexual orientation discrimination",
                                                                     
                                                                     country = "England", 
                                                                     
                                                                     subsetting_VAR1 =  "NA",  
                                                                     
                                                                     var1_value = "NA",   
                                                                     
                                                                     subsetting_VAR2= "NA", 
                                                                     
                                                                     var2_value = "NA", 
                                                                     
                                                                     discrimination_VAR = "w5discrim_sexuality2") 




Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_sexuality_results) 


#################### withing country weight: change below to be withinCOuntry_comparison of SES for weight 
####################
Wcountry_unadjusted_SES_weight_29_9_results = Wcountry_unadjusted_SES_weight (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                              analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                              country = "England", 
                                                                              subsetting_VAR1 =  "w4bmi_clean",  
                                                                              var1_value = 29.9, 
                                                                              subsetting_VAR2 = "NA", 
                                                                              var2_value = "NA",
                                                                              discrimination_VAR = "w5weightdiscrimination2")


Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_weight_29_9_results) 


Wcountry_unadjusted_SES_weight_25_results = Wcountry_unadjusted_SES_weight (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                            analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                            country = "England", 
                                                                            subsetting_VAR1 =  "w4bmi_clean",
                                                                            var1_value = 25.0, 
                                                                            subsetting_VAR2 = "w4bmi_clean", 
                                                                            var2_value = 29.9, 
                                                                            discrimination_VAR = "w5weightdiscrimination2" )


Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_weight_25_results) 



Wcountry_unadjusted_SES_weight_both_results = Wcountry_unadjusted_SES_weight(data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                             
                                                                             country = "England", 
                                                                             
                                                                             
                                                                             
                                                                             subsetting_VAR1 = "w4bmi_clean", 
                                                                             subsetting_VAR2  = "NA", 
                                                                             var1_value = 25.0,
                                                                             var2_value = "NA",
                                                                             
                                                                             discrimination_VAR = "w5weightdiscrimination2")


Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_weight_both_results) 

################### HRS:  Wcountry_unadjusted_SES

Wcountry_unadjusted_SES_disability_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         analysis_variable_name = "disability ",
                                                                         
                                                                         country = "USA", 
                                                                         subsetting_VAR1 = "limiting_condition_bin",
                                                                         var1_value = 1,
                                                                         subsetting_VAR2 =   "NA", 
                                                                         var2_value = "NA", 
                                                                         discrimination_VAR = "HRS2010_reason_discrim1_reason_disability")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_disability_HRS_results) 


Wcountry_unadjusted_SES_financial_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                        analysis_variable_name = " financial discrimination ",
                                                                        
                                                                        country = "USA", 
                                                                        subsetting_VAR1 = "NA",
                                                                        var1_value = "NA", 
                                                                        subsetting_VAR2 =   "NA", 
                                                                        var2_value = "NA", 
                                                                        discrimination_VAR = "HRS2010_reason_discrim1_reason_financial")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_financial_HRS_results) 

Wcountry_unadjusted_SES_financial_female_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting,  
                                                                               analysis_variable_name = " financial discrimination, female",
                                                                               
                                                                               country = "USA", 
                                                                               subsetting_VAR1 = "sex_1_0",
                                                                               var1_value = 0, 
                                                                               subsetting_VAR2 =   "NA", 
                                                                               var2_value =  "NA", 
                                                                               discrimination_VAR = "HRS2010_reason_discrim1_reason_financial")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_financial_female_HRS_results) 


Wcountry_unadjusted_SES_financial_male_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             analysis_variable_name = " financial discrimination, male ",
                                                                             country = "USA",
                                                                             subsetting_VAR1 = "sex_1_0",
                                                                             var1_value = 1, 
                                                                             subsetting_VAR2 =   "NA", 
                                                                             var2_value =  "NA",
                                                                             discrimination_VAR = "HRS2010_reason_discrim1_reason_financial")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_financial_male_HRS_results) 


Wcountry_unadjusted_SES_sex_female_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         analysis_variable_name = "sex discrimination, female  ",
                                                                         
                                                                         country = "USA", 
                                                                         subsetting_VAR1 =   "sex_1_0", 
                                                                         var1_value = 0, 
                                                                         subsetting_VAR2 = "NA",
                                                                         var2_value = "NA", 
                                                                         discrimination_VAR = "HRS2010_reason_discrim1_reason_gender")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_sex_female_HRS_results) 



Wcountry_unadjusted_SES_sex_male_HRS_results = Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       analysis_variable_name = "sex discrimination, male ",
                                                                       country = "USA", 
                                                                       subsetting_VAR1 =   "sex_1_0", 
                                                                       var1_value = 1, 
                                                                       subsetting_VAR2 = "NA",
                                                                       var2_value = "NA", 
                                                                       discrimination_VAR = "HRS2010_reason_discrim1_reason_gender")


Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_sex_male_HRS_results) 



Wcountry_unadjusted_SES_race_HRS_results =  Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                    analysis_variable_name = "race discrimination",
                                                                    
                                                                    country = "USA", 
                                                                    subsetting_VAR1 =   "HRS2010_race_nonwhite", 
                                                                    var1_value = 1, 
                                                                    subsetting_VAR2 = "NA",
                                                                    var2_value = "NA", 
                                                                    discrimination_VAR = "HRS2010_reason_discrim1_reason_race")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_race_HRS_results) 


Wcountry_unadjusted_SES_sexuality_HRS_results =  Wcountry_unadjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         analysis_variable_name = "sexual orientation discrimination",
                                                                         
                                                                         country = "USA", 
                                                                         subsetting_VAR1 =   "NA",  
                                                                         var1_value = "NA",   
                                                                         subsetting_VAR2 = "NA",
                                                                         var2_value = "NA", 
                                                                         
                                                                         discrimination_VAR = "HRS2010_reason_discrim1_reason_sexuality")

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_sexuality_HRS_results) 


Wcountry_unadjusted_SES_weight_obese_HRS_results = Wcountry_unadjusted_SES_weight(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  analysis_variable_name = "weight discrimination,  BMI> 29.9 ",
                                                                                  country = "USA", 
                                                                                  subsetting_VAR1 =   "HRS2010_BMI", 
                                                                                  var1_value = 29.9,
                                                                                  subsetting_VAR2 = "NA",
                                                                                  var2_value  = "NA",
                                                                                  discrimination_VAR = "HRS2010_reason_discrim1_reason_weight") 
Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_weight_obese_HRS_results) 

Wcountry_unadjusted_SES_weight_overweight_HRS_results = Wcountry_unadjusted_SES_weight(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                       analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                                       country = "USA", 
                                                                                       subsetting_VAR1 =   "HRS2010_BMI", 
                                                                                       var1_value = 25.0,
                                                                                       subsetting_VAR2 =  "HRS2010_BMI",
                                                                                       var2_value  = 29.9,
                                                                                       discrimination_VAR = "HRS2010_reason_discrim1_reason_weight") 
Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_weight_overweight_HRS_results) 


Wcountry_unadjusted_SES_weight_both_HRS_results = Wcountry_unadjusted_SES_weight(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                 analysis_variable_name = "weight discrimination,  BMI>25.0,",
                                                                                 country = "USA", 
                                                                                 subsetting_VAR1 =   "HRS2010_BMI", 
                                                                                 var1_value = 25.0,
                                                                                 subsetting_VAR2 =  "NA",
                                                                                 var2_value  = "NA",
                                                                                 discrimination_VAR = "HRS2010_reason_discrim1_reason_weight") 

Wcountry_unadjusted_SES_results = rbind(Wcountry_unadjusted_SES_results, Wcountry_unadjusted_SES_weight_both_HRS_results) 

write.csv(Wcountry_unadjusted_SES_results, file = paste(OUTPUT_ROOT, "Wcountry_unadjusted_SES_results.csv", sep=""))




############################ Within country SES diff (adjusted)


Wcountry_adjusted_SES




Wcountry_adjusted_SES_results = data.frame()

Wcountry_adjusted_SES_disability_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                  analysis_variable_name = "disability",
                                                                  country = "England", 
                                                                  subsetting_VAR1 = "w5limill", 
                                                                  #has physical limitation
                                                                  var1_value = 1,
                                                                  subsetting_VAR2 =  "NA",
                                                                  var2_value = "NA",
                                                                  
                                                                  covariate1 = "age",
                                                                  covariate2 = "sex",
                                                                  covariate3 = "NA",
                                                                  covariate4 = "NA", 
                                                                  discrimination_VAR = "w5disabilitydiscrimination2") 

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_disability_results)



#financial discriminaiton both sexes
Wcountry_adjusted_SES_financial_bothSexes_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           analysis_variable_name = "financial discrimination",
                                                                           
                                                                           country = "England", 
                                                                           subsetting_VAR1 = "NA", 
                                                                           var1_value = "NA",
                                                                           subsetting_VAR2 =  "NA",  
                                                                           var2_value = "NA", 
                                                                           
                                                                           covariate1 = "age",
                                                                           covariate2 = "sex",   
                                                                           covariate4 = "NA",
                                                                           covariate3 = "employment",
                                                                           
                                                                           
                                                                           discrimination_VAR = "w5discrim_financial2")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_financial_bothSexes_results) 


#####


Wcountry_adjusted_SES_financial_female_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                        analysis_variable_name = "financial discrimination, female",
                                                                        country = "England", 
                                                                        subsetting_VAR1 = "w5sex_1_0",
                                                                        var1_value = 0, 
                                                                        subsetting_VAR2 =   "NA", 
                                                                        var2_value =  "NA", 
                                                                        
                                                                        covariate1 = "age",
                                                                        covariate2 = "NA",  
                                                                        covariate4 = "NA",
                                                                        covariate3 = "NA",
                                                                        
                                                                        discrimination_VAR = "w5discrim_financial2")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_financial_female_results) 


#####
Wcountry_adjusted_SES_financial_male_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                      analysis_variable_name = "financial discrimination, male",
                                                                      country = "England", 
                                                                      subsetting_VAR1 = "w5sex_1_0",
                                                                      var1_value = 1, 
                                                                      subsetting_VAR2 =   "NA", 
                                                                      var2_value =  "NA", 
                                                                      
                                                                      covariate1 = "age",
                                                                      covariate2 = "NA",    
                                                                      covariate4 = "NA",
                                                                      covariate3 = "NA",
                                                                      
                                                                      discrimination_VAR = "w5discrim_financial2")



Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_financial_male_results) 



#####
Wcountry_adjusted_SES_sex_female_results = Wcountry_adjusted_SES (data= ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                  analysis_variable_name = "sex discrimination, female",
                                                                  country = "England", 
                                                                  
                                                                  subsetting_VAR1 =  "w5sex_1_0",  
                                                                  var1_value = 0, 
                                                                  
                                                                  subsetting_VAR2 = "NA", 
                                                                  var2_value = "NA", 
                                                                  
                                                                  covariate1 = "age",
                                                                  covariate2 = "NA",
                                                                  covariate3 = "NA",
                                                                  covariate4 = "NA", 
                                                                  
                                                                  discrimination_VAR = "w5sexdiscrimination2")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_sex_female_results) 



#####
Wcountry_adjusted_SES_sex_male_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                
                                                                analysis_variable_name = "sex discrimination, male",
                                                                country = "England", 
                                                                
                                                                subsetting_VAR1 =  "w5sex_1_0",  
                                                                var1_value = 1, 
                                                                
                                                                subsetting_VAR2 = "NA", 
                                                                
                                                                var2_value = "NA", 
                                                                
                                                                covariate1 = "age",
                                                                covariate2 = "NA",
                                                                covariate3 = "NA",
                                                                covariate4 = "NA",
                                                                
                                                                discrimination_VAR = "w5sexdiscrimination2")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_sex_male_results) 




#####
Wcountry_adjusted_SES_race_results = Wcountry_adjusted_SES (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                            
                                                            analysis_variable_name = "race discrimination",
                                                            
                                                            country = "England", 
                                                            
                                                            subsetting_VAR1 =  "w5ethnicity",  
                                                            
                                                            
                                                            
                                                            var1_value = 2, 
                                                            
                                                            subsetting_VAR2= "NA", 
                                                            
                                                            var2_value = "NA", 
                                                            
                                                            covariate1 = "age",
                                                            covariate2 = "sex",
                                                            covariate3 = "NA",
                                                            covariate4 = "NA", 
                                                            
                                                            
                                                            discrimination_VAR = "w5racediscrimination2")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_race_results) 



Wcountry_adjusted_SES_sexuality_results =  cbind(NA,
                                                  NA,
                                                  NA,
                                                  NA,
                                                  NA, 
                                                  NA, 
                                                  NA, 
                                                  NA ,
                                                  NA, 
                                                  NA, 
                                                  NA,
                                                  NA, 
                                                  NA, 
                                                  NA)



Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_sexuality_results) 


Wcountry_adjusted_SES_results_weight_29_9_results = Wcountry_adjusted_SES_weight (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                                  country = "England", 
                                                                                  subsetting_VAR1 =  "w4bmi_clean",  
                                                                                  var1_value = 29.9, 
                                                                                  subsetting_VAR2 = "NA", 
                                                                                  var2_value = "NA",
                                                                                  
                                                                                  covariate1 = "age", 
                                                                                  covariate2 = "sex",    
                                                                                  covariate4 = "NA",
                                                                                  covariate3 = "NA",
                                                                                  
                                                                                  discrimination_VAR = "w5weightdiscrimination2")


Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_results_weight_29_9_results) 


Wcountry_adjusted_SES_weight_25_results = Wcountry_adjusted_SES_weight (data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                        analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                        country = "England", 
                                                                        subsetting_VAR1 =  "w4bmi_clean",
                                                                        var1_value = 25.0, 
                                                                        subsetting_VAR2 = "w4bmi_clean", 
                                                                        var2_value = 29.9, 
                                                                        
                                                                        covariate1 = "age", 
                                                                        covariate2 = "sex",   
                                                                        covariate4 = "NA",
                                                                        covariate3 = "NA",
                                                                        
                                                                        discrimination_VAR = "w5weightdiscrimination2" )


Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_weight_25_results) 



Wcountry_adjusted_SES_results_both_results = Wcountry_adjusted_SES_weight(data = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                          
                                                                          analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                          
                                                                          country = "England", 
                                                                          
                                                                          
                                                                          
                                                                          subsetting_VAR1 = "w4bmi_clean", 
                                                                          subsetting_VAR2  = "NA", 
                                                                          var1_value = 25.0,
                                                                          var2_value = "NA",
                                                                          
                                                                          covariate1 = "age", 
                                                                          covariate2 = "sex",    
                                                                          covariate4 = "NA",
                                                                          covariate3 = "NA",
                                                                          
                                                                          
                                                                          discrimination_VAR = "w5weightdiscrimination2")


Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_results_both_results) 

################### HRS:  Wcountry_adjusted_SES

Wcountry_adjusted_SES_disability_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     analysis_variable_name = "disability ",
                                                                     
                                                                     country = "USA", 
                                                                     subsetting_VAR1 = "limiting_condition_bin",
                                                                     var1_value = 1,
                                                                     subsetting_VAR2 =   "NA", 
                                                                     var2_value = "NA", 
                                                                     
                                                                     covariate1 = "age",
                                                                     covariate2 = "sex",
                                                                     covariate3 = "NA",
                                                                     covariate4 = "NA",
                                                                     
                                                                     discrimination_VAR = "HRS2010_reason_discrim1_reason_disability")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_disability_HRS_results) 


Wcountry_adjusted_SES_financial_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                    analysis_variable_name = " financial discrimination ",
                                                                    
                                                                    country = "USA", 
                                                                    subsetting_VAR1 = "NA",
                                                                    var1_value = "NA", 
                                                                    subsetting_VAR2 =   "NA", 
                                                                    var2_value = "NA", 
                                                                    
                                                                    covariate1 = "age",
                                                                    covariate2 = "sex",   
                                                                    covariate4 = "NA",
                                                                    covariate3 = "NA",
                                                                    
                                                                    discrimination_VAR = "HRS2010_reason_discrim1_reason_financial")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_financial_HRS_results) 

Wcountry_adjusted_SES_financial_female_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting,  
                                                                           analysis_variable_name = " financial discrimination, female",
                                                                           
                                                                           country = "USA", 
                                                                           subsetting_VAR1 = "sex_1_0",
                                                                           var1_value = 0, 
                                                                           subsetting_VAR2 =   "NA", 
                                                                           var2_value =  "NA", 
                                                                           
                                                                           covariate1 = "age",
                                                                           covariate2 = "NA",   
                                                                           covariate4 = "NA",
                                                                           covariate3 = "NA",
                                                                           
                                                                           discrimination_VAR = "HRS2010_reason_discrim1_reason_financial")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_financial_female_HRS_results) 


Wcountry_adjusted_SES_financial_male_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         analysis_variable_name = " financial discrimination, male ",
                                                                         country = "USA",
                                                                         subsetting_VAR1 = "sex_1_0",
                                                                         var1_value = 1, 
                                                                         subsetting_VAR2 =   "NA", 
                                                                         var2_value =  "NA",
                                                                         
                                                                         covariate1 = "age",
                                                                         covariate2 = "NA",   
                                                                         covariate4 = "NA",
                                                                         covariate3 = "NA",
                                                                         
                                                                         
                                                                         discrimination_VAR = "HRS2010_reason_discrim1_reason_financial")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results,Wcountry_adjusted_SES_financial_male_HRS_results) 


Wcountry_adjusted_SES_sex_female_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     analysis_variable_name = "sex discrimination, female  ",
                                                                     
                                                                     country = "USA", 
                                                                     subsetting_VAR1 =   "sex_1_0", 
                                                                     var1_value = 0, 
                                                                     subsetting_VAR2 = "NA",
                                                                     var2_value = "NA", 
                                                                     
                                                                     covariate1 = "age",
                                                                     covariate2 = "NA",
                                                                     covariate3 = "NA",
                                                                     covariate4 = "NA", 
                                                                     
                                                                     
                                                                     
                                                                     discrimination_VAR = "HRS2010_reason_discrim1_reason_gender")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_sex_female_HRS_results) 



Wcountry_adjusted_SES_sex_male_HRS_results = Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                   analysis_variable_name = "sex discrimination, male ",
                                                                   country = "USA", 
                                                                   subsetting_VAR1 =   "sex_1_0", 
                                                                   var1_value = 1, 
                                                                   subsetting_VAR2 = "NA",
                                                                   var2_value = "NA", 
                                                                   
                                                                   covariate1 = "age",
                                                                   covariate2 = "NA",
                                                                   covariate3 = "NA",
                                                                   covariate4 = "NA",
                                                                   
                                                                   discrimination_VAR = "HRS2010_reason_discrim1_reason_gender")


Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_sex_male_HRS_results) 



Wcountry_adjusted_SES_race_HRS_results =  Wcountry_adjusted_SES(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                analysis_variable_name = "race discrimination",
                                                                
                                                                country = "USA", 
                                                                subsetting_VAR1 =   "HRS2010_race_nonwhite", 
                                                                var1_value = 1, 
                                                                subsetting_VAR2 = "NA",
                                                                var2_value = "NA", 
                                                                
                                                                covariate1 = "age",
                                                                covariate2 = "sex",
                                                                covariate3 = "NA",
                                                                covariate4 = "NA", 
                                                                
                                                                discrimination_VAR = "HRS2010_reason_discrim1_reason_race")

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_race_HRS_results) 


Wcountry_adjusted_SES_sexuality_HRS_results =  cbind(NA,
                                                     NA,
                                                     NA,
                                                     NA,
                                                     NA, 
                                                     NA, 
                                                     NA, 
                                                     NA ,
                                                     NA, 
                                                     NA, 
                                                     NA,
                                                     NA, 
                                                     NA, 
                                                     NA)

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_sexuality_HRS_results) 



Wcountry_adjusted_SES_results_weight_obese_HRS_results = Wcountry_adjusted_SES_weight(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      analysis_variable_name = "weight discrimination,  BMI> 29.9 ",
                                                                                      country = "USA", 
                                                                                      subsetting_VAR1 =   "HRS2010_BMI", 
                                                                                      var1_value = 29.9,
                                                                                      subsetting_VAR2 = "NA",
                                                                                      var2_value  = "NA",
                                                                                      
                                                                                      covariate1 = "age", 
                                                                                      covariate2 = "sex",    
                                                                                      covariate4 = "NA",
                                                                                      covariate3 = "NA",
                                                                                      
                                                                                      
                                                                                      discrimination_VAR = "HRS2010_reason_discrim1_reason_weight") 

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_results_weight_obese_HRS_results) 


Wcountry_adjusted_SES_weight_overweight_HRS_results = Wcountry_adjusted_SES_weight(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                   analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                                   country = "USA", 
                                                                                   subsetting_VAR1 =   "HRS2010_BMI", 
                                                                                   var1_value = 25.0,
                                                                                   subsetting_VAR2 =  "HRS2010_BMI",
                                                                                   var2_value  = 29.9,
                                                                                   
                                                                                   covariate1 = "age", 
                                                                                   covariate2 = "sex",   
                                                                                   covariate4 = "NA",
                                                                                   covariate3 = "NA",
                                                                                   
                                                                                   
                                                                                   discrimination_VAR = "HRS2010_reason_discrim1_reason_weight") 

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_weight_overweight_HRS_results) 


Wcountry_adjusted_SES_weight_both_HRS_results = Wcountry_adjusted_SES_weight(data = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             analysis_variable_name = "weight discrimination,  BMI>25.0,",
                                                                             country = "USA", 
                                                                             subsetting_VAR1 =   "HRS2010_BMI", 
                                                                             var1_value = 25.0,
                                                                             subsetting_VAR2 =  "NA",
                                                                             var2_value  = "NA",
                                                                             
                                                                             covariate1 = "age", 
                                                                             covariate2 = "sex", 
                                                                             covariate4 = "NA",
                                                                             covariate3 = "NA",
                                                                             
                                                                             discrimination_VAR = "HRS2010_reason_discrim1_reason_weight") 

Wcountry_adjusted_SES_results = rbind(Wcountry_adjusted_SES_results, Wcountry_adjusted_SES_weight_both_HRS_results) 




write.csv(Wcountry_adjusted_SES_results, file = paste(OUTPUT_ROOT, "Wcountry_adjusted_SES_results_basic_adjustment.csv", sep=""))


##############################
##############################


