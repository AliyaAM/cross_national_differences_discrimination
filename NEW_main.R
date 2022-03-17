
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
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"
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


Unadjusted_results = data.frame()



###### disability
Unadjusted_cross_nat_disability_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           analysis_variable_name = "disability_discrimination",
                                                                           subsetting_VAR1_ELSA = "w5limill", 
                                                                           subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                           #has physical limitation
                                                                           ELSA_var1_value = 1,
                                                                           HRS_var1_value = 1,
                                                                           subsetting_VAR2_ELSA =  "NA",
                                                                           subsetting_VAR2_HRS =   "NA", 
                                                                           ELSA_var2_value = "NA",
                                                                           HRS_var2_value = "NA", 
                                                                           discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability") 

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_disability_results)

###### financial discriminaiton both sexes
Unadjusted_cross_nat_financial_bothSexes_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                analysis_variable_name = "financial_status_discrimination",
                                                                                
                                                                                subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                
                                                                                #low SES
                                                                                ELSA_var1_value = 1, 
                                                                                HRS_var1_value = 1, 
                                                                                
                                                                                
                                                                                subsetting_VAR2_ELSA =  "NA",  
                                                                                subsetting_VAR2_HRS =   "NA", 
                                                                                
                                                                            
                                                                                ELSA_var2_value = "NA", 
                                                                                HRS_var2_value = "NA", 
                                                                                
                                                                            
                                                                                discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_financial_bothSexes_results) 

#####


Unadjusted_cross_nat_financial_female_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "financial_status_discrimination_female",
                                                                             
                                                                             subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                             subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                             
                                                                             
                                                                             
                                                                             #low SES
                                                                             ELSA_var1_value = 1, 
                                                                             HRS_var1_value = 1, 
                                                                             
                                                                             
                                                                             subsetting_VAR2_ELSA =  "w5sex_1_0",  
                                                                             subsetting_VAR2_HRS =   "sex_1_0", 
                                                                             
                                                                             
                                                                             ELSA_var2_value = 0, 
                                                                             HRS_var2_value = 0, 
                                                                             
                                                                      
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_financial_female_results) 


#####
Unadjusted_cross_nat_financial_male_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           
                                                                           analysis_variable_name = "financial_status_discrimination_male",
                                                                           
                                                                           subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                           subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                           
                                                                           #low SES
                                                                           ELSA_var1_value = 1, 
                                                                           HRS_var1_value = 1, 
                                                                           
                                                                           subsetting_VAR2_ELSA =  "w5sex_1_0",  
                                                                           subsetting_VAR2_HRS =   "sex_1_0", 
                                                                           
                                                                           
                                                                           ELSA_var2_value = 1, 
                                                                           HRS_var2_value = 1, 
        
                                                                                                                                   
                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_financial_male_results) 

#####
Unadjusted_cross_nat_sex_female_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       analysis_variable_name = "sex discrimination",
                                                                       
                                                                       
                                                                       subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                       subsetting_VAR1_HRS =   "sex_1_0", 
                                                                       
                                                                       
                                                                       ELSA_var1_value = 0, 
                                                                       HRS_var1_value = 0, 
                                                                       
                                                                       subsetting_VAR2_ELSA = "NA", 
                                                                       subsetting_VAR2_HRS = "NA",
                                                                       
                                                                       #low SES
                                                                       ELSA_var2_value = "NA", 
                                                                       HRS_var2_value = "NA", 
                                                                       
                                                                  
                                                                       
                                                                       discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_sex_female_results) 


#####
Unadjusted_cross_nat_sex_male_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "sex_discrimination_male",
                                                                     
                                                                     
                                                                     subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                     subsetting_VAR1_HRS =   "sex_1_0", 
                                                                     
                                                                     
                                                                     ELSA_var1_value = 1, 
                                                                     HRS_var1_value = 1, 
                                                                     
                                                                     subsetting_VAR2_ELSA = "NA", 
                                                                     subsetting_VAR2_HRS = "NA",
                                                                     
                                                                     #low SES
                                                                     ELSA_var2_value = "NA", 
                                                                     HRS_var2_value = "NA", 
                                                                     

                                                                     
                                                                     discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_sex_male_results) 




#####
Unadjusted_cross_nat_race_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                 
                                                                 analysis_variable_name = "race_discrimination",
                                                                 
                                                                 
                                                                 subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                 subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                 
                                                                 
                                                                 ELSA_var1_value = 2, 
                                                                 HRS_var1_value = 1, 
                                                                 
                                                                 subsetting_VAR2_ELSA = "NA", 
                                                                 subsetting_VAR2_HRS = "NA",
                                                                 
                                                                 #low SES
                                                                 ELSA_var2_value = "NA", 
                                                                 HRS_var2_value = "NA", 
                                                           
                                                                 
                                                                 discrimination_VAR_elsa = "w5racediscrimination2",
                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
Unadjusted_cross_nat_sexuality_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                          
                                                                          analysis_variable_name = "sexual_orientation_discrimination",
                                                                          
                                                                          
                                                                          subsetting_VAR1_ELSA =  "NA",  
                                                                          subsetting_VAR1_HRS =   "NA",  
                                                                          
                                                                          
                                                                          ELSA_var1_value = "NA",   
                                                                          HRS_var1_value = "NA",   
                                                                          
                                                                          subsetting_VAR2_ELSA = "NA", 
                                                                          subsetting_VAR2_HRS = "NA",
                                                                          
                                                                          #low SES
                                                                          ELSA_var2_value = "NA", 
                                                                          HRS_var2_value = "NA", 
                                                                          
                                                                          
                                                                          discrimination_VAR_elsa = "w5discrim_sexuality2",
                                                                          discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_sexuality")

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_sexuality_results) 


Unadjusted_cross_nat_weight_29_9_results = Unadjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               analysis_variable_name = "weight discrimination,  BMI>30",
                                                                               
                                                                               
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


Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_weight_29_9_results) 


Unadjusted_cross_nat_weight_25_results = Unadjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "weight_discrimination_BMImore25_less30",
                                                                             
                                                                             
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


Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_weight_25_results) 



Unadjusted_cross_nat_weight_both_results = Unadjusted_cross_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       analysis_variable_name = "weight discrimination_BMImore25.0",
                                                                       
                                                                       
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


Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_weight_both_results) 
####################

####################################################################################

################### Adjusted analysis is below: 




# append the results for the adjusted for covariates ORs using function adjusted_cross_nat_comparison

adjusted_results = data.frame()

#adjusted model for disability, subsettign further to those who have a physical lim. 
adjusted_cross_nat_disability_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       analysis_variable_name = "disability_discrimination",
                                                                       
                                                                       subsetting_VAR1_ELSA = "w5limill", 
                                                                       subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                       
                                                                       
                                                                       
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
                                                                       

                                                                       discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_disability_results) 

#adjusted model for financial discrimination both genders 

adjusted_cross_nat_financial_bothSexes_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                analysis_variable_name = "financial_status_discrimination",
                                                                                
                                                                                subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                
                                                                                #low SES
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
                                                                                wealth_gradient_cov3 = "sex", 
                                                                                

                                                                                discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_bothSexes_results) 

#####
adjusted_cross_nat_financial_female_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "financial_status_discrimination_female",
                                                                             
                                                                             subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                             subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                             
                                                                             
                                                                             
                                                                             #low SES
                                                                             ELSA_var1_value = 1, 
                                                                             HRS_var1_value = 1, 
                                                                             
                                                                             
                                                                             subsetting_VAR2_ELSA =  "sex",  
                                                                             subsetting_VAR2_HRS =   "sex", 
                                                                             
                                                                             
                                                                             ELSA_var2_value = 0, 
                                                                             HRS_var2_value = 0, 
                                                                             
                                                                             
                                                                             covariate1 = "age",
                                                                             covariate2 = "wealth",
                                                                             covariate3 = "NA",
                                                                             covariate4 = "NA", 
                                                                             
                                                                             wealth_gradient_cov1 = "age", 
                                                                             wealth_gradient_cov2 = "employment",
                                                                             wealth_gradient_cov3 = "NA", 
                                                                             

                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_female_results) 


#####
adjusted_cross_nat_financial_male_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           
                                                                           analysis_variable_name = "financial_status_discrimination_male",
                                                                           
                                                                           subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                           subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                           
                                                                           #low SES
                                                                           ELSA_var1_value = 1, 
                                                                           HRS_var1_value = 1, 
                                                                           
                                                                           subsetting_VAR2_ELSA =  "sex",  
                                                                           subsetting_VAR2_HRS =   "sex", 
                                                                           
                                                                           
                                                                           ELSA_var2_value = 1, 
                                                                           HRS_var2_value = 1, 
                                                                           
                                                                           
                                                                           covariate1 = "age",
                                                                           covariate2 = "wealth",
                                                                           covariate3 = "NA",
                                                                           covariate4 = "NA", 
                                                                           
                                                                           
                                                                           wealth_gradient_cov1 = "age", 
                                                                           wealth_gradient_cov2 = "employment",
                                                                           wealth_gradient_cov3 = "NA", 
                                                                           

                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_male_results) 

#####
adjusted_cross_nat_sex_female_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       analysis_variable_name = "sex_discrimination",
                                                                       
                                                                       
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
                                                                       covariate2 = "wealth",
                                                                       covariate3 = "NA",
                                                                       covariate4 = "NA", 
                                                                       
                                                                       
                                                                       wealth_gradient_cov1 = "age", 
                                                                       wealth_gradient_cov2 = "NA",
                                                                       wealth_gradient_cov3 = "NA", 
                                                                       

                                                                      
                                                                       discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")



adjusted_results = rbind(adjusted_results, adjusted_cross_nat_sex_female_results) 


#####
adjusted_cross_nat_sex_male_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "sex_discrimination_male",
                                                                     
                                                                     
                                                                     subsetting_VAR1_ELSA =  "sex",  
                                                                     subsetting_VAR1_HRS =   "sex", 
                                                                     
                                                                     
                                                                     ELSA_var1_value = 1, 
                                                                     HRS_var1_value = 1, 
                                                                     
                                                                     subsetting_VAR2_ELSA = "NA", 
                                                                     subsetting_VAR2_HRS = "NA",
                                                                     
                                                                     #low SES
                                                                     ELSA_var2_value = "NA", 
                                                                     HRS_var2_value = "NA", 
                                                                     
                                                                     
                                                                     covariate1 = "age",
                                                                     covariate2 = "wealth",
                                                                     covariate3 = "NA",
                                                                     covariate4 = "NA", 
                                                                     
                                                                     wealth_gradient_cov1 = "age", 
                                                                     wealth_gradient_cov2 = "NA",
                                                                     wealth_gradient_cov3 = "NA", 
                                                                     
                                                                     
                                                                     discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_sex_male_results) 




#####
adjusted_cross_nat_race_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                 
                                                                 analysis_variable_name = "race_discrimination",
                                                                 
                                                                 
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
                                                                 covariate3 = "wealth",
                                                                 covariate4 = "NA", 
                                                                 
                                                                 wealth_gradient_cov1 = "sex", 
                                                                 wealth_gradient_cov2 = "NA",
                                                                 wealth_gradient_cov3 = "NA", 
                                                                 

                                                                 
                                                                 discrimination_VAR_elsa = "w5racediscrimination2",
                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_race_results) 


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


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_sexuality_results) 


adjusted_cross_nat_weight_29_9_results = adjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               analysis_variable_name = "weight_discrimination_BMI_more30",
                                                                               
                                                                               
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
                                                                               covariate3 = "wealth",
                                                                               covariate4 = "NA",
                                                                               
                                                                               wealth_gradient_cov1 = "age", 
                                                                               wealth_gradient_cov2 = "employment",
                                                                               wealth_gradient_cov3 = "sex", 
                                                                               
                                                                              
                                                                               
                                                                               discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")



adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_29_9_results) 


adjusted_cross_nat_weight_25_results = adjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "weight_discrimination_BMImore25_less29.9",
                                                                             
                                                                             
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
                                                                             covariate3 = "wealth",
                                                                             covariate4 = "NA",
                                                                             
                                                                             
                                                                             wealth_gradient_cov1 = "age", 
                                                                             wealth_gradient_cov2 = "employment",
                                                                             wealth_gradient_cov3 = "sex", 
                                                                             
                                                                          
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_25_results) 



adjusted_cross_nat_weight_both_results = adjusted_cross_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       analysis_variable_name = "weight_discrimination_BMImore25",
                                                                       
                                                                       
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
                                                                       covariate3 = "wealth",
                                                                       covariate4 = "NA",
                                                                       
                                                                       wealth_gradient_cov1 = "age", 
                                                                       wealth_gradient_cov2 = "employment",
                                                                       wealth_gradient_cov3 = "sex", 
                                                                       
                                                                      
                                                                       
                                                                       discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_both_results) 
####################

write.csv(adjusted_results, file = paste(OUTPUT_ROOT, "Cross_nat_diff_adjusted_results_basic_adjustment.csv", sep=""))


Results_cross_nat = cbind(Unadjusted_results, adjusted_results)


write.csv(Results_cross_nat, file = paste(OUTPUT_ROOT, "Results_cross_nat_basic_adjustment.csv", sep=""))

###################
##################
##################



ELSAdiscrimination_data_wave5_before_subsetting$SES = case_when(ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == '2' ~ '2',
                                                                ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == '1' ~ '1')

HRS2010_discrimination_dataset_before_subsetting$SES = case_when(HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS == '2' ~ '2', 
                                                                 HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS == '1' ~ '1')


SES_unadjusted_results = data.frame()

#adjusted model for disability, subsettign further to those who have a physical lim. 
SES_unadjusted_cross_nat_disability_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       

                                                                       analysis_variable_name = "disability_discrimination",
                                                                       
                                                                       subsetting_VAR1_ELSA = "w5limill", 
                                                                       
                                                                       subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                       
                                                                      
                                                                       SES_level = 1, 
                                                                       
                                                                       #has physical limitation 
                                                                       ELSA_var1_value = 1, 
                                                                       HRS_var1_value = 1, 
                                                                       
                                                                       
                                                                       subsetting_VAR2_ELSA =  "NA",  
                                                                       subsetting_VAR2_HRS =   "NA", 
                                                                       
                                                                       
                                                                       ELSA_var2_value = "NA", 
                                                                       HRS_var2_value = "NA", 
                                                                       
                                                                       discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_disability_results) 

SEShigh_unadjusted_cross_nat_disability_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "disability",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "w5limill", 
                                                                                  subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                                  
                                                                            
                                                                                  
                                                                                  SES_level = 2, 
                                                                                  
                                                                                  #has physical limitation 
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_disability_results) 


SES_unadjusted_cross_nat_financial_all_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "financial_all",
                                                                                  
                                                                                  SES_level = 1,
                                                                                  
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


SES_unadjusted_cross_nat_financial_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                     
                                                                                     
                                                                                     analysis_variable_name = "financial_female",
                                                                                
                                                                                     
                                                                                   
                                                                                     
                                                                                     SES_level = 1, 
                                                                                     
                                                                                     
                                                                                     subsetting_VAR1_ELSA = "sex",
                                                                                     subsetting_VAR1_HRS = "sex",
                                                                                     
                                                                                     ELSA_var1_value = 0, 
                                                                                     HRS_var1_value = 0,
                                                                                     
                                                                                     
                                                                                     subsetting_VAR2_ELSA =  "NA",  
                                                                                     subsetting_VAR2_HRS =   "NA", 
                                                                                     
                                                                                     
                                                                                     ELSA_var2_value = "NA", 
                                                                                     HRS_var2_value = "NA", 
                                                                                     
                                                                                     discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_financial_female_results) 



SEShigh_unadjusted_cross_nat_financial_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      
                                                                                      
                                                                                      analysis_variable_name = "financial_female",
                                                                                      
                                                                                      
                                                                                      
                                                                                      SES_level = 2, 
                                                                                      
                                                                                      
                                                                                      subsetting_VAR1_ELSA =  "sex", 
                                                                                      subsetting_VAR1_HRS =  "sex",
                                                                                      
                                                                                      
                                                                                      
                                                                                      ELSA_var1_value = 0, 
                                                                                      HRS_var1_value = 0, 
                                                                                      
                                                                                      
                                                                                      subsetting_VAR2_ELSA =  "NA",  
                                                                                      subsetting_VAR2_HRS =   "NA", 
                                                                                      
                                                                                      
                                                                                      ELSA_var2_value =  "NA", 
                                                                                      HRS_var2_value =  "NA", 
                                                                                      
                                                                                      discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                      discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_female_results) 




SES_unadjusted_cross_nat_financial_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      
                                                                                      
                                                                                      analysis_variable_name = "financial_male",
                                                                                      
                                                                                      SES_level = 1, 
                                                                                      
                                                                                      subsetting_VAR1_ELSA = "sex",
                                                                                      subsetting_VAR1_HRS = "sex",
                                                                                 
                                                                                
                                                                                
                                                                                      
                                                                                      ELSA_var1_value = 1, 
                                                                                      HRS_var1_value = 1, 
                                                                                      
                                                                                      
                                                                                      subsetting_VAR2_ELSA =  "NA",  
                                                                                      subsetting_VAR2_HRS =   "NA", 
                                                                                      
                                                                                      
                                                                                      ELSA_var2_value = "NA", 
                                                                                      HRS_var2_value = "NA",  
                                                                                      
                                                                                      discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                      discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_financial_male_results) 




SEShigh_unadjusted_cross_nat_financial_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                        
                                                                                        
                                                                                        analysis_variable_name = "financial_male",
                                                                                        
                                                                                        SES_level = 2, 
                                                                                        
                                                                                        subsetting_VAR1_ELSA = "sex", 
                                                                                        subsetting_VAR1_HRS = "sex",
                                                                                  
                                                                                
                                                                                        
                                                                                        ELSA_var1_value = 1, 
                                                                                        HRS_var1_value = 1, 
                                                                                        
                                                                                        
                                                                                        subsetting_VAR2_ELSA =  "NA",  
                                                                                        subsetting_VAR2_HRS =   "NA", 
                                                                                        
                                                                                        
                                                                                        ELSA_var2_value = "NA", 
                                                                                        HRS_var2_value = "NA", 
                                                                                        
                                                                                        discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                        discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


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



SES_unadjusted_cross_nat_sex_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                
                                                                                analysis_variable_name = "sex_discrimination_male",
                                                                                
                                                                              
                                                                                
                                                                                SES_level = 1, 
                                                                                
                                                                                subsetting_VAR1_ELSA = "sex", 
                                                                                subsetting_VAR1_HRS = "sex",
                                                                                
                                                                                
                                                                                ELSA_var1_value = 1, 
                                                                                HRS_var1_value = 1, 
                                                                                
                                                                                
                                                                                subsetting_VAR2_ELSA =  "NA",  
                                                                                subsetting_VAR2_HRS =   "NA", 
                                                                                
                                                                                
                                                                                ELSA_var2_value = "NA", 
                                                                                HRS_var2_value = "NA", 
                                                                                
                                                                                discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SES_unadjusted_results = rbind(SES_unadjusted_results, SES_unadjusted_cross_nat_sex_male_results) 


SEShigh_unadjusted_cross_nat_sex_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "sex_discrimination_male",
                                                                                  
                                                                                  
                                                                                  
                                                                                  SES_level = 2, 
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "sex", 
                                                                                  subsetting_VAR1_HRS = "sex",
                                                                      
                                                                                  
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_sex_male_results) 



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


write.csv(SES_unadjusted_results, file = paste(OUTPUT_ROOT, "SES_unadjusted_results.csv", sep=""))

###########################################################
# SES cross-national comparison (adjusted) 


#adjusted model for disability, subsettign further to those who have a physical lim. 
SES_adjusted_results = data.frame()

SES_adjusted_cross_nat_disability_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                        
                                                                       SES_level = 1, 
                                                                       
                                                                       
                                                                       analysis_variable_name = "disability_discrimination",
                                                                       
                                                                       subsetting_VAR1_ELSA = "w5limill", 
                                                                       subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                       
                                                                       
                                                                       
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
                                                                       
                                                                       
                                                                       discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_disability_results) 


SEShigh_adjusted_cross_nat_disability_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               SES_level = 2, 
                                                                               
                                                                               
                                                                               analysis_variable_name = "disability_discrimination",
                                                                               
                                                                               subsetting_VAR1_ELSA = "w5limill", 
                                                                               subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                               
                                                                               
                                                                               
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
                                                                               
                                                                               
                                                                               discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

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
                                                                                        
                                                                                        subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                        subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                        
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


##############
#############
SES_adjusted_cross_nat_financial_female_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "financial_discrimination_female",
                                                                             
                                                                          
                                                                             
                                                                             SES_level = 1,
                                                                    
                                                          
                                                                             
                                                                             subsetting_VAR1_ELSA =  "sex",  
                                                                             subsetting_VAR1_HRS =   "sex", 
                                                                             
                                                                        
                                                                             ELSA_var1_value = 0, 
                                                                             HRS_var1_value = 0, 
                                                                             
                                                                             subsetting_VAR2_ELSA = "NA", 
                                                                             subsetting_VAR2_HRS = "NA",
                                                                             
                                                                             ELSA_var2_value = "NA", 
                                                                             HRS_var2_value = "NA",
                                                                             
                                                                             
                                                                             covariate1 = "age",
                                                                             covariate2 = "NA",   
                                                                             covariate4 = "NA",
                                                                             covariate3 = "NA",
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_financial_female_results) 


SEShigh_adjusted_cross_nat_financial_female_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                     
                                                                                     analysis_variable_name = "financial_discrimination_female",
                                                                                     
                                                                      
                                                                                     
                                                                                     SES_level = 2,
                                                                                     
                                                                                     subsetting_VAR1_ELSA =  "sex",  
                                                                                     subsetting_VAR1_HRS =   "sex", 
                                                                                     
                                                                                     
                                                                                     ELSA_var1_value = 0, 
                                                                                     HRS_var1_value = 0, 
                                                                                     
                                                                                     subsetting_VAR2_ELSA = "NA", 
                                                                                     subsetting_VAR2_HRS = "NA",
                                                                                     
                                                                                     ELSA_var2_value = "NA", 
                                                                                     HRS_var2_value = "NA",
                                                                                     
                                                                                     
                                                                                     covariate1 = "age",
                                                                                     covariate2 = "NA",    
                                                                                     covariate4 = "NA",
                                                                                     covariate3 = "NA",
                                                                                     
                                                                                     
                                                                                     discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_financial_female_results) 

#####
SES_adjusted_cross_nat_financial_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           
                                                                           analysis_variable_name = "financial_discrimination_male",
                                                                           
                                                                           SES_level = 1, 
                                                                           
                                                                   
                                                                           subsetting_VAR1_ELSA =  "sex",  
                                                                           subsetting_VAR1_HRS =   "sex", 
                                                                           
                                                                           
                                                                           ELSA_var1_value = 1, 
                                                                           HRS_var1_value = 1, 
                                                                           
                                                                           subsetting_VAR2_ELSA = "NA", 
                                                                           subsetting_VAR2_HRS = "NA",
                                                                           
                                                                           ELSA_var2_value = "NA", 
                                                                           HRS_var2_value = "NA",
                                                                           
                                                                           covariate1 = "age",
                                                                           covariate2 = "NA",    
                                                                           covariate4 = "NA",
                                                                           covariate3 = "NA",
                                                                           
                                                                           
                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_financial_male_results) 


SEShigh_adjusted_cross_nat_financial_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                   
                                                                                   analysis_variable_name = "financial_discrimination_male",
                                                                                   
                                                                                   SES_level = 2, 
                                                                                   
                                                                                   subsetting_VAR1_ELSA =  "sex",  
                                                                                   subsetting_VAR1_HRS =   "sex", 
                                                                                   
                                                                                   
                                                                                   ELSA_var1_value = 1, 
                                                                                   HRS_var1_value = 1, 
                                                                                   
                                                                                   subsetting_VAR2_ELSA = "NA", 
                                                                                   subsetting_VAR2_HRS = "NA",
                                                                                   
                                                                                   ELSA_var2_value = "NA", 
                                                                                   HRS_var2_value = "NA",
                                                                                   
                                                                                   
                                                                                   covariate1 = "age",
                                                                                   covariate2 = "NA",    
                                                                                   covariate4 = "NA",
                                                                                   covariate3 = "NA",
                                                                                   
                                                                                   
                                                                                   discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                   discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_financial_male_results) 
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

#####
SES_adjusted_cross_nat_sex_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "sex_discrimination_male",
                                                                     
                                                                     
                                                                     SES_level = 1, 
                                                                     
                                                                     
                                                                     subsetting_VAR1_ELSA =  "sex",  
                                                                     subsetting_VAR1_HRS =   "sex", 
                                                                     
                                                                     
                                                                     ELSA_var1_value = 1, 
                                                                     HRS_var1_value = 1, 
                                                                     
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

SES_adjusted_results = rbind(SES_adjusted_results, SES_adjusted_cross_nat_sex_male_results) 



SEShigh_adjusted_cross_nat_sex_male_results = SES_adjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "sex_discrimination_male",
                                                                             
                                                                             
                                                                             SES_level = 2, 
                                                                             
                                                                             
                                                                             subsetting_VAR1_ELSA =  "sex",  
                                                                             subsetting_VAR1_HRS =   "sex", 
                                                                             
                                                                             
                                                                             ELSA_var1_value = 1, 
                                                                             HRS_var1_value = 1, 
                                                                             
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

SES_adjusted_results = rbind(SES_adjusted_results, SEShigh_adjusted_cross_nat_sex_male_results) 


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



write.csv(SES_adjusted_results, file = paste(OUTPUT_ROOT, "SES_adjusted_results_basic_adjustment.csv", sep=""))


SES_stratified_results = cbind(SES_unadjusted_results, SES_adjusted_results) 


write.csv(SES_stratified_results, file = paste(OUTPUT_ROOT, "SES_stratified_result_sv2_basic_adjustment.csv", sep=""))

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




##### there are no significnat covariates for sexual discrimination orientation 
Wcountry_adjusted_SES_sexuality_results = cbind(NA,
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





############################ SES country interaction ##############################
SESxCountry_interaction_results = data.frame() 
SESxCountry_interaction_disability = SESxCountry_interaction(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting,
                                                             
                                                             analysis_variable_name = "disability", 
                                                             subsetting_VAR1_ELSA = "w5limill", 
                                                             subsetting_VAR1_HRS = "limiting_condition_bin",
                                                             ELSA_var1_value = 1, 
                                                             HRS_var1_value = 1, 
                                                             subsetting_VAR2_ELSA =  "NA",  
                                                             subsetting_VAR2_HRS =   "NA", 
                                                             ELSA_var2_value = "NA", 
                                                             HRS_var2_value = "NA", 
                                                             
                                                             discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")


SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_disability)



###### financial discriminaiton both sexes
SESxCountry_interaction_financial_bothSexes_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                    data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                    
                                                                                    analysis_variable_name = "financial discrimination",
                                                                                    
                                                                                    subsetting_VAR1_ELSA = "NA",
                                                                                    subsetting_VAR1_HRS =  "NA",
                                                                                    
                                                                                    #low SES
                                                                                    ELSA_var1_value = "NA",
                                                                                    HRS_var1_value =  "NA",
                                                                                    
                                                                                    
                                                                                    subsetting_VAR2_ELSA =  "NA",  
                                                                                    subsetting_VAR2_HRS =   "NA", 
                                                                                    
                                                                                    
                                                                                    ELSA_var2_value = "NA", 
                                                                                    HRS_var2_value = "NA", 
                                                                                    
                                                                                    
                                                                                    discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                    discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_financial_bothSexes_results) 

#####


SESxCountry_interaction_financial_female_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                 
                                                                                 analysis_variable_name = "financial discrimination, female",
                                                                            
                                                                            subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                            subsetting_VAR1_HRS =   "sex_1_0", 
                                                                            
                                                                            
                                                                            ELSA_var1_value = 0, 
                                                                            HRS_var1_value = 0, 
                                                                                 
                                                                                 subsetting_VAR2_ELSA =  "NA",
                                                                                 subsetting_VAR2_HRS =  "NA",
                                                                                 
                                                                                 
                                                                                 
                                                                                 #low SES
                                                                                 ELSA_var2_value =  "NA",
                                                                                 HRS_var2_value =  "NA",
                                                                                 
                                                                                 
                                                                            
                                                                                 
                                                                                 discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_financial_female_results) 


#####
SESxCountry_interaction_financial_male_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               analysis_variable_name = "financial discrimination, male",
                                                                               
                                                                          subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                          subsetting_VAR1_HRS =   "sex_1_0", 
                                                                          
                                                                          
                                                                          ELSA_var1_value = 1, 
                                                                          HRS_var1_value = 1, 
                                                                          
                                                                          subsetting_VAR2_ELSA =  "NA",
                                                                          subsetting_VAR2_HRS =  "NA",
                                                                          
                                                                          
                                                                          
                                                                          #low SES
                                                                          ELSA_var2_value =  "NA",
                                                                          HRS_var2_value =  "NA",
                                                                          
                                                                               
                                                                               
                                                                               discrimination_VAR_elsa = "w5discrim_financial2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_financial_male_results) 

#####
SESxCountry_interaction_sex_female_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           
                                                                           analysis_variable_name = "sex discrimination, female",
                                                                           
                                                                           
                                                                           subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                           subsetting_VAR1_HRS =   "sex_1_0", 
                                                                           
                                                                           
                                                                           ELSA_var1_value = 0, 
                                                                           HRS_var1_value = 0, 
                                                                           
                                                                           subsetting_VAR2_ELSA = "NA", 
                                                                           subsetting_VAR2_HRS = "NA",
                                                                           
                                                                           #low SES
                                                                           ELSA_var2_value = "NA", 
                                                                           HRS_var2_value = "NA", 
                                                                           
                                                                           
                                                                           
                                                                           discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_sex_female_results) 


#####
SESxCountry_interaction_sex_male_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         
                                                                         analysis_variable_name = "sex discrimination, male",
                                                                         
                                                                         
                                                                         subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                         subsetting_VAR1_HRS =   "sex_1_0", 
                                                                         
                                                                         
                                                                         ELSA_var1_value = 1, 
                                                                         HRS_var1_value = 1, 
                                                                         
                                                                         subsetting_VAR2_ELSA = "NA", 
                                                                         subsetting_VAR2_HRS = "NA",
                                                                         
                                                                         #low SES
                                                                         ELSA_var2_value = "NA", 
                                                                         HRS_var2_value = "NA", 
                                                                         
                                                                         
                                                                         
                                                                         discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_sex_male_results) 




#####
SESxCountry_interaction_race_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "race discrimination",
                                                                     
                                                                     
                                                                     subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                     subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                     
                                                                     
                                                                     ELSA_var1_value = 2, 
                                                                     HRS_var1_value = 1, 
                                                                     
                                                                     subsetting_VAR2_ELSA = "NA", 
                                                                     subsetting_VAR2_HRS = "NA",
                                                                     
                                                                     #low SES
                                                                     ELSA_var2_value = "NA", 
                                                                     HRS_var2_value = "NA", 
                                                                     
                                                                     
                                                                     discrimination_VAR_elsa = "w5racediscrimination2",
                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
SESxCountry_interaction_sexuality_results = SESxCountry_interaction (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                          
                                                                          analysis_variable_name = "sexual orientation discrimination",
                                                                          
                                                                          
                                                                          subsetting_VAR1_ELSA =  "NA",  
                                                                          subsetting_VAR1_HRS =   "NA",  
                                                                          
                                                                          
                                                                          ELSA_var1_value = "NA",   
                                                                          HRS_var1_value = "NA",   
                                                                          
                                                                          subsetting_VAR2_ELSA = "NA", 
                                                                          subsetting_VAR2_HRS = "NA",
                                                                          
                                                                          #low SES
                                                                          ELSA_var2_value = "NA", 
                                                                          HRS_var2_value = "NA", 
                                                                          
                                                                          
                                                                          discrimination_VAR_elsa = "w5discrim_sexuality2",
                                                                          discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_sexuality")

SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_sexuality_results) 


SESxCountry_interaction_weight_29_9_results = SESxCountry_interaction_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                   
                                                                                   analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                                   
                                                                                   
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


SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_weight_29_9_results) 


SESxCountry_interaction_weight_25_results = SESxCountry_interaction_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                 
                                                                                 analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                                 
                                                                                 
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


SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_weight_25_results) 


SESxCountry_interaction_weight_both_results = SESxCountry_interaction_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                                  
                                                                                  
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


SESxCountry_interaction_results = rbind(SESxCountry_interaction_results, SESxCountry_interaction_weight_both_results) 

write.csv(SESxCountry_interaction_results, file = paste(OUTPUT_ROOT, "SESxCountry_interaction_results.csv", sep=""))

#################### ######################### Situations 
#################### ######################### Situations 

#################### ######################### Situations 
#################### ######################### Situations 

Unadjusted_cross_nat_Situations_results = data.frame()

###### disability
Unadjusted_cross_nat_Situations_disability_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           analysis_variable_name = "disability",
                                                                           subsetting_VAR1_ELSA = "w5limill", 
                                                                           subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                           #has physical limitation
                                                                           ELSA_var1_value = 1,
                                                                           HRS_var1_value = 1,
                                                                           subsetting_VAR2_ELSA =  "NA",
                                                                           subsetting_VAR2_HRS =   "NA", 
                                                                           ELSA_var2_value = "NA",
                                                                           HRS_var2_value = "NA", 
                                                                           discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability") 

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_disability_results)

###### financial discriminaiton both sexes
Unadjusted_cross_nat_Situations_financial_bothSexes_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                    data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                    
                                                                                    analysis_variable_name = "financial discrimination",
                                                                                    
                                                                                    subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                    subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                    
                                                                                    #low SES
                                                                                    ELSA_var1_value = 1, 
                                                                                    HRS_var1_value = 1, 
                                                                                    
                                                                                    
                                                                                    subsetting_VAR2_ELSA =  "NA",  
                                                                                    subsetting_VAR2_HRS =   "NA", 
                                                                                    
                                                                                    
                                                                                    ELSA_var2_value = "NA", 
                                                                                    HRS_var2_value = "NA", 
                                                                                    
                                                                                    
                                                                                    discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                    discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_financial_bothSexes_results) 

#####


Unadjusted_cross_nat_Situations_financial_female_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                 
                                                                                 analysis_variable_name = "financial discrimination, female",
                                                                                 
                                                                                 subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                 subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                 
                                                                                 
                                                                                 
                                                                                 #low SES
                                                                                 ELSA_var1_value = 1, 
                                                                                 HRS_var1_value = 1, 
                                                                                 
                                                                                 
                                                                                 subsetting_VAR2_ELSA =  "w5sex_1_0",  
                                                                                 subsetting_VAR2_HRS =   "sex_1_0", 
                                                                                 
                                                                                 
                                                                                 ELSA_var2_value = 0, 
                                                                                 HRS_var2_value = 0, 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_financial_female_results) 


#####
Unadjusted_cross_nat_Situations_financial_male_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               analysis_variable_name = "financial discrimination, male",
                                                                               
                                                                               subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                               subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                               
                                                                               #low SES
                                                                               ELSA_var1_value = 1, 
                                                                               HRS_var1_value = 1, 
                                                                               
                                                                               subsetting_VAR2_ELSA =  "w5sex_1_0",  
                                                                               subsetting_VAR2_HRS =   "sex_1_0", 
                                                                               
                                                                               
                                                                               ELSA_var2_value = 1, 
                                                                               HRS_var2_value = 1, 
                                                                               
                                                                               
                                                                               discrimination_VAR_elsa = "w5discrim_financial2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_financial_male_results) 

#####
Unadjusted_cross_nat_Situations_sex_female_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           
                                                                           analysis_variable_name = "sex discrimination, female",
                                                                           
                                                                           
                                                                           subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                           subsetting_VAR1_HRS =   "sex_1_0", 
                                                                           
                                                                           
                                                                           ELSA_var1_value = 0, 
                                                                           HRS_var1_value = 0, 
                                                                           
                                                                           subsetting_VAR2_ELSA = "NA", 
                                                                           subsetting_VAR2_HRS = "NA",
                                                                           
                                                                           #low SES
                                                                           ELSA_var2_value = "NA", 
                                                                           HRS_var2_value = "NA", 
                                                                           
                                                                           
                                                                           
                                                                           discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_sex_female_results) 


#####
Unadjusted_cross_nat_Situations_sex_male_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                         data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                         
                                                                         analysis_variable_name = "sex discrimination, male",
                                                                         
                                                                         
                                                                         subsetting_VAR1_ELSA =  "w5sex_1_0",  
                                                                         subsetting_VAR1_HRS =   "sex_1_0", 
                                                                         
                                                                         
                                                                         ELSA_var1_value = 1, 
                                                                         HRS_var1_value = 1, 
                                                                         
                                                                         subsetting_VAR2_ELSA = "NA", 
                                                                         subsetting_VAR2_HRS = "NA",
                                                                         
                                                                         #low SES
                                                                         ELSA_var2_value = "NA", 
                                                                         HRS_var2_value = "NA", 
                                                                         
                                                                         
                                                                         
                                                                         discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                         discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_sex_male_results) 




#####
Unadjusted_cross_nat_Situations_race_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "race discrimination",
                                                                     
                                                                     
                                                                     subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                     subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                     
                                                                     
                                                                     ELSA_var1_value = 2, 
                                                                     HRS_var1_value = 1, 
                                                                     
                                                                     subsetting_VAR2_ELSA = "NA", 
                                                                     subsetting_VAR2_HRS = "NA",
                                                                     
                                                                     #low SES
                                                                     ELSA_var2_value = "NA", 
                                                                     HRS_var2_value = "NA", 
                                                                     
                                                                     
                                                                     discrimination_VAR_elsa = "w5racediscrimination2",
                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
Unadjusted_cross_nat_Situations_sexuality_results = Unadjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                          
                                                                          analysis_variable_name = "sexual orientation discrimination",
                                                                          
                                                                          
                                                                          subsetting_VAR1_ELSA =  "NA",  
                                                                          subsetting_VAR1_HRS =   "NA",  
                                                                          
                                                                          
                                                                          ELSA_var1_value = "NA",   
                                                                          HRS_var1_value = "NA",   
                                                                          
                                                                          subsetting_VAR2_ELSA = "NA", 
                                                                          subsetting_VAR2_HRS = "NA",
                                                                          
                                                                          #low SES
                                                                          ELSA_var2_value = "NA", 
                                                                          HRS_var2_value = "NA", 
                                                                          
                                                                          
                                                                          discrimination_VAR_elsa = "w5discrim_sexuality2",
                                                                          discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_sexuality")

Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_sexuality_results) 



Unadjusted_cross_nat_Situations_weight_29_9_results = Unadjusted_cross_nat_Situations_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                   
                                                                                   analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                                   
                                                                                   
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


Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_weight_29_9_results) 


Unadjusted_cross_nat_Situations_weight_25_results = Unadjusted_cross_nat_Situations_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                 
                                                                                 analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                                 
                                                                                 
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


Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_weight_25_results) 



Unadjusted_cross_nat_Situations_weight_both_results = Unadjusted_cross_nat_Situations_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                                  
                                                                                  
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


Unadjusted_cross_nat_Situations_results = rbind(Unadjusted_cross_nat_Situations_results, Unadjusted_cross_nat_Situations_weight_both_results) 

write.csv(Unadjusted_cross_nat_Situations_results, file = paste(OUTPUT_ROOT, "Unadjusted_cross_nat_Situations_results.csv", sep=""))




####################

Adjusted_cross_nat_Situations_results = data.frame()

#adjusted model for disability, subsettign further to those who have a physical lim. 
Adjusted_cross_nat_Situations_disability_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       analysis_variable_name = "disability",
                                                                       
                                                                       subsetting_VAR1_ELSA = "w5limill", 
                                                                       subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                       
                                                                       
                                                                       
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
                                                                       
                                                                       
                                                                       discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_disability_results) 

#adjusted model for financial discrimination both genders 

Adjusted_cross_nat_Situations_financial_bothSexes_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                analysis_variable_name = "financial discrimination",
                                                                                
                                                                                subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                
                                                                                #low SES
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
                                                                                
                                                                                
                                                                                discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_financial_bothSexes_results) 

#####
Adjusted_cross_nat_Situations_financial_female_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "financial discrimination, female",
                                                                             
                                                                             subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                             subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                             
                                                                             
                                                                             
                                                                             #low SES
                                                                             ELSA_var1_value = 1, 
                                                                             HRS_var1_value = 1, 
                                                                             
                                                                             
                                                                             subsetting_VAR2_ELSA =  "sex",  
                                                                             subsetting_VAR2_HRS =   "sex", 
                                                                             
                                                                             
                                                                             ELSA_var2_value = 0, 
                                                                             HRS_var2_value = 0, 
                                                                             
                                                                             
                                                                             covariate1 = "age",
                                                                             covariate2 = "wealth",
                                                                             covariate3 = "NA",
                                                                             covariate4 = "NA", 
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_financial_female_results) 


#####
Adjusted_cross_nat_Situations_financial_male_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                           
                                                                           analysis_variable_name = "financial discrimination, male",
                                                                           
                                                                           subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                           subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                           
                                                                           #low SES
                                                                           ELSA_var1_value = 1, 
                                                                           HRS_var1_value = 1, 
                                                                           
                                                                           subsetting_VAR2_ELSA =  "sex",  
                                                                           subsetting_VAR2_HRS =   "sex", 
                                                                           
                                                                           
                                                                           ELSA_var2_value = 1, 
                                                                           HRS_var2_value = 1, 
                                                                           
                                                                           
                                                                           covariate1 = "age",
                                                                           covariate2 = "wealth",
                                                                           covariate3 = "NA",
                                                                           covariate4 = "NA", 
                                                                           
                                                                           
                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_financial_male_results) 

#####
Adjusted_cross_nat_Situations_sex_female_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       analysis_variable_name = "sex discrimination, female",
                                                                       
                                                                       
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
                                                                       covariate2 = "wealth",
                                                                       covariate3 = "NA",
                                                                       covariate4 = "NA", 
                                                                       
                                                                       
                                                                       discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")



Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_sex_female_results) 


#####
Adjusted_cross_nat_Situations_sex_male_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                     
                                                                     analysis_variable_name = "sex discrimination, male",
                                                                     
                                                                     
                                                                     subsetting_VAR1_ELSA =  "sex",  
                                                                     subsetting_VAR1_HRS =   "sex", 
                                                                     
                                                                     
                                                                     ELSA_var1_value = 1, 
                                                                     HRS_var1_value = 1, 
                                                                     
                                                                     subsetting_VAR2_ELSA = "NA", 
                                                                     subsetting_VAR2_HRS = "NA",
                                                                     
                                                                     #low SES
                                                                     ELSA_var2_value = "NA", 
                                                                     HRS_var2_value = "NA", 
                                                                     
                                                                     
                                                                     covariate1 = "age",
                                                                     covariate2 = "wealth",
                                                                     covariate3 = "NA",
                                                                     covariate4 = "NA", 
                                                                     
                                                                     
                                                                     discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_sex_male_results) 




#####
Adjusted_cross_nat_Situations_race_results = Adjusted_cross_nat_Situations (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                 
                                                                 analysis_variable_name = "race discrimination",
                                                                 
                                                                 
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
                                                                 covariate3 = "wealth",
                                                                 covariate4 = "NA", 
                                                                 
                                                                 
                                                                 discrimination_VAR_elsa = "w5racediscrimination2",
                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")


Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
Adjusted_cross_nat_Situations_sexuality_results = c(NA, 
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


Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_sexuality_results) 




Adjusted_cross_nat_Situations_weight_29_9_results = Adjusted_cross_nat_Situations_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                               
                                                                               
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
                                                                               covariate3 = "wealth",
                                                                               covariate4 = "NA",
                                                                               
                                                                               
                                                                               discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")



Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_weight_29_9_results) 


Adjusted_cross_nat_Situations_weight_25_results = Adjusted_cross_nat_Situations_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                             data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                             
                                                                             analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                             
                                                                             
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
                                                                             covariate3 = "wealth",
                                                                             covariate4 = "NA",
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_weight_25_results) 



Adjusted_cross_nat_Situations_weight_both_results = Adjusted_cross_nat_Situations_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                              data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                              
                                                                              analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                              
                                                                              
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
                                                                              covariate3 = "wealth",
                                                                              covariate4 = "NA",
                                                                              
                                                                              discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                              discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")

Adjusted_cross_nat_Situations_results = rbind(Adjusted_cross_nat_Situations_results, Adjusted_cross_nat_Situations_weight_both_results) 


write.csv(Adjusted_cross_nat_Situations_results, file = paste(OUTPUT_ROOT, "Adjusted_cross_nat_Situations_result_basic_adjustment.csv", sep=""))

