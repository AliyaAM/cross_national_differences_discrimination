
#set the source and output so you can read the files from any computer
library(dplyr)
library(car)
library(stats)

## Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"
## Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"



#sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_comparison.R", sep=""))

#sourcing separately code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_comparison_weight.R", sep=""))


#sourcing the analysis for adjusted models 
source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison.R", sep=""))

source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison_weight.R", sep=""))


#### sourcing the analysis for cross-national comparison stratified by SES
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison.R", sep=""))

#sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison_weight.R", sep=""))

#read data files for ELSA and HRS
ELSAdiscrimination_data_wave5_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep=""))
HRS2010_discrimination_dataset_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv", sep=""))



#subset HRS and ELSA dataset to those who are 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 
HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)

#subet to those who responded to the discrimination items: 
ELSAdiscrimination_data_wave5_before_subsetting = subset(ELSAdiscrimination_data_wave5_age50, ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 0 | ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 1)

#drop rows where the responses across all situations are all NAs in the HRS study 
HRS2010_discrimination_dataset_before_subsetting = subset(HRS2010_discrimination_dataset_age50,HRS2010_discrimination_dataset_age50$HRS2010_discrim_lessrespect!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_harassed!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_poorerservice!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_notclever!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_medical!= "NA")
ELSAdiscrimination_data_wave5_before_subsetting$w5age = as.integer(ELSAdiscrimination_data_wave5_before_subsetting$w5age)

#check these are correct answers in the bin var: 0 or 1
Total_N_ELSA = nrow(ELSAdiscrimination_data_wave5_before_subsetting)
mean_age_ELSA = mean(ELSAdiscrimination_data_wave5_before_subsetting$w5age)
sd_age_ELSA = sd(ELSAdiscrimination_data_wave5_before_subsetting$w5age)
N_women_ELSA = nrow(ELSAdiscrimination_data_wave5_before_subsetting$sex_1_0 == 0)

Total_N_HRS = nrow(HRS2010_discrimination_dataset_before_subsetting)
mean_age_HRS = mean(HRS2010_discrimination_dataset_before_subsetting$continious_age)
sd_age_HRS = sd(HRS2010_discrimination_dataset_before_subsetting$continious_age)
N_women_HRS = nrow(HRS2010_discrimination_dataset_before_subsetting$sex_1_0 == 0)

total_sample = cbind(Total_N_ELSA, mean_age_ELSA, sd_age_ELSA, N_women_ELSA,
                     Total_N_HRS, mean_age_HRS, sd_age_HRS, N_women_HRS)

total_sample

write.csv(total_sample, file = paste(OUTPUT_ROOT, "total_sample_characteristics.csv", sep=""))

# dummy code the countries 
ELSAdiscrimination_data_wave5_before_subsetting$country = rep(1, times = nrow(ELSAdiscrimination_data_wave5_before_subsetting))
HRS2010_discrimination_dataset_before_subsetting$country = rep(0, times = nrow(HRS2010_discrimination_dataset_before_subsetting))



#to be able to subset to low ses and high ses creaate new var: 


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

#save the results of the unadjusted analysis into a dataframe

Unadjusted_results = data.frame()

#disability
Unadjusted_cross_nat_disability_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_disability_results)

#financial discriminaiton both sexes
Unadjusted_cross_nat_financial_bothSexes_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_financial_bothSexes_results) 

#####


Unadjusted_cross_nat_financial_female_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_financial_female_results) 


#####
Unadjusted_cross_nat_financial_male_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_financial_male_results) 

#####
Unadjusted_cross_nat_sex_female_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_sex_female_results) 


#####
Unadjusted_cross_nat_sex_male_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_sex_male_results) 




#####
Unadjusted_cross_nat_race_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_race_results) 


##### there are no significnat covariates for sexual discrimination orientation 
Unadjusted_cross_nat_sexuality_results = Unadjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_sexuality_results) 


Unadjusted_cross_nat_weight_29_9_results = Unadjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                               covariate4 = "employment",
                                                                               
                                                                               
                                                                               discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_weight_29_9_results) 


Unadjusted_cross_nat_weight_25_results = Unadjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                             covariate4 = "employment",
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_weight_25_results) 



Unadjusted_cross_nat_weight_both_results = Unadjusted_cross_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                       covariate4 = "employment",
                                                                       
                                                                       discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


Unadjusted_results = rbind(Unadjusted_results, Unadjusted_cross_nat_weight_both_results) 
####################

####################################################################################

################### Adjusted analysis is below: 

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


ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$w5wealth
HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010


ELSAdiscrimination_data_wave5_before_subsetting$wealth_quantiles = ELSAdiscrimination_data_wave5_before_subsetting$w5wealthq
HRS2010_discrimination_dataset_before_subsetting$wealth_quantiles =  HRS2010_discrimination_dataset_before_subsetting$Percentile_wealth_noIRA_HRS2010

ELSAdiscrimination_data_wave5_before_subsetting$married = ELSAdiscrimination_data_wave5_before_subsetting$w5married
HRS2010_discrimination_dataset_before_subsetting$married =  HRS2010_discrimination_dataset_before_subsetting$married_bin

#covariates pooled from ELSA and HRS  (make sure the order as above)
#Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
ELSAdiscrimination_data_wave5_before_subsetting$marital_status = ELSAdiscrimination_data_wave5_before_subsetting$w5married4
HRS2010_discrimination_dataset_before_subsetting$marital_status = HRS2010_discrimination_dataset_before_subsetting$marital_status




# append the results for the adjusted for covariates ORs using function adjusted_cross_nat_comparison

adjusted_results = data.frame()

#adjusted model for disability, subsettign further to those who have a physical lim. 
adjusted_cross_nat_disability_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                       covariate2 = "employment",
                                                                       covariate3 = "NA",
                                                                       covariate4 = "NA",
                                                                       
                                                                       
                                                                       discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_disability_results) 

#adjusted model for financial discrimination both genders 

adjusted_cross_nat_financial_bothSexes_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                                covariate4 = "employment", 
                                                                                
                                                                                
                                                                                discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_bothSexes_results) 

#####
adjusted_cross_nat_financial_female_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                             covariate2 = "married",
                                                                             covariate3 = "wealth",
                                                                             covariate4 = "employment", 
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_female_results) 


#####
adjusted_cross_nat_financial_male_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                           covariate2 = "married",
                                                                           covariate3 = "wealth",
                                                                           covariate4 = "employment", 
                                                                           
                                                                           
                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_male_results) 

#####
adjusted_cross_nat_sex_female_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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



adjusted_results = rbind(adjusted_results, adjusted_cross_nat_sex_female_results) 


#####
adjusted_cross_nat_sex_male_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_sex_male_results) 




#####
adjusted_cross_nat_race_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                 
                                                                 
                                                                 covariate1 = "sex",
                                                                 covariate2 = "NA",
                                                                 covariate3 = "NA",
                                                                 covariate4 = "NA", 
                                                                 
                                                                 
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
                                                                               covariate4 = "employment",
                                                                               
                                                                               
                                                                               discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                               discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")



adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_29_9_results) 


adjusted_cross_nat_weight_25_results = adjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                             covariate4 = "employment",
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_25_results) 



adjusted_cross_nat_weight_both_results = adjusted_cross_nat_comparison_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
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
                                                                       covariate4 = "employment",
                                                                       
                                                                       discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_both_results) 
####################

write.csv(adjusted_results, file = paste(OUTPUT_ROOT, "Cross_nat_diff_adjusted_results.csv", sep=""))


Results_cross_nat = cbind(Unadjusted_results, adjusted_results)


write.csv(Results_cross_nat, file = paste(OUTPUT_ROOT, "Results_cross_nat", sep=""))

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
                                                                       

                                                                       analysis_variable_name = "disability",
                                                                       
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
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "financial, all",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "NA",
                                                                                  subsetting_VAR1_HRS = "NA",
                                                                         
                                                                                  
                                                                                  SES_level = 1, 
                                                                                  
                                                                          
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
                                                                                     
                                                                                     
                                                                                     analysis_variable_name = "financial, all",
                                                                                     
                                                                                     subsetting_VAR1_ELSA = "NA",
                                                                                     subsetting_VAR1_HRS = "NA",
                                                                                     
                                                                                
                                                                                     
                                                                                     SES_level = 2, 
                                                                                     
                                                                                     
                                                                                     ELSA_var1_value = "NA", 
                                                                                     HRS_var1_value = "NA",
                                                                                     
                                                                                     
                                                                                     subsetting_VAR2_ELSA =  "NA",  
                                                                                     subsetting_VAR2_HRS =   "NA", 
                                                                                     
                                                                                     
                                                                                     ELSA_var2_value = "NA", 
                                                                                     HRS_var2_value = "NA", 
                                                                                     
                                                                                     discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                     discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_all_results) 


SES_unadjusted_cross_nat_financial_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                     
                                                                                     
                                                                                     analysis_variable_name = "financial, male",
                                                                                     
                                                                                     subsetting_VAR1_ELSA = "sex",
                                                                                     subsetting_VAR1_HRS = "sex",
                                                                                     
                                                                                   
                                                                                     
                                                                                     SES_level = 1, 
                                                                                     
                                                                                     
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
                                                                                      
                                                                                      
                                                                                      analysis_variable_name = "financial, male",
                                                                                      
                                                                                      subsetting_VAR1_ELSA =  "sex", 
                                                                                      subsetting_VAR1_HRS =  "sex",
                                                                                      
                                                                                      
                                                                               
                                                                                      
                                                                                      SES_level = 2, 
                                                                                      
                                                                                      
                                                                                      ELSA_var1_value = 1, 
                                                                                      HRS_var1_value = 1, 
                                                                                      
                                                                                      
                                                                                      subsetting_VAR2_ELSA =  "NA",  
                                                                                      subsetting_VAR2_HRS =   "NA", 
                                                                                      
                                                                                      
                                                                                      ELSA_var2_value =  "NA", 
                                                                                      HRS_var2_value =  "NA", 
                                                                                      
                                                                                      discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                      discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_male_results) 




SES_unadjusted_cross_nat_financial_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                      
                                                                                      
                                                                                      analysis_variable_name = "financial, female",
                                                                                      
                                                                                      subsetting_VAR1_ELSA = "sex",
                                                                                      subsetting_VAR1_HRS = "sex",
                                                                                 
                                                                                      SES_level = 1, 
                                                                                      
                                                                                      
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
                                                                                        
                                                                                        
                                                                                        analysis_variable_name = "financial, female",
                                                                                        
                                                                                        subsetting_VAR1_ELSA = "sex", 
                                                                                        subsetting_VAR1_HRS = "sex",
                                                                                  
                                                                                        SES_level = 2, 
                                                                                        
                                                                                        
                                                                                        ELSA_var1_value = 0, 
                                                                                        HRS_var1_value = 0, 
                                                                                        
                                                                                        
                                                                                        subsetting_VAR2_ELSA =  "NA",  
                                                                                        subsetting_VAR2_HRS =   "NA", 
                                                                                        
                                                                                        
                                                                                        ELSA_var2_value = "NA", 
                                                                                        HRS_var2_value = "NA", 
                                                                                        
                                                                                        discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                        discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")


SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_financial_female_results) 


SES_unadjusted_cross_nat_sex_male_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "sex discrimination, male",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "sex", 
                                                                                  subsetting_VAR1_HRS = "sex",
                                                                                
                                                                             
                                                                                
                                                                                
                                                                                  SES_level = 1, 
                                                                                  
                                                                                
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
                                                                                
                                                                                
                                                                                analysis_variable_name = "sex discrimination, male",
                                                                                
                                                                                subsetting_VAR1_ELSA = "sex", 
                                                                                subsetting_VAR1_HRS = "sex",
                                                                                
                                                                      
                                                                                
                                                                                SES_level = 2, 
                                                                                
                                                                                
                                                                                ELSA_var1_value = 1, 
                                                                                HRS_var1_value = 1, 
                                                                                
                                                                                
                                                                                subsetting_VAR2_ELSA =  "NA",  
                                                                                subsetting_VAR2_HRS =   "NA", 
                                                                                
                                                                                
                                                                                ELSA_var2_value = "NA", 
                                                                                HRS_var2_value = "NA", 
                                                                                
                                                                                discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_sex_male_results) 



SES_unadjusted_cross_nat_sex_female_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                
                                                                                analysis_variable_name = "sex discrimination, female",
                                                                                
                                                                                subsetting_VAR1_ELSA = "sex", 
                                                                                subsetting_VAR1_HRS = "sex",
                                                                                
                                                                                
                                                                      
                                                                                
                                                                                SES_level = 1, 
                                                                                
                                                                                
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
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "sex discrimination, female",
                                                                                  
                                                                                  subsetting_VAR1_ELSA = "sex", 
                                                                                  subsetting_VAR1_HRS = "sex",
                                                                      
                                                                                  SES_level = 2, 
                                                                                  
                                                                                  
                                                                                  ELSA_var1_value = 0, 
                                                                                  HRS_var1_value = 0, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA", 
                                                                                  
                                                                                  discrimination_VAR_elsa = "w5sexdiscrimination2",
                                                                                  discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_gender")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_sex_female_results) 



SES_unadjusted_cross_nat_race_results = SES_unadjusted_cros_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                                                  analysis_variable_name = "race",
                                                                                  
                                                                            subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                            subsetting_VAR1_HRS =   "HRS2010_race_nonwhite",  
                                                                            
                                                                         
                                                                                  
                                                                                  SES_level = 1, 
                                                                                  
                                                                        
                                                                                  ELSA_var1_value = 1, 
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
                                                                            
                                                                            subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                            subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                            
                                                                        
                                                                            
                                                                            SES_level = 2, 
                                                                            
                                                                            
                                                                            ELSA_var1_value = 1, 
                                                                            HRS_var1_value = 1, 
                                                                            
                                                                            
                                                                            subsetting_VAR2_ELSA =  "NA",  
                                                                            subsetting_VAR2_HRS =   "NA", 
                                                                            
                                                                            
                                                                            ELSA_var2_value = "NA", 
                                                                            HRS_var2_value = "NA", 
                                                                            
                                                                            discrimination_VAR_elsa = "w5racediscrimination2",
                                                                            discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_race")

SES_unadjusted_results = rbind(SES_unadjusted_results, SEShigh_unadjusted_cross_nat_race_results) 


SES_unadjusted_cross_nat_weight_29_9_results = SES_unadjusted_cros_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                               data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                               
                                                                               analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                               
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
                                                                                           
                                                                                           analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                                           
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
                                                                             
                                                                             analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                             
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
                                                                                         
                                                                                         analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                                         
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
                                                                              
                                                                              analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                              
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
                                                                                          
                                                                                          analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                                          
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

############################



