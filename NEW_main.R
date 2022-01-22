
#set the source and output so you can read the files from any computer
library(dplyr)
## Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"
## Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"


#sourcing the analysis for adjusted models 
source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison.R", sep=""))

#read data files for ELSA and HRS
ELSAdiscrimination_data_wave5_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep=""))
HRS2010_discrimination_dataset_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv", sep=""))

#subset HRS and ELSA dataset to those who are 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 
HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)



# dummy code the countries 
ELSAdiscrimination_data_wave5_age50$country = rep(1, times = nrow(ELSAdiscrimination_data_wave5_age50))
HRS2010_discrimination_dataset_age50$country = rep(0, times = nrow(HRS2010_discrimination_dataset_age50))

#rename the covariates so the names are consistant between ELSA and HRS
# the new names are: 
#age 
#sex 
#education 
#employment 
#wealth 
#wealth_quantiles
#married 
#marital_statusELSAdiscrimination_data_wave5_age50$age = ELSAdiscrimination_data_wave5_age50$w5age

ELSAdiscrimination_data_wave5_age50$age = ELSAdiscrimination_data_wave5_age50$w5age
HRS2010_discrimination_dataset_age50$age  = HRS2010_discrimination_dataset_age50$continious_age


ELSAdiscrimination_data_wave5_age50$sex = ELSAdiscrimination_data_wave5_age50$w5sex_1_0
HRS2010_discrimination_dataset_age50$sex = HRS2010_discrimination_dataset_age50$sex_1_0

ELSAdiscrimination_data_wave5_age50$education = ELSAdiscrimination_data_wave5_age50$ELSA_Education
HRS2010_discrimination_dataset_age50$education = HRS2010_discrimination_dataset_age50$education_levels


ELSAdiscrimination_data_wave5_age50$employment = ELSAdiscrimination_data_wave5_age50$employment
HRS2010_discrimination_dataset_age50$employment =  HRS2010_discrimination_dataset_age50$employment_allCategories


ELSAdiscrimination_data_wave5_age50$wealth = ELSAdiscrimination_data_wave5_age50$w5wealth
HRS2010_discrimination_dataset_age50$wealth = HRS2010_discrimination_dataset_age50$wealth_noIRA_HRS2010


ELSAdiscrimination_data_wave5_age50$wealth_quantiles = ELSAdiscrimination_data_wave5_age50$w5wealthq
HRS2010_discrimination_dataset_age50$wealth_quantiles =  HRS2010_discrimination_dataset_age50$Percentile_wealth_noIRA_HRS2010

ELSAdiscrimination_data_wave5_age50$married = ELSAdiscrimination_data_wave5_age50$w5married
HRS2010_discrimination_dataset_age50$married =  HRS2010_discrimination_dataset_age50$married_bin

#covariates pooled from ELSA and HRS  (make sure the order as above)
#Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
ELSAdiscrimination_data_wave5_age50$marital_status = ELSAdiscrimination_data_wave5_age50$w5married4
HRS2010_discrimination_dataset_age50$marital_status = HRS2010_discrimination_dataset_age50$marital_status


ELSAdiscrimination_data_wave5_age50$medianWealth_ELSA = median(ELSAdiscrimination_data_wave5_age50$w5wealth)
#ELSA median: 652000
#creating a new binary variable: 
ELSAdiscrimination_data_wave5_age50$median_wealth_bin_ELSA = case_when(ELSAdiscrimination_data_wave5_age50$w5wealth >= 652000 ~'2',
                                                                       ELSAdiscrimination_data_wave5_age50$w5wealth < 652000 ~ '1')

#average national wealth excluding pension for age >52 for year 2021: 410100
#average national wealth for the entire nation median = 238,500

HRS2010_discrimination_dataset_age50$medianWealth_HRS = median(HRS2010_discrimination_dataset_age50$wealth_noIRA_HRS2010)
#HRS median: 787500
#creating a new binary variable: 
HRS2010_discrimination_dataset_age50$median_wealth_bin_HRS = case_when(HRS2010_discrimination_dataset_age50$wealth_noIRA_HRS2010 >=787500 ~ '2', 
                                                                       HRS2010_discrimination_dataset_age50$wealth_noIRA_HRS2010 < 787500 ~ '1')



# append the results for the adjusted for covariates ORs using function adjusted_cross_nat_comparison

adjusted_results = data.frame()

#adjusted model for disability 
adjusted_cross_nat_disability_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                       data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
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

adjusted_cross_nat_financial_bothSexes_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                                data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                                
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
                                                                                covariate3 = "married",
                                                                                covariate4 = "employment", 
                                                                                
                                                                                
                                                                                discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_bothSexes_results) 

#####
adjusted_cross_nat_financial_female_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                             data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                             
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
                                                                             covariate2 = "sex",
                                                                             covariate3 = "married",
                                                                             covariate4 = "employment", 
                                                                             
                                                                             
                                                                             discrimination_VAR_elsa = "w5discrim_financial2",
                                                                             discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_female_results) 


#####
adjusted_cross_nat_financial_male_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                           data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                           
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
                                                                           covariate2 = "sex",
                                                                           covariate3 = "married",
                                                                           covariate4 = "employment", 
                                                                           
                                                                           
                                                                           discrimination_VAR_elsa = "w5discrim_financial2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_male_results) 

#####
adjusted_cross_nat_sex_female_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                       data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                       
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
adjusted_cross_nat_sex_male_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                     data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                     
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
adjusted_cross_nat_race_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                 data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                 
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