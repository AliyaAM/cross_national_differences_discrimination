
#set the source and output so you can read the files from any computer
library(dplyr)
library(car)
library(stats)
library(ggplot2)
library(scales)
library(arm)
library(stats)

library("ggpubr")



library(tidyverse)



###### Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"


###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"



###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "subsetting_sample_char.R", sep=""))

source(paste(SOURCE_ROOT, "subsetting_sample_char_weight.R", sep=""))


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





#adjusted model for disability, subsettign further to those who have a physical lim. 
sample_cross_nat_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                                                
                                                   version = "version_1", 
                                                   subset_name = "ALL", 
                                                                                
                                                                                
                                                                                subsetting_VAR1_ELSA = "NA", 
                                                                                subsetting_VAR1_HRS = "NA",
                                                                                
                                                                                
                                                                                
                                                                                #has physical limitation 
                                                                                ELSA_var1_value = 1, 
                                                                                HRS_var1_value = 1, 
                                                                                
                                                                                
                                                                                subsetting_VAR2_ELSA =  "NA",  
                                                                                subsetting_VAR2_HRS =   "NA", 
                                                                                
                                                                                
                                                                                ELSA_var2_value = "NA", 
                                                                                HRS_var2_value = "NA")


#adjusted model for disability, subsettign further to those who have a physical lim. 
sample_cross_nat_disability_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                                                  
                                                              version = "version_1", 
                                                              subset_name = "disability", 
                                                              
                                                                                  

                                                                                  subsetting_VAR1_ELSA = "w5limill", 
                                                                                  subsetting_VAR1_HRS = "limiting_condition_bin",
                                                                                  
                                                                                  
                                                                                  
                                                                                  #has physical limitation 
                                                                                  ELSA_var1_value = 1, 
                                                                                  HRS_var1_value = 1, 
                                                                                  
                                                                                  
                                                                                  subsetting_VAR2_ELSA =  "NA",  
                                                                                  subsetting_VAR2_HRS =   "NA", 
                                                                                  
                                                                                  
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA")



#adjusted model for financial discrimination both genders 

sample_cross_nat_financial_bothSexes_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                           data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                       
                                                                       version = "version_1", 
                                                                       subset_name = "financial",    

                                                                                           subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                           subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                           
                                                                                           #low SES
                                                                                           ELSA_var1_value = 1, 
                                                                                           HRS_var1_value = 1, 
                                                                                           
                                                                                           
                                                                                           subsetting_VAR2_ELSA =  "NA",  
                                                                                           subsetting_VAR2_HRS =   "NA", 
                                                                                           
                                                                                           
                                                                                           ELSA_var2_value = "NA", 
                                                                                           HRS_var2_value = "NA")


#####
sample_cross_nat_financial_female_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                    
                                                                    version = "version_1", 
                                                                    subset_name = "financial_female",                       

                                                                                        subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                        subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                        
                                                                                        
                                                                                        
                                                                                        #low SES
                                                                                        ELSA_var1_value = 1, 
                                                                                        HRS_var1_value = 1, 
                                                                                        
                                                                                        
                                                                                        subsetting_VAR2_ELSA =  "sex",  
                                                                                        subsetting_VAR2_HRS =   "sex", 
                                                                                        
                                                                                        
                                                                                        ELSA_var2_value = 0, 
                                                                                        HRS_var2_value = 0)



#####
sample_cross_nat_financial_male_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting,
                                                                  
                                                                  version = "version_1", 
                                                                  subset_name = "financial_male", 
                                                                                      

                                                                                      subsetting_VAR1_ELSA = "median_wealth_bin_ELSA", 
                                                                                      subsetting_VAR1_HRS = "median_wealth_bin_HRS",
                                                                                      
                                                                                      #low SES
                                                                                      ELSA_var1_value = 1, 
                                                                                      HRS_var1_value = 1, 
                                                                                      
                                                                                      subsetting_VAR2_ELSA =  "sex",  
                                                                                      subsetting_VAR2_HRS =   "sex", 
                                                                                      
                                                                                      
                                                                                      ELSA_var2_value = 1, 
                                                                                      HRS_var2_value = 1)


#####
sample_cross_nat_sex_female_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                  
                                                              
                                                              version = "version_1", 
                                                              subset_name = "sex_female", 
                                                              
                                                                                  
                                                                                  subsetting_VAR1_ELSA =  "sex",  
                                                                                  subsetting_VAR1_HRS =   "sex", 
                                                                                  
                                                                                  
                                                                                  ELSA_var1_value = 0, 
                                                                                  HRS_var1_value = 0, 
                                                                                  
                                                                                  subsetting_VAR2_ELSA = "NA", 
                                                                                  subsetting_VAR2_HRS = "NA",
                                                                                  
                                                                                  #low SES
                                                                                  ELSA_var2_value = "NA", 
                                                                                  HRS_var2_value = "NA")





#####
sample_cross_nat_sex_male_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                
                                                            
                                                            version = "version_1", 
                                                            subset_name = "sex_male", 
                                                            
                                                                                
                                                                                subsetting_VAR1_ELSA =  "sex",  
                                                                                subsetting_VAR1_HRS =   "sex", 
                                                                                
                                                                                
                                                                                ELSA_var1_value = 1, 
                                                                                HRS_var1_value = 1, 
                                                                                
                                                                                subsetting_VAR2_ELSA = "NA", 
                                                                                subsetting_VAR2_HRS = "NA",
                                                                                
                                                                                #low SES
                                                                                ELSA_var2_value = "NA", 
                                                                                HRS_var2_value = "NA")





#####
sample_cross_nat_race_results = subsetting_sample_char (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                            data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                            
                                                        
                                                        version = "version_1", 
                                                        subset_name = "race", 
                                                        
                                                                            
                                                                            subsetting_VAR1_ELSA =  "w5ethnicity",  
                                                                            subsetting_VAR1_HRS =   "HRS2010_race_nonwhite", 
                                                                            
                                                                            
                                                                            ELSA_var1_value = 2, 
                                                                            HRS_var1_value = 1, 
                                                                            
                                                                            subsetting_VAR2_ELSA = "NA", 
                                                                            subsetting_VAR2_HRS = "NA",
                                                                            
                                                                            #low SES
                                                                            ELSA_var2_value = "NA", 
                                                                            HRS_var2_value = "NA")



##### there are no significnat covariates for sexual discrimination orientation 
sample_cross_nat_sexuality_results = c(NA, 
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




sample_cross_nat_weight_29_9_results = subsetting_sample_char_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                          

                                                                                          
                                                                                         subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                         subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                          
                                                                                          
                                                                                         ELSA_var1_value = 29.9, 
                                                                                         HRS_var1_value  = 29.9,  
                                                                                          
                                                                                          subsetting_VAR2_ELSA = "NA", 
                                                                                          subsetting_VAR2_HRS = "NA",
                                                                                          
                                                                                          #low SES
                                                                                          ELSA_var2_value = "NA", 
                                                                                          HRS_var2_value = "NA")





sample_cross_nat_weight_25_results = subsetting_sample_char_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                        

                                                                                        
                                                                                       subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                        subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                        
                                                                                       ELSA_var1_value = 25.0, 
                                                                                        HRS_var1_value  = 25.0, 
                                                                                        
                                                                                        
                                                                                        
                                                                                        subsetting_VAR2_ELSA = "w4bmi_clean", 
                                                                                        subsetting_VAR2_HRS = "HRS2010_BMI",
                                                                                      
                                                                                        ELSA_var2_value = 29.9, 
                                                                                        HRS_var2_value = 29.9)





sample_cross_nat_weight_both_results = subsetting_sample_char_weight(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                                         

                                                                                         
                                                                                         subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                                         subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                                         
                                                                                         
                                                                                         ELSA_var1_value = 25.0, 
                                                                                         HRS_var1_value = 25.0,  
                                                                                         
                                                                                        subsetting_VAR2_ELSA = "NA",
                                                                                         subsetting_VAR2_HRS = "NA",
                                                                                         
                                                                                         
                                                                                         ELSA_var2_value = "NA",
                                                                                         HRS_var2_value  = "NA")




