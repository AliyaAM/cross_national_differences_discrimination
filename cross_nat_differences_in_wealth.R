

#set the source and output so you can read the files from any computer
library(dplyr)
library(stats)
library(tidyr)

#https://github.com/dreamRs/esquisse

###### Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"


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


ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds = ELSAdiscrimination_data_wave5_before_subsetting$w5wealth
#median in ELSA: £239600  OR $324211.1 

median(ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds, na.rm = TRUE)


ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds * 1.353135
median(ELSAdiscrimination_data_wave5_before_subsetting$wealth, na.rm = TRUE)


HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010

median(HRS2010_discrimination_dataset_before_subsetting$wealth, na.rm = TRUE)
# median in HRS: $143000

country = c(rep(1, times = nrow(ELSAdiscrimination_data_wave5_before_subsetting)), 
                               rep(0, times = nrow(HRS2010_discrimination_dataset_before_subsetting)))

country_comparison = data.frame(country)

country_comparison$wealth = c(ELSAdiscrimination_data_wave5_before_subsetting$wealth, 
                              HRS2010_discrimination_dataset_before_subsetting$wealth)

nrow(ELSAdiscrimination_data_wave5_before_subsetting)
nrow(HRS2010_discrimination_dataset_before_subsetting)

ELSAdiscrimination_data_wave5_before_subsetting$wealth

median_wealth_ELSA = median(ELSAdiscrimination_data_wave5_before_subsetting$wealth)

ELSAdiscrimination_data_wave5_before_subsetting$medianWealth_ELSA = median(ELSAdiscrimination_data_wave5_before_subsetting$wealth)
HRS2010_discrimination_dataset_before_subsetting$medianWealth_HRS = median(HRS2010_discrimination_dataset_before_subsetting$wealth)



#marital_statusELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age

ELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age
HRS2010_discrimination_dataset_before_subsetting$age  = HRS2010_discrimination_dataset_before_subsetting$continious_age

#test for differences in age, sex, education, marital status, and employment between US and England 


country_comparison$age = c(ELSAdiscrimination_data_wave5_before_subsetting$age, 
                           HRS2010_discrimination_dataset_before_subsetting$age)



ELSAdiscrimination_data_wave5_before_subsetting$sex = ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0
HRS2010_discrimination_dataset_before_subsetting$sex = HRS2010_discrimination_dataset_before_subsetting$sex_1_0


country_comparison$sex = c(ELSAdiscrimination_data_wave5_before_subsetting$sex, 
                           HRS2010_discrimination_dataset_before_subsetting$sex)

sex_table = as.table(country_comparison$sex, country_comparison$country)
chisq.test(sex_table)

#check what -1 is in ELSA in education 
#S1EDYRS

#SCLDDR -- social standing 

#ELSAdiscrimination_data_wave5_before_subsetting$fqqual
  

ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree = case_when(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree == 1 ~ 1, 
                                                                                      ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree == 0 ~ 0) 


ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree = as.numeric(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree)


unique(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree)
unique(HRS2010_discrimination_dataset_before_subsetting$education_high_level_bin)

country_comparison$education_high = c(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Degree_higher_degree,
                                      HRS2010_discrimination_dataset_before_subsetting$education_high_level_bin)

ELSAdiscrimination_data_wave5_before_subsetting$ELSA_A_levels = case_when(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_A_levels == 1 ~ 1, 
                                                                          ELSAdiscrimination_data_wave5_before_subsetting$ELSA_A_levels == 0 ~ 0) 


country_comparison$education_medium = c(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_A_levels,
                                        HRS2010_discrimination_dataset_before_subsetting$education_intermediate_level_bin)


ELSAdiscrimination_data_wave5_before_subsetting$ELSA_GCSE_A_C = case_when(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_GCSE_A_C == 1 ~ 1,
                                                                          ELSAdiscrimination_data_wave5_before_subsetting$ELSA_GCSE_A_C == 0 ~ 0)


country_comparison$education_low = c(ELSAdiscrimination_data_wave5_before_subsetting$ELSA_GCSE_A_C,
                                        HRS2010_discrimination_dataset_before_subsetting$education_low_level_bin)



ELSAdiscrimination_data_wave5_before_subsetting$fqaqua = case_when(ELSAdiscrimination_data_wave5_before_subsetting$fqaqua == 1 ~ 1,
                                                                          ELSAdiscrimination_data_wave5_before_subsetting$fqaqua == 2 ~ 0)



country_comparison$education_none = c(ELSAdiscrimination_data_wave5_before_subsetting$fqaqua,
                                      HRS2010_discrimination_dataset_before_subsetting$education_no)


unique(HRS2010_discrimination_dataset_before_subsetting$education_no) 
unique(ELSAdiscrimination_data_wave5_before_subsetting$fqaqua)
unique(country_comparison$education_none)

#ELSAdiscrimination_data_wave5_before_subsetting$education = ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Education
#HRS2010_discrimination_dataset_before_subsetting$education = HRS2010_discrimination_dataset_before_subsetting$education_levels


#country_comparison$education = c(ELSAdiscrimination_data_wave5_before_subsetting$education, 
#                                 HRS2010_discrimination_dataset_before_subsetting$education)

#################
ELSAdiscrimination_data_wave5_before_subsetting$employment_working = case_when(ELSAdiscrimination_data_wave5_before_subsetting$employment == 1 ~1, 
                                                                               ELSAdiscrimination_data_wave5_before_subsetting$employment == 2 ~ 0,
                                                                               ELSAdiscrimination_data_wave5_before_subsetting$employment == 4 ~ 0,
                                                                               ELSAdiscrimination_data_wave5_before_subsetting$employment == 5 ~ 0, 
                                                                               ELSAdiscrimination_data_wave5_before_subsetting$employment == 6 ~ 0,
                                                                               ELSAdiscrimination_data_wave5_before_subsetting$employment == 7 ~ 0)



HRS2010_discrimination_dataset_before_subsetting$employment_working = case_when(HRS2010_discrimination_dataset_before_subsetting$employment_allCategories == 1 ~1, 
                                                                                HRS2010_discrimination_dataset_before_subsetting$employment_allCategories == 2 ~ 0,
                                                                                HRS2010_discrimination_dataset_before_subsetting$employment_allCategories == 4 ~ 0,
                                                                                HRS2010_discrimination_dataset_before_subsetting$employment_allCategories == 5 ~ 0, 
                                                                                HRS2010_discrimination_dataset_before_subsetting$employment_allCategories == 6 ~ 0,
                                                                                HRS2010_discrimination_dataset_before_subsetting$employment_allCategories == 7 ~ 0)


country_comparison$employment = c(ELSAdiscrimination_data_wave5_before_subsetting$employment_working, 
                                  HRS2010_discrimination_dataset_before_subsetting$employment_working)

#check what is -8, 85 and 96 in ELSA employment 
unique(ELSAdiscrimination_data_wave5_before_subsetting$employment) 
unique(HRS2010_discrimination_dataset_before_subsetting$employment)


########################################

ELSAdiscrimination_data_wave5_before_subsetting$married = ELSAdiscrimination_data_wave5_before_subsetting$w5married
HRS2010_discrimination_dataset_before_subsetting$married =  HRS2010_discrimination_dataset_before_subsetting$married_bin

country_comparison$married = c(ELSAdiscrimination_data_wave5_before_subsetting$w5married, 
                               HRS2010_discrimination_dataset_before_subsetting$married_bin)

unique(country_comparison$married)

ELSAdiscrimination_data_wave5_before_subsetting$marital_status = ELSAdiscrimination_data_wave5_before_subsetting$w5married4
HRS2010_discrimination_dataset_before_subsetting$marital_status = HRS2010_discrimination_dataset_before_subsetting$marital_status

unique(ELSAdiscrimination_data_wave5_before_subsetting$marital_status) 
unique(HRS2010_discrimination_dataset_before_subsetting$marital_status) 

country_comparison$marital_status = c(ELSAdiscrimination_data_wave5_before_subsetting$marital_status, 
                                  HRS2010_discrimination_dataset_before_subsetting$marital_status)



#####################




country_comparison$discrim_disability = c(ELSAdiscrimination_data_wave5_before_subsetting$w5disabilitydiscrimination2,
                                      HRS2010_discrimination_dataset_before_subsetting$HRS2010_reason_discrim1_reason_disability)


unique(country_comparison$marital_status)
#0  2  3  1 NA  5  4  6  7  9




country_comparison = drop_na(country_comparison)


#test for differences in  wealth, 
t.test(age ~ country, data = country_comparison,   alternative = "two.sided", var.equal = TRUE)

t.test(wealth ~ country, data = country_comparison,   alternative = "two.sided", var.equal = TRUE)

education_sex_table = as.table(country_comparison$sex, country_comparison$country)
chisq.test(education_sex_table)


education_high_table = as.table(country_comparison$education_high, country_comparison$country)
chisq.test(education_high_table)

education_medium_table = as.table(country_comparison$education_medium, country_comparison$country)
chisq.test(education_medium_table)


education_low_table = as.table(country_comparison$education_low, country_comparison$country)
chisq.test(education_low_table)

education_none_table = as.table(country_comparison$education_none, country_comparison$country)
chisq.test(education_none_table)

employment_table = as.table(country_comparison$employment, country_comparison$country)
chisq.test(employment_table)

##### country_comparison$employment: 

##### 1. recode employment as a binary var 
##### 2. ch-test for binary employment 
#2. put in suppliments: we report adjusting for age, sex and wealth (basic adjustment) share with Ruth
#3. also adjust for variables that were different between US and England samples (education (high) is different) in supplements 
#4. table 4 edit the excel output into word
#5. sex discrimination being worse in US: explain in the discussion 
#6. check in methods that we are reporitng only the variables that we adjust for 
#7. plot: probability being discriminated (y) by wealth (x) and grouped by country

married_table = as.table(country_comparison$married, country_comparison$country)
chisq.test(married_table)


#https://stats.oarc.ucla.edu/r/dae/logit-regression/
  
  
