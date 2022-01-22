


source("/Users/aliya/my_docs/proj/cross_national_differences_discrimination/adjusted_cross_nat_comparison.R")



# dummy code the countries 
ELSAdiscrimination_data_wave5_age50$country = rep(1, times = N_ELSA_subset)
HRS2010_discrimination_dataset_age50$country = rep(0, times = N_HRS_subset)

#rename the covariates so the names are consistant between ELSA and HRS
# the new names are: 
#age 
#sex 
#education 
#employment 
#wealth 
#wealth_quantiles
#married 
#marital_status

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
                                                                           
                                                                           subsetting_VAR_ELSA = "w5limill", 
                                                                           subsetting_VAR_HRS = "limiting_condition_bin",
                                                                           
                                                                      #has physical limitation 
                                                                           ELSA_var_value = 1, 
                                                                           HRS_var_value = 1, 
                                                                           
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
                                                                       
                                                                       subsetting_VAR_ELSA = "median_wealth_bin_ELSA", 
                                                                       subsetting_VAR_HRS = "median_wealth_bin_HRS",
                                                                       
                                                                       #low SES
                                                                       ELSA_var_value = 1, 
                                                                       HRS_var_value = 1, 
                                                            
                                                                       
                                                                       covariate1 = "age",
                                                                       covariate2 = "sex",
                                                                       covariate3 = "married",
                                                                       covariate4 = "employment", 
                                                                       
                                                                       
                                                                       discrimination_VAR_elsa = "w5discrim_financial2",
                                                                       discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")

adjusted_results = rbind(adjusted_results, adjusted_cross_nat_financial_bothSexes_results) 


adjusted_cross_nat_financial_bothSexes_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                                data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                                
                                                                                analysis_variable_name = "financial discrimination",
                                                                                
                                                                                subsetting_VAR_ELSA = "median_wealth_bin_ELSA", 
                                                                                subsetting_VAR_HRS = "median_wealth_bin_HRS",
                                                                                
                                                                                #low SES
                                                                                ELSA_var_value = 1, 
                                                                                HRS_var_value = 1, 
                                                                                
                                                                                
                                                                                covariate1 = "age",
                                                                                covariate2 = "sex",
                                                                                covariate3 = "married",
                                                                                covariate4 = "employment", 
                                                                                
                                                                                
                                                                                discrimination_VAR_elsa = "w5discrim_financial2",
                                                                                discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_financial")
