


source("/Users/aliya/my_docs/proj/cross_national_differences_discrimination/adjusted_cross_nat_comparison.R")

#rename the covariates
#age 
#sex 
#education 
#employment 
#wealth 
#wealth_quantiles
#married 
#marital_status

#covariates pooled from ELSA and HRS  (make sure the order as above)
#Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS


adjusted_cross_nat_comparison_results = adjusted_cross_nat_comparison (data_ELSA = ELSAdiscrimination_data_wave5_age50, 
                                                                           data_HRS = HRS2010_discrimination_dataset_age50, 
                                                                           
                                                                           analysis_variable_name = "disability",
                                                                           
                                                                           subsetting_VAR_ELSA = "w5limill", 
                                                                           subsetting_VAR_HRS = "limiting_condition_bin",
                                                                           
                                                                           ELSA_var_value = 1, 
                                                                           HRS_var_value = 1, 
                                                                           
                                                                           covariate1 = "age",
                                                                           covariate2 = "NA",
                                                                           covariate3 = "NA",
                                                                           covariate4 = "NA",
                                                                           
                                                                    
                                                                           discrimination_VAR_elsa = "w5disabilitydiscrimination2",
                                                                           discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_disability")

