

SES_unadjusted_cros_nat_comparison_weight = function (data_ELSA, 
                                                   data_HRS,
                                                   
                                                   analysis_variable_name, 
                                                   
                                                   SES_level, 
                                                   
                                                   subsetting_VAR1_ELSA, 
                                                   subsetting_VAR1_HRS,
                                                   
                                                   subsetting_VAR2_ELSA, 
                                                   subsetting_VAR2_HRS,
                                                   
                                                   ELSA_var1_value,
                                                   HRS_var1_value,
                                                   
                                                   ELSA_var2_value,
                                                   HRS_var2_value,
                                                   
                                                   covariate1, 
                                                   covariate2,
                                                   covariate3, 
                                                   covariate4, 
                                                   
                                                   discrimination_VAR_elsa,
                                                   discrimination_VAR_hrs){
  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  
  
  #data_HRS <- data_HRS[ , subsetting_VAR_HRS]
  #data_ELSA <- data_ELSA[ , subsetting_VAR_ELSA]
  
  
  # subsetting data to the right variable for the analysis (eg, sex, physical lim.)
  # if there is only one subsetting var: subsetting_VAR1_ELSA and subsetting_VAR1_HRS
  
  #Obese is BMI>29.9
  #	Overweight is 25-29.9 >25 but =<29.9 
  #	Obese and overweight >25
  
  
  if (subsetting_VAR1_ELSA == "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS == "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = data_ELSA 
    data_HRS_subset = data_HRS
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] > ELSA_var1_value)
    data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR1_HRS] > HRS_var1_value)
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA !="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS != "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] > ELSA_var1_value & data_ELSA[subsetting_VAR2_ELSA] <= ELSA_var2_value)
    data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR1_HRS] > HRS_var1_value & data_HRS[ ,subsetting_VAR2_HRS] <= HRS_var2_value)
  }
  
  
  
  
  # calculate the number of cases for this subset 
  N_ELSA_subset = nrow(data_ELSA_subset)
  N_HRS_subset = nrow(data_HRS_subset)
  
  SES_level = SES_level
  #calculate the number of people who perceived this type of discrimination 
  
  ELSA_discrimYES_subset = subset(data_ELSA_subset, data_ELSA_subset[ , discrimination_VAR_elsa] == 1) 
  HRS_discrimYES_subset = subset(data_HRS_subset,  data_HRS_subset[ , discrimination_VAR_hrs] == 1)
  
  N_ELSA_discrimYES = nrow(ELSA_discrimYES_subset)
  N_HRS_discrimYES = nrow(HRS_discrimYES_subset)
  
  N_ELSA_discrim_NO = N_ELSA_subset - N_ELSA_discrimYES
  N_HRS_discrim_NO = N_HRS_subset - N_HRS_discrimYES
  
  ###################
  probability_disc_ELSA = N_ELSA_discrimYES/N_ELSA_subset 
  probability_no_disc_ELSA = 1 - (probability_disc_ELSA) 
  Odds_yes_ELSA = probability_disc_ELSA/(1-probability_disc_ELSA)
  Odds_no_ELSA = probability_no_disc_ELSA/(1-probability_no_disc_ELSA)
  
  Odds_ratio_ELSA = Odds_yes_ELSA/Odds_no_ELSA
  Odds_ratio_ELSA_CI_lower = exp(log(Odds_ratio_ELSA) - 1.96 * sqrt(1/N_ELSA_discrimYES + 1/N_ELSA_discrim_NO))
  Odds_ratio_ELSA_CI_upper = exp(log(Odds_ratio_ELSA) + 1.96 * sqrt(1/N_ELSA_discrimYES + 1/N_ELSA_discrim_NO))
  
  ###################
  probability_disc_HRS = N_HRS_discrimYES/N_HRS_subset 
  probability_no_disc_HRS = 1 - (probability_disc_HRS) 
  Odds_yes_HRS = probability_disc_HRS/(1-probability_disc_HRS)
  Odds_no_HRS = probability_no_disc_HRS/(1-probability_no_disc_HRS)
  
  Odds_ratio_HRS = Odds_yes_HRS/Odds_no_HRS
  Odds_ratio_HRS_CI_lower = exp(log(Odds_ratio_HRS) - 1.96 * sqrt(1/N_HRS_discrimYES + 1/N_HRS_discrim_NO))
  Odds_ratio_HRS_CI_upper = exp(log(Odds_ratio_HRS) + 1.96 * sqrt(1/N_HRS_discrimYES + 1/N_HRS_discrim_NO))
  
  
  
  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  
  ###############
  
  
  contengency_table_discrimination_AND_country  = table(data_both_countries$discrimination, data_both_countries$country_cat)
  print(contengency_table_discrimination_AND_country)
  test_discrimination_AND_country = chisq.test(contengency_table_discrimination_AND_country)
  summary(test_discrimination_AND_country)
  
  OR_discrimination_cross_national = oddsratio.wald(contengency_table_discrimination_AND_country)
  
  OR_discrimination_cross_national_value = OR_discrimination_cross_national$measure[2, 1]
  OR_discrimination_cross_national_values_CI_lower = OR_discrimination_cross_national$measure[2, 2]
  OR_discrimination_cross_national_values_CI_upper = OR_discrimination_cross_national$measure[2, 3]
  
  discrimination_chi_value_cross_national = test_discrimination_AND_country$statistic
  discrimination_pvalue_cross_national = test_discrimination_AND_country$p.value
  
  
  cross_national_discrimination_findings = cbind(analysis_variable_name,
                                                 
                                                 SES_level, 
                                                 N_ELSA_subset, 
                                                 N_HRS_subset, 
                                                 
                                                 N_ELSA_discrimYES, 
                                                 N_HRS_discrimYES, 
                                                 
                                                 Odds_ratio_ELSA,
                                                 Odds_ratio_ELSA_CI_lower,
                                                 Odds_ratio_ELSA_CI_upper,
                                                 
                                                 Odds_ratio_HRS, 
                                                 Odds_ratio_HRS_CI_lower, 
                                                 Odds_ratio_HRS_CI_upper, 
                                                 
                                                 
                                                 OR_discrimination_cross_national_value, 
                                                 OR_discrimination_cross_national_values_CI_lower, 
                                                 OR_discrimination_cross_national_values_CI_upper, 
                                                 
                                                 
                                                 discrimination_chi_value_cross_national,
                                                 discrimination_pvalue_cross_national)
  
  return(cross_national_discrimination_findings)
} 