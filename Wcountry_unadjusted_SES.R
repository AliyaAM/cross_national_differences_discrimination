

Wcountry_unadjusted_SES = function (data, 
                                                    country, 
                                                    
                                                    analysis_variable_name,
                                                    
                                                    subsetting_VAR1, 
                                                    subsetting_VAR2, 
                                                    var1_value,
                                                    var2_value,
                                                    
                                                    discrimination_VAR){
  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  
  
  #data_HRS <- data_HRS[ , subsetting_VAR_HRS]
  #data <- data[ , subsetting_VAR ]
  
  
  # subsetting data to the right variable for the analysis (eg, sex, physical lim.)
  # if there is only one subsetting var: subsetting_VAR1 and subsetting_VAR1_HRS
  if (subsetting_VAR1 == "NA" & subsetting_VAR2 =="NA" ){
    
    data_subset = data
  } 
  
  if (subsetting_VAR1 != "NA" & subsetting_VAR2 =="NA" ){
    
    data_subset = subset(data, data[ , subsetting_VAR1] == var1_value)
  } 
  
  if (subsetting_VAR1 != "NA" & subsetting_VAR2 !="NA" ){
    
    data_subset = subset(data,  data[ , subsetting_VAR1] == var1_value & data[subsetting_VAR2] == var2_value)
  }
  
  # calculate the number of cases for this subset 
  N_subset = nrow(data_subset)

  country = country
  
  #calculate the number of people who perceived this type of discrimination 
  
  discrimYES_subset = subset(data_subset, data_subset[ , discrimination_VAR] == 1) 

  N_discrimYES = nrow(discrimYES_subset)
  N_discrim_NO = N_subset - N_discrimYES



  ###################
  probability_disc = N_discrimYES/N_subset 
  probability_no_disc  = 1 - (probability_disc) 
  Odds_yes  = probability_disc/(1-probability_disc)
  Odds_no  = probability_no_disc /(1-probability_no_disc )
  
  Odds_ratio  = Odds_yes /Odds_no 
  Odds_ratio_CI_lower = exp(log(Odds_ratio ) - 1.96 * sqrt(1/N_discrimYES + 1/N_discrim_NO))
  Odds_ratio_CI_upper = exp(log(Odds_ratio ) + 1.96 * sqrt(1/N_discrimYES + 1/N_discrim_NO))
  
  ###################
  
  
  ###################
  low_SES = subset(data_subset, data_subset$SES == 1)
  N_low_SES = nrow(subset(data_subset, data_subset$SES == 1))
  
  discrimYES_low_SESsubset = subset(low_SES, low_SES[ , discrimination_VAR] == 1) 
  n_discrimYES_low_SES= nrow(discrimYES_low_SESsubset)
  n_discrimNO_low_SES = N_low_SES - n_discrimYES_low_SES
  
  
  
  high_SES = subset(data_subset, data_subset$SES == 2)
  N_high_SES = nrow(subset(data_subset, data_subset$SES == 2))
  
  discrimYES_high_SESsubset = subset(high_SES, high_SES[ , discrimination_VAR] == 1) 
  n_discrimYES_high_SES= nrow(discrimYES_high_SESsubset)
  n_discrimNO_high_SES = N_high_SES - n_discrimYES_high_SES

  
  #predictor dummy varibale: country (UK vs USA)
   SES = data_subset$SES
  

  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  discrimination = data_subset[ , discrimination_VAR]
                                         

  ###############
  
  contengency_table_discrimination_AND_SES  = table(SES, discrimination)
  print(contengency_table_discrimination_AND_SES)
  test_discrimination_AND_SES = chisq.test(contengency_table_discrimination_AND_SES)
  summary(test_discrimination_AND_SES)
  
  OR_discrimination_SES = oddsratio.wald(contengency_table_discrimination_AND_SES)
  
  OR_discrimination_SES_value = OR_discrimination_SES$measure[2, 1]
  OR_discrimination_SES_values_CI_lower = OR_discrimination_SES$measure[2, 2]
  OR_discrimination_SES_values_CI_upper = OR_discrimination_SES$measure[2, 3]
  
  discrimination_chi_value_SES = test_discrimination_AND_SES$statistic
  discrimination_pvalue_SES = test_discrimination_AND_SES$p.value
  
  
  SES_discrimination_findings = cbind(analysis_variable_name,
                                      
                                      country,
                                      
                                      N_subset, 
                                      
                                      N_discrimYES, 
                                      
                                      N_low_SES, 
                                      n_discrimYES_low_SES, 
                                      N_high_SES, 
                                      n_discrimYES_high_SES, 
                                      
                                      Odds_ratio,
                                      Odds_ratio_CI_lower,
                                      Odds_ratio_CI_upper,
                                      
                                      OR_discrimination_SES_value, 
                                      OR_discrimination_SES_values_CI_lower, 
                                      OR_discrimination_SES_values_CI_upper, 
                                      
                                      discrimination_chi_value_SES,
                                      discrimination_pvalue_SES)
  
  return(SES_discrimination_findings)
} 