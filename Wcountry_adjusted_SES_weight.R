
Wcountry_adjusted_SES_weight = function (data, 
                                  country, 
                                  
                                  analysis_variable_name,
                                  
                                  subsetting_VAR1, 
                                  subsetting_VAR2, 
                                  var1_value,
                                  var2_value,
                                  
                                  covariate1, 
                                  covariate2,
                                  covariate3, 
                                  covariate4, 
                                  
                                  discrimination_VAR){
  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  


if (subsetting_VAR1 == "NA" & subsetting_VAR2 =="NA" ){
  
  data_subset = data
} 

if (subsetting_VAR1 != "NA" & subsetting_VAR2 =="NA" ){
  
  data_subset = subset(data, data[ , subsetting_VAR1] > var1_value)
  
} 

if (subsetting_VAR1 != "NA" & subsetting_VAR2 !="NA" ){
  
  data_subset = subset(data,  data[ , subsetting_VAR1] > var1_value & data[subsetting_VAR2] <= var2_value)
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
  
  data_both_countries = cbind(SES, discrimination)
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 
  
  # when only covariate 1 is included (i.e, not NA, !=NA)  and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    
    data_both_countries$covariate1 = data_subset[ , covariate1]
    
    
    fm1 <- glm(discrimination ~  covariate1, 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ SES 
               + covariate1 , 
               
               data = data_both_countries)
  }
  
  # when  covariate 1 and covariate 2  (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 == "NA" &  covariate4 == "NA"){
    
    
    data_both_countries$covariate1= data_subset[, covariate1]
    
    
    data_both_countries$covariate2 = data_subset[, covariate2]
    
    
    
    
    fm1 <- glm(discrimination ~ covariate1
               + covariate2, 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ SES 
               +  covariate1
               + covariate2, 
               
               data = data_both_countries)
  }
  
  # when  covariate 1 and covariate 2 and covariate 3 (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 == "NA"){
    
    data_both_countries$covariate1 = data_subset[, covariate1]
    
    
    data_both_countries$covariate2 = data_subset[, covariate2]
    
    data_both_countries$covariate3 = data_subset[, covariate3]
    
    
    fm1 <- glm(discrimination ~   covariate1
               +  covariate2
               +  covariate3, 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ SES 
               +  covariate1
               +  covariate2
               +  covariate3, 
               
               data = data_both_countries)
    
  } 
  
  # when  covariate 1 and covariate 2 and covariate 3 and covariate 4 (i.e, not NA, !=NA)  are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 != "NA"){
    
    data_both_countries$covariate1 = data_subset[, covariate1]
    
    data_both_countries$covariate2  = data_subset[, covariate2]
    
    
    data_both_countries$covariate3 = data_subset[, covariate3]
    
    data_both_countries$covariate4 = data_subset[, covariate4]
    
    
    
    fm1 <- glm(discrimination ~  covariate1
               +  covariate2
               +  covariate3
               + covariate4, 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ SES 
               +  covariate1
               +  covariate2
               +  covariate3
               +  covariate4, 
               
               data = data_both_countries)
    
    
  }
  
  SES_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  SES_OR_SES_high = SES_OR[2, 1]
  CI1_SES_high = SES_OR[2, 2]
  CI2_SES_high = SES_OR[2, 3]
  
  
  SES_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  SES_OR_low_SES = SES_OR[1, 1]
  CI1_low_SES = SES_OR[1, 2]
  CI2_low_SES = SES_OR[1, 3]
  
  ## various equivalent specifications of the LR test
  SES_diff = lrtest(fm1, fm2)
  
  chi_value_SES = SES_diff$stats[1]
  pvalue_SES = SES_diff$stats[3]
  
  
  
  SES_findings = cbind(analysis_variable_name, 
                       
                       country, 
                       
                       
                       N_low_SES,
                       N_high_SES,
                       
                       n_discrimYES_low_SES,
                       n_discrimYES_high_SES,
                       
                       SES_OR_low_SES, 
                       CI1_low_SES,
                       CI2_low_SES,
                       
                       SES_OR_SES_high, 
                       CI1_SES_high, 
                       CI2_SES_high, 
                       
                       chi_value_SES,
                       pvalue_SES)
  
  #ELSA_OR_value,
  #ELSA_CI1,
  #ELSA_CI2,
  #HRS_OR_value,
  #HRS_CI1,
  #HRS_CI2)
  
  
  return(SES_findings)
}