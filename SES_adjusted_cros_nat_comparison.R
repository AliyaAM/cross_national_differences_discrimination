SES_adjusted_cros_nat_comparison = function (data_ELSA, 
                                          data_HRS,
                                          
                                          SES_level, 
                                          
                                          analysis_variable_name, 
                                          
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
  if (subsetting_VAR1_ELSA == "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS == "NA" & subsetting_VAR2_HRS == "NA"){
  
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA$SES == SES_level) 
    data_HRS_subset = subset(data_HRS, data_HRS$SES == SES_level) 
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA$SES == SES_level & data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value)
    data_HRS_subset = subset(data_HRS, data_HRS$SES == SES_level & data_HRS[ , subsetting_VAR1_HRS] == HRS_var1_value)
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA !="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS != "NA"){
    
    data_ELSA_subset = subset(data_ELSA,  data_ELSA$SES == SES_level & data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value & data_ELSA[subsetting_VAR2_ELSA] == ELSA_var2_value)
    data_HRS_subset = subset(data_HRS, data_HRS$SES == SES_level & data_HRS[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS[ ,subsetting_VAR2_HRS] == HRS_var2_value)
  }
  
  # calculate the number of cases for this subset 
  N_ELSA_subset = nrow(data_ELSA_subset)
  N_HRS_subset = nrow(data_HRS_subset)
  
  SES_level =   SES_level
  
  #calculate the number of people who perceived this type of discrimination 
  
  ELSA_discrimYES_subset = subset(data_ELSA_subset, data_ELSA_subset[ , discrimination_VAR_elsa] == 1) 
  HRS_discrimYES_subset = subset(data_HRS_subset,  data_HRS_subset[ , discrimination_VAR_hrs] == 1)
  
  N_ELSA_discrimYES = nrow(ELSA_discrimYES_subset)
  N_HRS_discrimYES = nrow(HRS_discrimYES_subset)
  
  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  
  
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 
  
  # when only covariate 1 is included (i.e, not NA, !=NA)  and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                             data_HRS_subset[ ,   covariate1])
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1] , 
               
               data = data_both_countries)
  }
  
  # when  covariate 1 and covariate 2  (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 == "NA" &  covariate4 == "NA"){
    
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                             data_HRS_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2],
                                             data_HRS_subset[ ,   covariate2])
    
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2] , 
               
               data = data_both_countries)
  }
  
  # when  covariate 1 and covariate 2 and covariate 3 (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 == "NA"){
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                             data_HRS_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2],
                                             data_HRS_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(data_ELSA_subset[ ,   covariate3], 
                                            data_HRS_subset[ ,   covariate3])
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3], 
               
               data = data_both_countries)
    
  } 
  
  # when  covariate 1 and covariate 2 and covariate 3 and covariate 4 (i.e, not NA, !=NA)  are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 != "NA"){
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                             data_HRS_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2],
                                             data_HRS_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(data_ELSA_subset[ ,   covariate3], 
                                            data_HRS_subset[ ,   covariate3])
    
    data_both_countries[ ,   covariate4]= c(data_ELSA_subset[ ,   covariate4], 
                                            data_HRS_subset[ ,   covariate4])
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    
  }
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_UK = cross_country_OR[2, 1]
  CI1_UK = cross_country_OR[2, 2]
  CI2_UK = cross_country_OR[2, 3]
  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_USA = cross_country_OR[1, 1]
  CI1_USA = cross_country_OR[1, 2]
  CI2_USA = cross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
  cross_national_diff = lrtest(fm1, fm2)
  
  chi_value_cross_national = cross_national_diff$Chisquared
  pvalue_cross_national = cross_national_diff$p.value
  
  
  
  cross_national_findings = cbind(analysis_variable_name, 
                                  
                                  SES_level, 
                        
                                  N_ELSA_subset, 
                                  N_HRS_subset, 
                                  
                                  N_ELSA_discrimYES, 
                                  N_HRS_discrimYES, 
                                  
                                  
                                  cross_country_OR_UK, 
                                  CI1_UK, 
                                  CI2_UK, 
                                  
                                  cross_country_OR_USA, 
                                  CI1_USA, 
                                  CI2_USA,
                                  
                                  chi_value_cross_national,
                                  pvalue_cross_national)
  
  #ELSA_OR_value,
  #ELSA_CI1,
  #ELSA_CI2,
  #HRS_OR_value,
  #HRS_CI1,
  #HRS_CI2)
  
  
  return(cross_national_findings)
}