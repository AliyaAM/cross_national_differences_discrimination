adjusted_cross_nat_comparison = function (data_ELSA, 
                                          data_HRS,
                                          analysis_variable_name, 
                                          subsetting_VAR_ELSA, 
                                          subsetting_VAR_HRS,
                                          ELSA_var_value,
                                          HRS_var_value,
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
  data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR_ELSA] == ELSA_var_value)
  data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR_HRS] == HRS_var_value)

  # calculate the number of cases for this subset 
  N_ELSA_subset = nrow(data_ELSA_subset)
  N_HRS_subset = nrow(data_HRS_subset)
  
  
  #rename the covariates
  data_ELSA_subset$age = data_ELSA_subset$w5age
  data_HRS_subset$age  = data_HRS_subset$continious_age
  
  
  data_ELSA_subset$sex = data_ELSA_subset$w5sex_1_0
  data_HRS_subset$sex = data_HRS_subset$sex_1_0
  
  data_ELSA_subset$education = data_ELSA_subset$ELSA_Education
  data_HRS_subset$education = data_HRS_subset$education_levels
  
  
  data_ELSA_subset$employment = data_ELSA_subset$employment
  data_HRS_subset$employment =  data_HRS_subset$employment_allCategories
  
  
  data_ELSA_subset$wealth = data_ELSA_subset$w5wealth
  data_HRS_subset$wealth = data_HRS_subset$wealth_noIRA_HRS2010
  
  
  data_ELSA_subset$wealth_quantiles = data_ELSA_subset$w5wealthq
  data_HRS_subset$wealth_quantiles =  data_HRS_subset$Percentile_wealth_noIRA_HRS2010
  
  data_ELSA_subset$married = data_ELSA_subset$w5married
  data_HRS_subset$married =  data_HRS_subset$married_bin
  
  #covariates pooled from ELSA and HRS  (make sure the order as above)
  #Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
  data_ELSA_subset$marital_status = data_ELSA_subset$w5married4
  data_HRS_subset$marital_status = data_HRS_subset$marital_status
  
  
  
  # dummy code the countries 
  data_ELSA_subset$country = rep(1, times = N_ELSA_subset)
  data_HRS_subset$country = rep(0, times = N_HRS_subset)
  

  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  
  data_both_countries$age = c(data_ELSA_subset$age,
                              data_HRS_subset$age)
  
  
  data_both_countries$sex = c(data_ELSA_subset$sex,
                              data_HRS_subset$sex)
  
  data_both_countries$education = c(data_ELSA_subset$education, 
                                    data_HRS_subset$education)
  
  
  data_both_countries$employment = c(data_ELSA_subset$employment,
                                     data_HRS_subset$employment)
  
  
  data_both_countries$wealth = c(data_ELSA_subset$wealth,
                                 data_HRS_subset$wealth)
  
  
  data_both_countries$wealth_quantiles = c(data_ELSA_subset$wealth_quantiles,
                                           data_HRS_subset$wealth_quantiles) 
  
  data_both_countries$married = c(data_ELSA_subset$married,
                                  data_HRS_subset$married)
  
  #covariates pooled from ELSA and HRS  (make sure the order as above)
  #Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
  data_ELSA_subset$marital_status = data_ELSA_subset$marital_status

  data_both_countries$marital_status = c(data_ELSA_subset$marital_status, 
                                         data_HRS_subset$marital_status)
  
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 

  if(data_both_countries[ ,   covariate1] != "NA" & data_both_countries[ ,   covariate2] != "NA" &    + data_both_countries[ ,   covariate3] != "NA" & + data_both_countries[ ,   covariate4]!= "NA" ){
  
  
  fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
             + data_both_countries[ ,   covariate2] 
             + data_both_countries[ ,   covariate3] 
             + data_both_countries[ ,   covariate4] , 
             
             data = data_both_countries)
  
  fm2 <- glm(discrimination ~ country_cat 
             + data_both_countries[ ,   covariate1]
             + data_both_countries[ ,   covariate2] 
             + data_both_countries[ ,   covariate3] 
             + data_both_countries[ ,   covariate4] , 

             data = data_both_countries)
  }
  
  
  if(data_both_countries[ ,   covariate1] != "NA" & data_both_countries[ ,   covariate2] != "NA" &    + data_both_countries[ ,   covariate3] != "NA" ){
    
    
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
  
  if(data_both_countries[ ,   covariate1] != "NA" & data_both_countries[ ,   covariate2] != "NA"){
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2], 
               
               data = data_both_countries)
  }else{
  
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
  } 
  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_UK = cross_country_OR[2]
  CI1_UK = cross_country_OR[6]
  CI2_UK = cross_country_OR[10]
  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_USA = cross_country_OR[1]
  CI1_USA = cross_country_OR[5]
  CI2_USA = cross_country_OR[9]
  
  ## various equivalent specifications of the LR test
  cross_national_diff = lrtest(fm1, fm2)
  
  chi_value_cross_national = cross_national_diff$stats[1]
  pvalue_cross_national = cross_national_diff$stats[3]
  
  cross_national_findings = cbind(analysis_variable_name, 
                                  N_ELSA_subset, 
                                  N_HRS_subset, 
                                  chi_value_cross_national,
                                  pvalue_cross_national,
                                             
                                  cross_country_OR_UK, 
                                  CI1_UK, 
                                  CI2_UK, 
                                             
                                  cross_country_OR_USA, 
                                  CI1_USA, 
                                  CI2_USA)
                                             
                                  #ELSA_OR_value,
                                  #ELSA_CI1,
                                  #ELSA_CI2,
                                  #HRS_OR_value,
                                  #HRS_CI1,
                                  #HRS_CI2)
  
  return(cross_national_findings)
}