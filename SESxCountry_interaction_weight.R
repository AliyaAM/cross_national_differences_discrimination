
SESxCountry_interaction_weight = function (data_ELSA, 
                                            data_HRS,
                                            
                                            analysis_variable_name, 
                                            
                                            subsetting_VAR1_ELSA, 
                                            subsetting_VAR1_HRS,
                                            
                                            subsetting_VAR2_ELSA, 
                                            subsetting_VAR2_HRS,
                                            
                                            ELSA_var1_value,
                                            HRS_var1_value,
                                            
                                            ELSA_var2_value,
                                            HRS_var2_value,
                                            
                                            
                                            discrimination_VAR_elsa,
                                            discrimination_VAR_hrs){
  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  
  
  #data_HRS <- data_HRS[ , subsetting_VAR_HRS]
  #data_ELSA <- data_ELSA[ , subsetting_VAR_ELSA]
  
  
  # subsetting data to the right variable for the analysis (eg, sex, physical lim.)
  # if there is only one subsetting var: subsetting_VAR1_ELSA and subsetting_VAR1_HRS
  
  # a.	Obese is BMI>29.9 b.	Overweight is 25-29.9 >25 but =<29.9 c.	Obese and overweight >25
  
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
  

  
  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  
  data_both_countries$SES_both =  c(data_ELSA_subset$SES, 
                                    data_HRS_subset$SES)
  
  
  
  
  
  ################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
  ################ ####Rippon:  in  order  to  determine  any  cross-national  differences  in  age  discrimination. 
  
  
  fm1 <- glm(discrimination ~  country_cat + SES_both, 
             data = data_both_countries)
  
  fm2<- glm(discrimination ~ country_cat + SES_both + country_cat*SES_both, 
            data = data_both_countries)
  print("done")
  
  
  ## various equivalent specifications of the LR test
  
  OR_fm2 = exp(cbind(OR = coef(fm2), confint(fm2)))
  
  
  cross_national_diff= lrtest(fm1, fm2)
  
  results_interaction_model = summary(fm2)
  
  results_interaction_model_coeffcients = results_interaction_model$coefficients
  SESxCountry = cbind(results_interaction_model_coeffcients, cross_national_diff)
  
  SES_country_interaction = cbind(OR_fm2, results_interaction_model_coeffcients)
  
  var_name = rbind(analysis_variable_name, analysis_variable_name, analysis_variable_name, analysis_variable_name)
  
  results = cbind(SES_country_interaction, var_name)
  
  return(results)
}
