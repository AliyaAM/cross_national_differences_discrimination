

wealth_gradient_function_adjust_age_only = function (data_ELSA, 
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
                                                     
                                                     version = version, 
                                                     
                                                     discrimination_VAR_elsa,
                                                     discrimination_VAR_hrs){
                  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  
  
  #data_HRS <- data_HRS[ , subsetting_VAR_HRS]
  #data_ELSA <- data_ELSA[ , subsetting_VAR_ELSA]
  
  
  # subsetting data to the right variable for the analysis (eg, sex, physical lim.)
  # if there is only one subsetting var: subsetting_VAR1_ELSA and subsetting_VAR1_HRS
  if (subsetting_VAR1_ELSA == "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS == "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = data_ELSA 
    data_HRS_subset = data_HRS
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value)
    data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR1_HRS] == HRS_var1_value)
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA !="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS != "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value & data_ELSA[subsetting_VAR2_ELSA] == ELSA_var2_value)
    data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS[ ,subsetting_VAR2_HRS] == HRS_var2_value)
  }
  
  # calculate the number of cases for this subset 
  N_ELSA_subset = nrow(data_ELSA_subset)
  N_HRS_subset = nrow(data_HRS_subset)
  
  
  #calculate the number of people who perceived this type of discrimination 
  
  ELSA_discrimYES_subset = subset(data_ELSA_subset, data_ELSA_subset[ , discrimination_VAR_elsa] == 1) 
  HRS_discrimYES_subset = subset(data_HRS_subset,  data_HRS_subset[ , discrimination_VAR_hrs] == 1)
  
  N_ELSA_discrimYES = nrow(ELSA_discrimYES_subset)
  N_HRS_discrimYES = nrow(HRS_discrimYES_subset)
  
  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  country_cat = as.factor(country_cat)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  #data_both_countries$discrimination = as.factor(data_both_countries$discrimination)
  
  data_both_countries$wealth = c(data_ELSA_subset$wealth,
                                 data_HRS_subset$wealth)
  
  data_both_countries$age =  c(data_ELSA_subset$age,
                               data_HRS_subset$age)
  
  
  data_both_countries$sex =  c(data_ELSA_subset$sex,
                               data_HRS_subset$sex)
  
  path <- OUTPUT_ROOT
  
  version = version
  
  folder = paste(analysis_variable_name,version, "/", sep = "")
  
  dir.create(paste(path, folder, sep = ""))
  
  
  
 
  ########
  ########
  ########
  ########

    
    #wealth gradient 
    wealth_discrimination_adjusted =  summary(glm(discrimination ~ wealth + age, 
                                                  family = "binomial", data = data_both_countries))
    
    wealth_gradient_adjusted  = wealth_discrimination_adjusted$coefficients
    #wealth_gradient_adjusted = as.data.frame(wealth_gradient_adjusted)
    
    write.csv(wealth_gradient_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_age.csv", sep=""))
    
    
    
    ########
    ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
    
    
    wealth_discrimination_ELSA_adjusted =  summary(glm(discrimination ~ wealth + age, 
                                                       family = "binomial", data = ELSA_subset_wealth_gradient))
    wealth_gradient_ELSA_adjusted = wealth_discrimination_ELSA_adjusted$coefficients
    wealth_gradient_ELSA_adjusted = as.data.frame(wealth_gradient_ELSA_adjusted)
    
    
    write.csv(wealth_gradient_ELSA_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_age.csv", sep=""))
    
    
    
    ########
    HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
    
    
    wealth_discrimination_HRS_adjusted =  summary(glm(discrimination ~ wealth + age, 
                                                      family = "binomial", data = HRS_subset_wealth_gradient))
    wealth_gradient_HRS_adjusted = wealth_discrimination_HRS_adjusted$coefficients
    wealth_gradient_HRS_adjusted = as.data.frame(wealth_gradient_HRS_adjusted)
    
    
    write.csv(wealth_gradient_HRS_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS_adjusted_age.csv", sep=""))
    
    


    #wealth gradient 
    wealth_discrimination_adjusted2 =  summary(glm(discrimination ~ wealth + age + sex, 
                                                   family = "binomial", data = data_both_countries))
    
    wealth_gradient_adjusted2  = wealth_discrimination_adjusted2$coefficients
    wealth_gradient_adjusted2 = as.data.frame(wealth_gradient_adjusted2)
    
    
    write.csv(wealth_gradient_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_age_sex.csv", sep=""))
    
    
    
    ########
    ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
    
    
    wealth_discrimination_ELSA_adjusted2 =  summary(glm(discrimination ~ wealth + age + sex, 
                                                        family = "binomial", data = ELSA_subset_wealth_gradient))
    
    wealth_gradient_ELSA_adjusted2 = wealth_discrimination_ELSA_adjusted2$coefficients
    wealth_gradient_ELSA_adjusted2 = as.data.frame(wealth_gradient_ELSA_adjusted2)
    
    
    
    write.csv(wealth_gradient_ELSA_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_age_sex.csv", sep=""))
    
    
    ########
    HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
    
    
    wealth_discrimination_HRS_adjusted2 =  summary(glm(discrimination ~ wealth + + age + sex, 
                                                       family = "binomial", data = HRS_subset_wealth_gradient))
    
    wealth_gradient_HRS_adjusted2 = wealth_discrimination_HRS_adjusted2$coefficients
    wealth_gradient_HRS_adjusted2 = as.data.frame(wealth_gradient_HRS_adjusted2)
    
    
    write.csv(wealth_gradient_HRS_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS_adjusted_age_sex.csv", sep=""))
    
    
    
  
  
 
  #########
  #########
  
  #outputting age gradient results 
  
  age_discrimination =  summary(glm(discrimination ~ age, family = "binomial", data = data_both_countries))
  age_gradient = age_discrimination$coefficients
  age_gradient = as.data.frame(age_gradient)
  
  write.csv(age_gradient, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination.csv", sep=""))
  
  
  ########
  ELSA_subset_age_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
  
  
  age_discrimination_ELSA =  summary(glm(discrimination ~ age, family = "binomial", data = ELSA_subset_age_gradient))
  age_gradient_ELSA = age_discrimination_ELSA$coefficients
  age_gradient_ELSA = as.data.frame(age_gradient_ELSA)
  
  
  write.csv(age_gradient_ELSA, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination_ELSA.csv", sep=""))
  
  
  ########
  HRS_subset_age_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
  
  
  age_discrimination_HRS =  summary(glm(discrimination ~ age, family = "binomial", data = HRS_subset_age_gradient))
  age_gradient_HRS = age_discrimination_HRS$coefficients
  age_gradient_HRS = as.data.frame(age_gradient_HRS)
  
  
  write.csv(age_gradient_HRS, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination_HRS.csv", sep=""))
  
}
