

adjusted_cross_nat_comparison_weight = function (data_ELSA, 
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
                                          
                                          covariate1, 
                                          covariate2,
                                          covariate3, 
                                          covariate4, 
                                          
                                          wealth_gradient_cov1, 
                                          wealth_gradient_cov2, 
                                          wealth_gradient_cov3, 
                                          
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
  
  data_both_countries$wealth = c(data_ELSA_subset$wealth,
                                 data_HRS_subset$wealth)
  
  data_both_countries$age =  c(data_ELSA_subset$age,
                               data_HRS_subset$age)
  
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 
  
  # when only covariate 1 is included (i.e, not NA, !=NA)  and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    print("first if statement ")
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
    
    print("2 if statement ")
    
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
    
    print("3 if statement ")
    
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
    
    print("4 if statement ")
    
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
  #data_both_countries = na.omit(data_both_countries)
  
  
  print("if statements done ")
  
  
  
  ############
  #outputting wealth gradient results 
  
  path <- OUTPUT_ROOT
  
  folder = paste(analysis_variable_name, "/", sep = "")
  
  dir.create(paste(path, folder, sep = ""))
  
  if(wealth_gradient_cov1 == "NA" & wealth_gradient_cov2 == "NA" & wealth_gradient_cov3 == "NA"){
    
    
    #wealth gradient 
    wealth_discrimination =  summary(glm(scale(discrimination) ~ scale(wealth), family = "binomial", data = data_both_countries))
    wealth_gradient = wealth_discrimination$coefficients
    wealth_gradient = as.data.frame(wealth_gradient)
    write.csv(wealth_gradient, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination.csv", sep=""))
    
    wealth_gradient_coef = coef(wealth_discrimination)
    
    write.csv(wealth_gradient_coef, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_coef.csv", sep=""))
    
    
    
    ########
    ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
    
    
    wealth_discrimination_ELSA =  summary(glm(scale(discrimination) ~ scale(wealth), family = "binomial", data = ELSA_subset_wealth_gradient))
    wealth_gradient_ELSA = wealth_discrimination_ELSA$coefficients
    wealth_gradient_ELSA = as.data.frame(wealth_gradient_ELSA)
    
    
    write.csv(wealth_gradient_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA.csv", sep=""))
   
    wealth_gradient_coef_ELSA = coef(wealth_discrimination_ELSA)
    
    write.csv(wealth_gradient_coef_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_coef_ELSA.csv", sep=""))
    
    ########
    HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
    
    
    wealth_discrimination_HRS =  summary(glm(scale(discrimination) ~ scale(wealth), family = "binomial", data = HRS_subset_wealth_gradient))
    wealth_gradient_HRS = wealth_discrimination_HRS$coefficients
    wealth_gradient_HRS = as.data.frame(wealth_gradient_HRS)
    
    
    write.csv(wealth_gradient_HRS, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS.csv", sep=""))
  
      wealth_gradient_coef_HRS = coef(wealth_discrimination_HRS)
    
    write.csv(wealth_gradient_coef_HRS, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_coef_HRS.csv", sep=""))
    
  }
  ########
  ########
  ########
  ########
  if(wealth_gradient_cov1 != "NA" & wealth_gradient_cov2 == "NA" & wealth_gradient_cov3 == "NA"){
    
    data_both_countries[ ,   wealth_gradient_cov1] = c(data_ELSA_subset[ ,   wealth_gradient_cov1],
                                                       data_HRS_subset[ ,   wealth_gradient_cov1])
    
    
    #wealth gradient 
    wealth_discrimination_adjusted =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]), 
                                                  family = "binomial", data = data_both_countries))
    wealth_gradient_adjusted  = wealth_discrimination_adjusted$coefficients
    wealth_gradient_adjusted = as.data.frame(wealth_gradient_adjusted)
    
    
    write.csv(wealth_gradient_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_cov1.csv", sep=""))
    
    
    wealth_gradient_adjusted_coef = coef(wealth_discrimination_adjusted)
  
    write.csv(wealth_gradient_adjusted_coef, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef_cov1.csv", sep=""))
    
    
    ########
    ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
    
    
    wealth_discrimination_ELSA_adjusted =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]), 
                                                       family = "binomial", data = ELSA_subset_wealth_gradient))
    wealth_gradient_ELSA_adjusted = wealth_discrimination_ELSA_adjusted$coefficients
    wealth_gradient_ELSA_adjusted = as.data.frame(wealth_gradient_ELSA_adjusted)
    
    
    write.csv(wealth_gradient_ELSA_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_cov1.csv", sep=""))
    
    wealth_gradient_adjusted_coef_ELSA = coef(wealth_discrimination_ELSA_adjusted)
    
    write.csv(wealth_gradient_adjusted_coef_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef_ELSA_cov1.csv", sep=""))
    
    
    ########
    HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
    
    
    wealth_discrimination_HRS_adjusted =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]), 
                                                      family = "binomial", data = HRS_subset_wealth_gradient))
    wealth_gradient_HRS_adjusted = wealth_discrimination_HRS_adjusted$coefficients
    wealth_gradient_HRS_adjusted = as.data.frame(wealth_gradient_HRS_adjusted)
    
    
    write.csv(wealth_gradient_HRS_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS_adjusted_cov1.csv", sep=""))
   
     wealth_gradient_adjusted_coef_HRS = coef(wealth_discrimination_HRS_adjusted)
    
    write.csv(wealth_gradient_adjusted_coef_HRS, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef_HRS_cov1.csv", sep=""))
    
  }
  #########
  #########
  
  ########
  ########
  ########
  ########
  if(wealth_gradient_cov1 != "NA" & wealth_gradient_cov2 != "NA" & wealth_gradient_cov3 == "NA"){
    
    data_both_countries[ ,   wealth_gradient_cov1] = c(data_ELSA_subset[ ,   wealth_gradient_cov1],
                                                       data_HRS_subset[ ,   wealth_gradient_cov1])
    
    data_both_countries[ ,   wealth_gradient_cov2] = c(data_ELSA_subset[ ,   wealth_gradient_cov2],
                                                       data_HRS_subset[ ,   wealth_gradient_cov2])
    
    #wealth gradient 
    wealth_discrimination_adjusted2 =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]) + scale(data_both_countries[ ,   wealth_gradient_cov2]), 
                                                   family = "binomial", data = data_both_countries))
    
    wealth_gradient_adjusted2  = wealth_discrimination_adjusted2$coefficients
    wealth_gradient_adjusted2 = as.data.frame(wealth_gradient_adjusted2)
    
    
    write.csv(wealth_gradient_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_cov2.csv", sep=""))
    
    
    wealth_gradient_adjusted_coef2 = coef(wealth_discrimination_adjusted2)
    
    write.csv(wealth_gradient_adjusted_coef2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef2.csv", sep=""))
    
    ########
    ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
    
    
    wealth_discrimination_ELSA_adjusted2 =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]) + scale(data_both_countries[ ,   wealth_gradient_cov2]), 
                                                        family = "binomial", data = ELSA_subset_wealth_gradient))
    
    wealth_gradient_ELSA_adjusted2 = wealth_discrimination_ELSA_adjusted2$coefficients
    wealth_gradient_ELSA_adjusted2 = as.data.frame(wealth_gradient_ELSA_adjusted2)
    
    wealth_gradient_adjusted_ELSA_coef2 = coef(wealth_discrimination_ELSA_adjusted2)
    write.csv(wealth_gradient_adjusted_ELSA_coef2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_ELSA_coef2.csv", sep=""))
    
    
    write.csv(wealth_gradient_ELSA_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_cov2.csv", sep=""))
    
    
    ########
    HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
    
    
    wealth_discrimination_HRS_adjusted2 =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]) + scale(data_both_countries[ ,   wealth_gradient_cov2]), 
                                                       family = "binomial", data = HRS_subset_wealth_gradient))
    
    wealth_gradient_HRS_adjusted2 = wealth_discrimination_HRS_adjusted2$coefficients
    wealth_gradient_HRS_adjusted2 = as.data.frame(wealth_gradient_HRS_adjusted2)
    
    
    write.csv(wealth_gradient_HRS_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS_adjusted_cov2.csv", sep=""))
    
    wealth_gradient_adjusted_HRS_coef2 = coef(wealth_discrimination_HRS_adjusted2)
    write.csv(wealth_gradient_adjusted_HRS_coef2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_HRS_coef2.csv", sep=""))
    
    
  }
  
  
  if(wealth_gradient_cov1 != "NA" & wealth_gradient_cov2 != "NA" & wealth_gradient_cov3 != "NA"){
    
    
    data_both_countries[ ,   wealth_gradient_cov1] = c(data_ELSA_subset[ ,   wealth_gradient_cov1],
                                                       data_HRS_subset[ ,   wealth_gradient_cov1])
    
    data_both_countries[ ,   wealth_gradient_cov2] = c(data_ELSA_subset[ ,   wealth_gradient_cov2],
                                                       data_HRS_subset[ ,   wealth_gradient_cov2])
    
    data_both_countries[ ,   wealth_gradient_cov3] = c(data_ELSA_subset[ ,   wealth_gradient_cov3],
                                                       data_HRS_subset[ ,   wealth_gradient_cov3])
    
    print(" TEST 0 stopped here")
    
    #wealth gradient 
    wealth_discrimination_adjusted3 =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]) + scale(data_both_countries[ ,   wealth_gradient_cov2]) + scale(data_both_countries[ ,   wealth_gradient_cov3]), 
                                                   family = "binomial", data = data_both_countries))
    
    wealth_gradient_adjusted3  = wealth_discrimination_adjusted3$coefficients
    wealth_gradient_adjusted3 = as.data.frame(wealth_gradient_adjusted3)
    
    
    write.csv(wealth_gradient_adjusted3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_cov3.csv", sep=""))
    
    wealth_gradient_adjusted_coef3 = coef(wealth_discrimination_adjusted3)
    write.csv(wealth_gradient_adjusted_coef3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef3.csv", sep=""))
    
    
    ########
    ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
    
    
    wealth_discrimination_ELSA_adjusted3 =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]) + scale(data_both_countries[ ,   wealth_gradient_cov2]) + scale(data_both_countries[ ,   wealth_gradient_cov3]), 
                                                        family = "binomial", data = ELSA_subset_wealth_gradient))
    
    wealth_gradient_ELSA_adjusted3 = wealth_discrimination_ELSA_adjusted3$coefficients
    wealth_gradient_ELSA_adjusted3 = as.data.frame(wealth_gradient_ELSA_adjusted3)
    
    
    write.csv(wealth_gradient_ELSA_adjusted3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_cov3.csv", sep=""))
    
    wealth_gradient_ELSA_adjusted_coef3 = coef(wealth_discrimination_ELSA_adjusted3)
    write.csv(wealth_gradient_ELSA_adjusted_coef3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_ELSA_adjusted_coef3.csv", sep=""))
    
    
    ########
    HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
    
    
    wealth_discrimination_HRS_adjusted3 =  summary(glm(scale(discrimination) ~ scale(wealth) + scale(data_both_countries[ ,   wealth_gradient_cov1]) + scale(data_both_countries[ ,   wealth_gradient_cov2]) + scale(data_both_countries[ ,   wealth_gradient_cov3]),
                                                       family = "binomial", data = HRS_subset_wealth_gradient))
    
    wealth_gradient_HRS_adjusted3 = wealth_discrimination_HRS_adjusted3$coefficients
    wealth_gradient_HRS_adjusted3 = as.data.frame(wealth_gradient_HRS_adjusted3)
    
    
    write.csv(wealth_gradient_HRS_adjusted3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS_adjusted_cov3.csv", sep=""))
    
    wealth_gradient_HRS_adjusted_coef3 = coef(wealth_discrimination_HRS_adjusted3)
    write.csv(wealth_gradient_HRS_adjusted_coef3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_HRS_adjusted_coef3.csv", sep=""))
    
    
  }
  #########
  #########
  
  print(" TEST stopped here")
  data_both_countries = na.omit(data_both_countries)
  
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
  
  
  #age_discrimination_HRS =  summary(glm(discrimination ~ age, family = "binomial", data = HRS_subset_age_gradient))
  #age_gradient_HRS = age_discrimination_HRS$coefficients
  #age_gradient_HRS = as.data.frame(age_gradient_HRS)
  
  
  #write.csv(age_gradient_HRS, file = paste(OUTPUT_ROOT, folder,  "age_gradient_discrimination_HRS.csv", sep=""))
  print("stopped here")
  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_UK = cross_country_OR[2, 1]
  CI1_UK = cross_country_OR[2, 2]
  CI2_UK = cross_country_OR[2, 3]
  
  print("stopped here 2")
  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_USA = cross_country_OR[1, 1]
  CI1_USA = cross_country_OR[1, 2]
  CI2_USA = cross_country_OR[1, 3]
  
  print("stopped here 3")
  
  ## various equivalent specifications of the LR test
  cross_national_diff = lrtest(fm1, fm2)
  
  print("stopped here 4")
  
  
  chi_value_cross_national = cross_national_diff$stats[1]
  pvalue_cross_national = cross_national_diff$stats[3]
  
  
  
  cross_national_findings = cbind(analysis_variable_name, 
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