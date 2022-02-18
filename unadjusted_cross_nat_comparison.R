
Unadjusted_cross_nat_comparison = function (data_ELSA, 
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
  
  country_cat = as.factor(country_cat)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  data_both_countries$wealth = c(data_ELSA_subset$wealth,
                                 data_HRS_subset$wealth)
  
  data_both_countries$age =  c(data_ELSA_subset$age,
                               data_HRS_subset$age)
  

###############
  
  data_both_countries = na.omit(data_both_countries)
  
  
  
  plot_wealth = ggplot(data_both_countries, aes(wealth, discrimination)) +
    #geom_point(alpha = 0.2) +
    geom_smooth(aes(colour = country_cat), method = "glm", method.args = list(family = "binomial"), fullrange = TRUE) +
    scale_colour_discrete(name="country",
                          breaks = c(0, 1), 
                          labels=c("United States", "England")) + 
    labs(
      title = analysis_variable_name, 
      x = "wealth excluding pension, USD",
      y = "Probability of perceived discrimination"
    )+
    scale_x_continuous(labels = comma, limits = c(-500000, 500000))+ 
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), limits = c(0, 1)) + 
    
    theme(text = element_text(size = 20), legend.justification=c(1,1), legend.position=c(1,1))
  
  
  
  plot_age = ggplot(data_both_countries, aes(age, discrimination)) +
    #geom_point(alpha = 0.2) +
    geom_smooth(aes(colour = country_cat), method = "glm", method.args = list(family = "binomial"), fullrange = TRUE) +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), limits = c(0, 1)) + 
    
    scale_colour_discrete(name="country",
                          breaks = c(0, 1), 
                          labels=c("United States", "England")) + 
    labs(
      title = analysis_variable_name, 
      x = "age, years",
      y = "Probability of perceived discrimination"
    )+
    theme(text = element_text(size = 20), legend.justification=c(1,1), legend.position=c(1,1))
  
  
  
  
  print(plot_wealth) 
  print(plot_age) 
  
  
  ############
  #outputting wealth gradient results 
  
  path <- OUTPUT_ROOT
  
  folder = paste(analysis_variable_name, "/", sep = "")
  
  dir.create(paste(path, folder, sep = ""))
  
  
  #wealth gradient 
  wealth_discrimination =  summary(glm(discrimination ~ wealth, family = "binomial", data = data_both_countries))
  wealth_gradient = wealth_discrimination$coefficients
  wealth_gradient = as.data.frame(wealth_gradient)
  
  
  write.csv(wealth_gradient, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination.csv", sep=""))
  
  ########
  ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
  
  
  wealth_discrimination_ELSA =  summary(glm(discrimination ~ wealth, family = "binomial", data = ELSA_subset_wealth_gradient))
  wealth_gradient_ELSA = wealth_discrimination_ELSA$coefficients
  wealth_gradient_ELSA = as.data.frame(wealth_gradient_ELSA)
  
  
  write.csv(wealth_gradient_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA.csv", sep=""))
  
  
  ########
  HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
  
  
  wealth_discrimination_HRS =  summary(glm(discrimination ~ wealth, family = "binomial", data = HRS_subset_wealth_gradient))
  wealth_gradient_HRS = wealth_discrimination_HRS$coefficients
  wealth_gradient_HRS = as.data.frame(wealth_gradient_HRS)
  
  
  write.csv(wealth_gradient_HRS, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_HRS.csv", sep=""))
  
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