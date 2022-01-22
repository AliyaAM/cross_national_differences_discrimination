
library(emmeans)

vart_test = rbinom(100, 1, 0.5)
var1_test = rbinom(100, 1, 0.8)
 

contengency_table  = table(vart_test, var1_test)

print(contengency_table)

test_vart_test_AND_var1_test = chisq.test(contengency_table)

summary(test_vart_test_AND_var1_test)

OR_vart_test = oddsratio.wald(contengency_table)

OR_vart_testvalue = OR_vart_test$measure

OR_vart_test$measure

N_ELSA_discrim_YES = 40

ELSA_subset = 100 

N_ELSA_discrim_NO = 60 


probability_disc_ELSA = N_ELSA_discrim_YES/ELSA_subset 
probability_no_disc_ELSA = 1 - (probability_disc_ELSA) 

Odds_yes = probability_disc_ELSA/(1-probability_disc_ELSA)

Odds_no = probability_no_disc_ELSA/(1-probability_no_disc_ELSA)

Odds_ratio = Odds_yes/Odds_no

CI_lower = exp(log(Odds_ratio) - 1.96 * sqrt(1/N_ELSA_discrim_YES + 1/N_ELSA_discrim_NO))

CI_upper = exp(log(Odds_ratio) + 1.96 * sqrt(1/N_ELSA_discrim_YES + 1/N_ELSA_discrim_NO))

confint(Odds_ratio)


################
#Obese is BMI>29.9
#	Overweight is 25-29.9 >25 but =<29.9 
#	Obese and overweight >25

adjusted_cross_nat_weight_29_9_results = adjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                 
                                                                 analysis_variable_name = "weight discrimination,  BMI>29.9",
                                                                 
                                                                 
                                                                 subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                 subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                 
                                                                 
                                                                 ELSA_var1_value = 29.9, 
                                                                 HRS_var1_value  = 29.9,  
                                                                 
                                                                 subsetting_VAR2_ELSA = "NA", 
                                                                 subsetting_VAR2_HRS = "NA",
                                                                 
                                                                 #low SES
                                                                 ELSA_var2_value = "NA", 
                                                                 HRS_var2_value = "NA", 
                                                                 
                                                                 
                                                                 covariate1 = "age", 
                                                                 covariate2 = "sex", 
                                                                 covariate3 = "wealth",
                                                                 covariate4 = "employment",
                                                                 
                                                                 
                                                                 discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                 discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_29_9_results) 


adjusted_cross_nat_weight_25_results = adjusted_cross_nat_comparison_weight (data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                        
                                                                        analysis_variable_name = "weight discrimination,  BMI>25.0, <=29.9 ",
                                                                        
                                                                        
                                                                        subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                        subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                        
                                                                        ELSA_var1_value = 25.0, 
                                                                        HRS_var1_value  = 25.0, 
                                                                        
                                                              
                                                                        
                                                                        subsetting_VAR2_ELSA = "w4bmi_clean", 
                                                                        subsetting_VAR2_HRS = "HRS2010_BMI",
                                                                        
                                                                        ELSA_var2_value = 29.9, 
                                                                        HRS_var2_value = 29.9,  
                                                                        
                                                                      covariate1 = "age", 
                                                                      covariate2 = "sex", 
                                                                      covariate3 = "wealth",
                                                                      covariate4 = "employment",
                                                                        
                                                                        
                                                                        discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                        discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_25_results) 



adjusted_cross_nat_weight_both_results = adjusted_cross_nat_comparison(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting, 
                                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting, 
                                                                      
                                                                      analysis_variable_name = "weight discrimination,  BMI>25.0 ",
                                                                      
                                                                      
                                                                      subsetting_VAR1_ELSA =  "w4bmi_clean",  
                                                                      subsetting_VAR1_HRS =   "HRS2010_BMI", 
                                                                      
                                                                      
                                                                      ELSA_var1_value = 25.0, 
                                                                      HRS_var1_value = 25.0,  
                                                                      
                                                                      subsetting_VAR2_ELSA = "NA",
                                                                      subsetting_VAR2_HRS = "NA",
                                                                      
                                                                      #low SES
                                                                      ELSA_var2_value = "NA",
                                                                      HRS_var2_value  = "NA",
                                                                      
                                                                      
                                                                      covariate1 = "age", 
                                                                      covariate2 = "sex", 
                                                                      covariate3 = "wealth",
                                                                      covariate4 = "employment",
                                                                      
                                                                      discrimination_VAR_elsa = "w5weightdiscrimination2",
                                                                      discrimination_VAR_hrs = "HRS2010_reason_discrim1_reason_weight")


adjusted_results = rbind(adjusted_results, adjusted_cross_nat_weight_both_results) 
####################

facet = type_discrim 
ggplot(acs, aes(x = country, fill = SES)) +
  geom_bar(position = "dodge")


facet = country 
ggplot(acs, aes(x = SES, fill = type_discrim)) +
  geom_bar(position = "dodge")


facet = SES 
ggplot(acs, aes(x = country, fill = type_discrim)) +
  geom_bar(position = "dodge")
