library(epitools)

library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
library(dplyr)



Unadjusted_cross_nat_Situations_weight = function (data_ELSA, 
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
  
  
  country_cat = c(ELSA_discrimYES_subset$country,
                  HRS_discrimYES_subset$country)
  
  
  discrim_lessrespect = c(ELSA_discrimYES_subset$w5discrim_lessrespect_bin2,
                          HRS_discrimYES_subset$HRS2010_discrim_lessrespect_bin)
  
  
  #Received poorer service or treatment than other people from doctors or hospitals
  unique(ELSA_discrimYES_subset$w5discrim_medical_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_medical_bin)
  
  discrim_medical = c(ELSA_discrimYES_subset$w5discrim_medical_bin2, 
                      HRS_discrimYES_subset$HRS2010_discrim_medical_bin)
  
  #People act as if they think you are not clever or smart
  unique(ELSA_discrimYES_subset$w5discrim_notclever_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_notclever_bin)
  
  discrim_notclever = c(ELSA_discrimYES_subset$w5discrim_notclever_bin2, 
                        HRS_discrimYES_subset$HRS2010_discrim_notclever_bin)
  
  #Received poorer service than others in a restaurant or shop
  unique(ELSA_discrimYES_subset$w5discrim_poorerservice_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_poorerservice_bin)
  
  discrim_poor_service = c(ELSA_discrimYES_subset$w5discrim_poorerservice_bin2,
                           HRS_discrimYES_subset$HRS2010_discrim_poorerservice_bin)
  
  #You are threatened or harassed
  unique(ELSA_discrimYES_subset$w5discrim_harassed_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_harassed_bin)
  
  discrim_harrased = c(ELSA_discrimYES_subset$w5discrim_harassed_bin2, 
                       HRS_discrimYES_subset$HRS2010_discrim_harassed_bin)
  
  
  data_both_countries_discrim <- data.frame(country_cat, 
                                            
                                            
                                            discrim_lessrespect, 
                                            discrim_medical,
                                            discrim_notclever, 
                                            discrim_poor_service, 
                                            discrim_harrased)
  
  
  
  ############## less respect 
  
  contengency_table_discrim_lessrespect = table(data_both_countries_discrim$discrim_lessrespect,
                                                data_both_countries_discrim$country_cat)
  
  test_contengency_tablediscrim_lessrespect = chisq.test(contengency_table_discrim_lessrespect)
  test_contengency_tablediscrim_lessrespect
  ## various equivalent specifications of the LR test
  #weight_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)
  summary(test_contengency_tablediscrim_lessrespect)
  
  OR_cross_nationaldiscrim_lessrespect = oddsratio.wald(data_both_countries_discrim$discrim_lessrespect,
                                                        data_both_countries_discrim$country_cat)
  
  OR_cross_national_discrim_lessrespect_values = OR_cross_nationaldiscrim_lessrespect$measure
  OR_cross_national_discrim_lessrespect_values
  
  discrim_lessrespectchi_value_cross_national = test_contengency_tablediscrim_lessrespect$statistic
  discrim_lessrespectpvalue_cross_national = test_contengency_tablediscrim_lessrespect$p.value
  
  #########################################
  
  
  
  ############## discrim_medical
  
  contengency_table_discrim_medical = table(data_both_countries_discrim$discrim_medical,
                                            data_both_countries_discrim$country_cat)
  test_contengency_tablediscrim_medical = chisq.test(contengency_table_discrim_medical)
  test_contengency_tablediscrim_medical
  ## various equivalent specifications of the LR test
  #weight_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)
  summary(test_contengency_tablediscrim_medical)
  
  OR_cross_nationaldiscrim_medical = oddsratio.wald(data_both_countries_discrim$discrim_medical,
                                                    data_both_countries_discrim$country_cat)
  OR_cross_national_discrim_medical_values = OR_cross_nationaldiscrim_medical$measure
  OR_cross_national_discrim_medical_values
  
  discrim_medicalchi_value_cross_national = test_contengency_tablediscrim_medical$statistic
  discrim_medicalpvalue_cross_national = test_contengency_tablediscrim_medical$p.value
  
  #########################################
  
  
  
  ############## discrim_notclever
  
  contengency_table_discrim_notclever = table(data_both_countries_discrim$discrim_notclever,
                                              data_both_countries_discrim$country_cat)
  test_contengency_tablediscrim_notclever = chisq.test(contengency_table_discrim_notclever)
  test_contengency_tablediscrim_notclever
  ## various equivalent specifications of the LR test
  #weight_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)
  summary(test_contengency_tablediscrim_notclever)
  
  OR_cross_nationaldiscrim_notclever = oddsratio.wald(data_both_countries_discrim$discrim_notclever,
                                                      data_both_countries_discrim$country_cat)
  OR_cross_national_discrim_notclever_values = OR_cross_nationaldiscrim_notclever$measure
  OR_cross_national_discrim_notclever_values
  
  discrim_notcleverchi_value_cross_national = test_contengency_tablediscrim_notclever$statistic
  discrim_notcleverpvalue_cross_national = test_contengency_tablediscrim_notclever$p.value
  
  #########################################
  
  
  ############## discrim_poor_service
  
  contengency_table_discrim_poor_service = table(data_both_countries_discrim$discrim_poor_service,
                                                 data_both_countries_discrim$country_cat)
  test_contengency_tablediscrim_poor_service = chisq.test(contengency_table_discrim_poor_service)
  test_contengency_tablediscrim_poor_service
  ## various equivalent specifications of the LR test
  #weight_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)
  summary(test_contengency_tablediscrim_poor_service)
  
  OR_cross_nationaldiscrim_poor_service = oddsratio.wald(data_both_countries_discrim$discrim_poor_service,
                                                         data_both_countries_discrim$country_cat)
  OR_cross_national_discrim_poor_service_values = OR_cross_nationaldiscrim_poor_service$measure
  OR_cross_national_discrim_poor_service_values
  
  discrim_poor_servicechi_value_cross_national = test_contengency_tablediscrim_poor_service$statistic
  discrim_poor_servicepvalue_cross_national = test_contengency_tablediscrim_poor_service$p.value
  
  #########################################
  
  
  
  ############## discrim_harrased
  
  contengency_table_discrim_harrased = table(data_both_countries_discrim$discrim_harrased,
                                             data_both_countries_discrim$country_cat)
  test_contengency_tablediscrim_harrased = chisq.test(contengency_table_discrim_harrased)
  test_contengency_tablediscrim_harrased
  ## various equivalent specifications of the LR test
  #weight_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)
  summary(test_contengency_tablediscrim_harrased)
  
  OR_cross_nationaldiscrim_harrased = oddsratio.wald(data_both_countries_discrim$discrim_harrased,
                                                     data_both_countries_discrim$country_cat)
  OR_cross_national_discrim_harrased_values = OR_cross_nationaldiscrim_harrased$measure
  OR_cross_national_discrim_harrased_values
  
  discrim_harrasedchi_value_cross_national = test_contengency_tablediscrim_harrased$statistic
  discrim_harrasedpvalue_cross_national = test_contengency_tablediscrim_harrased$p.value
  
  #########################################
  
  findings_discrim_lessrespect = cbind(OR_cross_national_discrim_lessrespect_values,
                                       discrim_lessrespectchi_value_cross_national,
                                       discrim_lessrespectpvalue_cross_national)
  
  findings_discrim_medical = cbind(OR_cross_national_discrim_medical_values,
                                   discrim_medicalchi_value_cross_national,
                                   discrim_medicalpvalue_cross_national)
  
  findings_discrim_notclever = cbind(OR_cross_national_discrim_notclever_values,
                                     discrim_notcleverchi_value_cross_national,
                                     discrim_notcleverpvalue_cross_national)
  
  findings_discrim_poor_service = cbind(OR_cross_national_discrim_poor_service_values,
                                        discrim_poor_servicechi_value_cross_national,
                                        discrim_poor_servicepvalue_cross_national)
  
  findings_discrim_harrased = cbind(OR_cross_national_discrim_harrased_values,
                                    discrim_harrasedchi_value_cross_national,
                                    discrim_harrasedpvalue_cross_national)
  
  
  findings1 = rbind(findings_discrim_lessrespect, 
                    findings_discrim_medical,
                    findings_discrim_notclever,
                    findings_discrim_poor_service, 
                    findings_discrim_harrased)
  
  
  rownames_situations = c("discrim_lessrespect_USA",
                          "discrim_lessrespect_UK",
                          "discrim_medical_USA",
                          "discrim_medical_UK",
                          "discrim_notclever_USA",
                          "discrim_notclever_UK",
                          "discrim_poor_service_USA",
                          "discrim_poor_service_UK",
                          "discrim_harrased_USA",
                          "discrim_harrased_UK")
  
  findings = data.frame(analysis_variable_name, rownames_situations, 
                        findings1, stringsAsFactors = TRUE)
  
  return(findings)
}

