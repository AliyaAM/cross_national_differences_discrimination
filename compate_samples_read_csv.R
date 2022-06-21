
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"


ALL_age = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "age_compare.csv", sep = ""))  
colnames(ALL_age) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

ALL_sex = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "sex_diff.csv", sep = ""))  
colnames(ALL_sex) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
#ALL_sex = cbind(ALL_sex, sex_name)
ALL_wealth = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "wealth_compare.csv", sep = ""))  
colnames(ALL_wealth) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

ALL_BMI = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "BMI_compare.csv", sep = ""))  
colnames(ALL_BMI) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")


disability_age = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "age_compare.csv", sep = ""))
colnames(disability_age) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
disability_sex = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "sex_diff.csv", sep = ""))  
colnames(disability_sex) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

#disability_sex = cbind(disability_sex, sex_name)
disability_wealth = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "wealth_compare.csv", sep = ""))  
colnames(disability_wealth) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

disability_BMI = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "BMI_compare.csv", sep = ""))  
colnames(disability_BMI) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")



financial_age = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "age_compare.csv", sep = ""))  
colnames(financial_age) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

financial_sex = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "sex_diff.csv", sep = ""))  
colnames(financial_sex) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

#financial_sex = cbind(financial_sex, sex_name)
financial_wealth = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "wealth_compare.csv", sep = ""))  
colnames(financial_wealth) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
financial_BMI = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "BMI_compare.csv", sep = ""))  
colnames(financial_BMI) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")



sex_female_age = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "age_compare.csv", sep = ""))  
colnames(sex_female_age) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

sex_female_wealth = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "wealth_compare.csv", sep = ""))  
colnames(sex_female_wealth) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

sex_female_BMI = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "BMI_compare.csv", sep = ""))  
colnames(sex_female_BMI) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")




race_age = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "age_compare.csv", sep = ""))  
colnames(race_age) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

race_sex = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "sex_diff.csv", sep = ""))  
colnames(race_sex) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

#race_sex = cbind(race_sex, sex_name)
race_wealth = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "wealth_compare.csv", sep = ""))  
colnames(race_wealth) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

race_BMI = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "BMI_compare.csv", sep = ""))  
colnames(race_BMI) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")



BMI_30_age = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "age_compare.csv", sep = "")) 
colnames(BMI_30_age) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

BMI_30_sex = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "sex_diff.csv", sep = ""))  
colnames(BMI_30_sex) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

#BMI_30_sex = cbind(BMI_30_sex, sex_name)

BMI_30_wealth = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "wealth_compare.csv", sep = ""))  
colnames(BMI_30_wealth) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

BMI_30_BMI = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "BMI_compare.csv", sep = ""))  
colnames(BMI_30_BMI) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")


###################
###################




print("change names for sex dataframe so they are consistent with the rest")
print("change names for sex dataframe so they are consistent with the rest")
print("change names for sex dataframe so they are consistent with the rest")
print("change names for sex dataframe so they are consistent with the rest")
print("change names for sex dataframe so they are consistent with the rest")
print("change names for sex dataframe so they are consistent with the rest")

 Participant_characteristics_table  =   rbind(ALL_age,  
                                              ALL_sex, 
                                              ALL_wealth, 
                                              ALL_BMI, 
                                              
                                              disability_age,
                                              disability_sex,
                                              disability_wealth,
                                              disability_BMI,
                                              
                                              financial_age,
                                              financial_sex,
                                              financial_wealth,
                                              financial_BMI,
                                              
                                              sex_female_age, 
                                              sex_female_wealth, 
                                              sex_female_BMI, 
                                              
                                              race_age, 
                                              race_sex,
                                              race_wealth, 
                                              race_BMI, 
                                              
                                              BMI_30_age, 
                                              BMI_30_sex,
                                              BMI_30_wealth, 
                                              BMI_30_BMI) 
                                      
                                      
          

table_names =  c("",
                 "ALL_age",  
                 "",
                 "ALL_sex", 
                 "",
                 "ALL_wealth", 
                 "",
                 "ALL_BMI", 
                 "",
                 "disability_age",
                 "",
                 "disability_sex",
                 "",
                 "disability_wealth",
                 "", 
                 "disability_BMI",
                 "",
                 "financial_age",
                 "",
                 "financial_sex",
                 "",
                 "financial_wealth",
                 "",
                 "financial_BMI",
                 "",
                 "sex_female_age", 
                 "",
                 "sex_female_wealth", 
                 "",
                 "sex_female_BMI", 
                 "",
                 "race_age", 
                 "",
                 "race_sex",
                 "",
                 "race_wealth", 
                 "",
                 "race_BMI", 
                 "",
                 "BMI_30_age",
                 "",
                 "BMI_30_sex",
                 "",
                 "BMI_30_wealth", 
                 "",
                 "BMI_30_BMI")

nrow(Participant_characteristics_table)

Participant_characteristics_table = cbind(table_names, Participant_characteristics_table)

write.csv(Participant_characteristics_table, paste(OUTPUT_ROOT, "Participant_characteristics_table_differences.csv", sep = "")) 

##############
##############



ALL_age_Participant_char = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
ALL_sex_Participant_char = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
ALL_wealth_Participant_char = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
ALL_BMI_Participant_char = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  


disability_age_Participant_char = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
disability_sex_Participant_char = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
disability_wealth_Participant_char = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
disability_BMI_Participant_char = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  


financial_age_Participant_char = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
financial_sex_Participant_char = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
financial_wealth_Participant_char = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
financial_BMI_Participant_char = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  



sex_female_age_Participant_char = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
sex_female_wealth_Participant_char = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
sex_female_BMI_Participant_char = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  




race_female_age_Participant_char = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
race_female_sex_Participant_char = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
race_female_wealth_Participant_char = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
race_female_BMI_Participant_char = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  



BMI_30_female_age_Participant_char = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = "")) 
BMI_30_female_sex_Participant_char = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
BMI_30_female_wealth_Participant_char = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = ""))  
BMI_30_female_BMI_Participant_char = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "Participant_char_sample_by_HRS_ELSA.csv", sep = "")) 



###########################
###########################
###########################

Participant_char_mean_sd = rbind(ALL_age_Participant_char,
                   
                                
                                
                                disability_age_Participant_char,
                             
                                
                                
                                financial_age_Participant_char,
                     
                                
                                sex_female_age_Participant_char,
                       
                                
                                race_female_age_Participant_char,
                              
                                
                                BMI_30_female_age_Participant_char) 



nrow(Participant_char_mean_sd)

table_names_mean_sd = c("ALL",
                        
                        
                        
                        "disability",
                        
                        
                        
                        "financial",
                        
                        
                        "sex_female",
                        
                        
                        "race",
                        
                        
                        "BMI_30") 


Participant_char_mean_sd = cbind(table_names_mean_sd, Participant_char_mean_sd)

write.csv(Participant_char_mean_sd, paste(OUTPUT_ROOT, "Participant_char_mean_sd.csv", sep = "")) 


##############
##############

ALL_age_ftest = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "age_ftest.csv", sep = ""))  
ALL_wealth_ftest = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "wealth_ftest.csv", sep = ""))  
ALL_BMI_ftest = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "BMI_ftest.csv", sep = ""))  


disability_age_ftest = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "age_ftest.csv", sep = ""))  
disability_wealth_ftest = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "wealth_ftest.csv", sep = ""))  
disability_BMI_ftest = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "BMI_ftest.csv", sep = ""))  


financial_age_ftest = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "age_ftest.csv", sep = ""))  
financial_wealth_ftest = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "wealth_ftest.csv", sep = ""))  
financial_BMI_ftest = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "BMI_ftest.csv", sep = ""))  



sex_female_age_ftest = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "age_ftest.csv", sep = ""))  
sex_female_wealth_ftest = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "wealth_ftest.csv", sep = ""))  
sex_female_BMI_ftest = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "BMI_ftest.csv", sep = ""))  




race_female_age_ftest = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "age_ftest.csv", sep = ""))  
race_female_wealth_ftest = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "wealth_ftest.csv", sep = ""))  
race_female_BMI_ftest = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "BMI_ftest.csv", sep = ""))  



BMI_30_female_age_ftest = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "age_ftest.csv", sep = ""))  
BMI_30_female_wealth_ftest = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "wealth_ftest.csv", sep = ""))  
BMI_30_female_BMI_ftest = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "BMI_ftest.csv", sep = ""))  



