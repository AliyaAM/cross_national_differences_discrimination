
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"

ALL_age = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "age_compare.csv", sep = ""))  
ALL_wealth = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "wealth_compare.csv", sep = ""))  
ALL_BMI = read.csv(paste(OUTPUT_ROOT, "ALLversion_1/", "BMI_compare.csv", sep = ""))  


disability_age = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "age_compare.csv", sep = ""))  
disability_wealth = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "wealth_compare.csv", sep = ""))  
disability_BMI = read.csv(paste(OUTPUT_ROOT, "disabilityversion_1/", "BMI_compare.csv", sep = ""))  


financial_age = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "age_compare.csv", sep = ""))  
financial_wealth = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "wealth_compare.csv", sep = ""))  
financial_BMI = read.csv(paste(OUTPUT_ROOT, "financialversion_1/", "BMI_compare.csv", sep = ""))  



sex_female_age = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "age_compare.csv", sep = ""))  
sex_female_wealth = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "wealth_compare.csv", sep = ""))  
sex_female_BMI = read.csv(paste(OUTPUT_ROOT, "sex_femaleversion_1/", "BMI_compare.csv", sep = ""))  




race_female_age = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "age_compare.csv", sep = ""))  
race_female_wealth = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "wealth_compare.csv", sep = ""))  
race_female_BMI = read.csv(paste(OUTPUT_ROOT, "raceversion_1/", "BMI_compare.csv", sep = ""))  



BMI_30_female_age = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "age_compare.csv", sep = ""))  
BMI_30_female_wealth = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "wealth_compare.csv", sep = ""))  
BMI_30_female_BMI = read.csv(paste(OUTPUT_ROOT, "BMI_30version_1/", "BMI_compare.csv", sep = ""))  

 Participant_characteristics_table  =   rbind(ALL_age,  
                                              ALL_wealth, 
                                              ALL_BMI, 
                                              disability_age,
                                              disability_wealth,
                                              disability_BMI,
                                              financial_age,
                                              financial_wealth,
                                              financial_BMI,
                                              sex_female_age, 
                                              sex_female_wealth, 
                                              sex_female_BMI, 
                                              race_female_age, 
                                              race_female_wealth, 
                                              race_female_BMI, 
                                              BMI_30_female_age, 
                                              BMI_30_female_wealth, 
                                              BMI_30_female_BMI) 
                                      
                                      
          






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

