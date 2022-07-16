
print("if statements done ")



############
#outputting wealth gradient results 

path <- OUTPUT_ROOT

folder = paste(analysis_variable_name, "/", sep = "")

dir.create(paste(path, folder, sep = ""))

if(wealth_gradient_cov1 == "NA" & wealth_gradient_cov2 == "NA" & wealth_gradient_cov3 == "NA"){
  
  
  #wealth gradient 
  wealth_discrimination =  summary(glm(discrimination ~ wealth , family = "binomial", data = data_both_countries))
  wealth_gradient = wealth_discrimination$coefficients
  wealth_gradient = as.data.frame(wealth_gradient)
  write.csv(wealth_gradient, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination.csv", sep=""))
  
  wealth_gradient_coef = coef(wealth_discrimination)
  
  write.csv(wealth_gradient_coef, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_coef.csv", sep=""))
  
  
  
  ########
  ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
  
  
  wealth_discrimination_ELSA =  summary(glm(discrimination ~ wealth, family = "binomial", data = ELSA_subset_wealth_gradient))
  wealth_gradient_ELSA = wealth_discrimination_ELSA$coefficients
  wealth_gradient_ELSA = as.data.frame(wealth_gradient_ELSA)
  
  
  write.csv(wealth_gradient_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA.csv", sep=""))
  
  wealth_gradient_coef_ELSA = coef(wealth_discrimination_ELSA)
  
  write.csv(wealth_gradient_coef_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_coef_ELSA.csv", sep=""))
  
  ########
  HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
  
  
  wealth_discrimination_HRS =  summary(glm(discrimination ~ wealth, family = "binomial", data = HRS_subset_wealth_gradient))
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
  wealth_discrimination_adjusted =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1], 
                                                family = "binomial", data = data_both_countries))
  wealth_gradient_adjusted  = wealth_discrimination_adjusted$coefficients
  wealth_gradient_adjusted = as.data.frame(wealth_gradient_adjusted)
  
  
  write.csv(wealth_gradient_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_cov1.csv", sep=""))
  
  
  wealth_gradient_adjusted_coef = coef(wealth_discrimination_adjusted)
  
  write.csv(wealth_gradient_adjusted_coef, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef_cov1.csv", sep=""))
  
  
  ########
  ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
  
  
  wealth_discrimination_ELSA_adjusted =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1], 
                                                     family = "binomial", data = ELSA_subset_wealth_gradient))
  wealth_gradient_ELSA_adjusted = wealth_discrimination_ELSA_adjusted$coefficients
  wealth_gradient_ELSA_adjusted = as.data.frame(wealth_gradient_ELSA_adjusted)
  
  
  write.csv(wealth_gradient_ELSA_adjusted, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_cov1.csv", sep=""))
  
  wealth_gradient_adjusted_coef_ELSA = coef(wealth_discrimination_ELSA_adjusted)
  
  write.csv(wealth_gradient_adjusted_coef_ELSA, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef_ELSA_cov1.csv", sep=""))
  
  
  ########
  HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
  
  
  wealth_discrimination_HRS_adjusted =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1], 
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
  wealth_discrimination_adjusted2 =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1] + data_both_countries[ ,   wealth_gradient_cov2], 
                                                 family = "binomial", data = data_both_countries))
  
  wealth_gradient_adjusted2  = wealth_discrimination_adjusted2$coefficients
  wealth_gradient_adjusted2 = as.data.frame(wealth_gradient_adjusted2)
  
  
  write.csv(wealth_gradient_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_cov2.csv", sep=""))
  
  
  wealth_gradient_adjusted_coef2 = coef(wealth_discrimination_adjusted2)
  
  write.csv(wealth_gradient_adjusted_coef2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef2.csv", sep=""))
  
  ########
  ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
  
  
  wealth_discrimination_ELSA_adjusted2 =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1] + data_both_countries[ ,   wealth_gradient_cov2], 
                                                      family = "binomial", data = ELSA_subset_wealth_gradient))
  
  wealth_gradient_ELSA_adjusted2 = wealth_discrimination_ELSA_adjusted2$coefficients
  wealth_gradient_ELSA_adjusted2 = as.data.frame(wealth_gradient_ELSA_adjusted2)
  
  wealth_gradient_adjusted_ELSA_coef2 = coef(wealth_discrimination_ELSA_adjusted2)
  write.csv(wealth_gradient_adjusted_ELSA_coef2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_ELSA_coef2.csv", sep=""))
  
  
  write.csv(wealth_gradient_ELSA_adjusted2, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_cov2.csv", sep=""))
  
  
  ########
  HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
  
  
  wealth_discrimination_HRS_adjusted2 =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1] + data_both_countries[ ,   wealth_gradient_cov2], 
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
  wealth_discrimination_adjusted3 =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1] + data_both_countries[ ,   wealth_gradient_cov2] + data_both_countries[ ,   wealth_gradient_cov3], 
                                                 family = "binomial", data = data_both_countries))
  
  wealth_gradient_adjusted3  = wealth_discrimination_adjusted3$coefficients
  wealth_gradient_adjusted3 = as.data.frame(wealth_gradient_adjusted3)
  
  
  write.csv(wealth_gradient_adjusted3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_cov3.csv", sep=""))
  
  wealth_gradient_adjusted_coef3 = coef(wealth_discrimination_adjusted3)
  write.csv(wealth_gradient_adjusted_coef3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_adjusted_coef3.csv", sep=""))
  
  
  ########
  ELSA_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 1)
  
  
  wealth_discrimination_ELSA_adjusted3 =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1] + data_both_countries[ ,   wealth_gradient_cov2] + data_both_countries[ ,   wealth_gradient_cov3], 
                                                      family = "binomial", data = ELSA_subset_wealth_gradient))
  
  wealth_gradient_ELSA_adjusted3 = wealth_discrimination_ELSA_adjusted3$coefficients
  wealth_gradient_ELSA_adjusted3 = as.data.frame(wealth_gradient_ELSA_adjusted3)
  
  
  write.csv(wealth_gradient_ELSA_adjusted3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_discrimination_ELSA_adjusted_cov3.csv", sep=""))
  
  wealth_gradient_ELSA_adjusted_coef3 = coef(wealth_discrimination_ELSA_adjusted3)
  write.csv(wealth_gradient_ELSA_adjusted_coef3, file = paste(OUTPUT_ROOT, folder,  "wealth_gradient_ELSA_adjusted_coef3.csv", sep=""))
  
  
  ########
  HRS_subset_wealth_gradient = subset(data_both_countries, data_both_countries$country_cat == 0)
  
  
  wealth_discrimination_HRS_adjusted3 =  summary(glm(discrimination ~ wealth + data_both_countries[ ,   wealth_gradient_cov1] + data_both_countries[ ,   wealth_gradient_cov2] + data_both_countries[ ,   wealth_gradient_cov3],
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
