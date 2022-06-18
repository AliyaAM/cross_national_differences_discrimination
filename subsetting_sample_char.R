subsetting_sample_char = function (data_ELSA,
                                   data_HRS,
                
                                   version, 
                                   subset_name, 
                                   
                                   subsetting_VAR1_ELSA,
                                   subsetting_VAR1_HRS,
                                   
                                   subsetting_VAR2_ELSA, 
                                   subsetting_VAR2_HRS,
                                   
                                   ELSA_var1_value,
                                   HRS_var1_value,
                                   
                                   ELSA_var2_value,
                                   HRS_var2_value){
  
  #list the subsetting var name inside the function 

  
  
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
  
  #ELSA_discrimYES_subset = subset(data_ELSA_subset, data_ELSA_subset[ , discrimination_VAR_elsa] == 1) 
  #HRS_discrimYES_subset = subset(data_HRS_subset,  data_HRS_subset[ , discrimination_VAR_hrs] == 1)
  
  #N_ELSA_discrimYES = nrow(ELSA_discrimYES_subset)
  #N_HRS_discrimYES = nrow(HRS_discrimYES_subset)
  
  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  country_cat = as.factor(country_cat)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  #data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
  #                                       data_HRS_subset[ , discrimination_VAR_hrs] )
  
  #data_both_countries$discrimination = as.factor(data_both_countries$discrimination)
  

  
  data_both_countries$age =  c(data_ELSA_subset$age,
                               data_HRS_subset$age)
  
  
  data_both_countries$sex =  c(data_ELSA_subset$sex,
                               data_HRS_subset$sex)
  
  
  data_both_countries$BMI =  c(data_ELSA_subset$w4bmi_clean,
                               data_HRS_subset$HRS2010_BMI)
  
  
  data_both_countries$wealth = c(data_ELSA_subset$wealth,
                                 data_HRS_subset$wealth)
  
  
  #data_ELSA_subset$age
  #data_ELSA_subset$sex
  #data_ELSA_subset$wealth
  #data_ELSA_subset$w4bmi_clean
  #data_ELSA_subset$w5bmi_clean
  
  
  #data_HRS_subset$age
  #data_HRS_subset$sex
  #data_HRS_subset$wealth
  #data_HRS_subset$HRS2010_BMI
  
  
  #summary descriptive statistic 
  by(data_both_countries, data_both_countries$country_cat, summary)
  
  
  
  path <- OUTPUT_ROOT
  
  version = version
  subset_name = subset_name
  
  folder = paste(path, subset_name, version, "/", sep = "")
  
  #dir.create(paste(path, folder, sep = ""))
  
  pdf(file = paste(folder, "/age_compare_plot.pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  
  #plots for age
  age_plot = ggboxplot(data_both_countries, x = "country_cat", y = "age", 
            color = "country_cat", palette = c("#00AFBB", "#E7B800"),
            ylab = "age", xlab = "country_cat")
  
  
  print(age_plot)
  
  dev.off()
  
  
  #testing assumption of normality test 
  # Shapiro-Wilk normality test 
  SW_test_1 = with(data_both_countries, shapiro.test(age[country_cat == 0]))# should be >0.05
  # Shapiro-Wilk normality test 
  SW_test_1 = with(data_both_countries, shapiro.test(age[country_cat == 1])) # should be >0.05
  
  SW_test_1 = as.data.frame(SW_test_1)
  SW_test_2 = as.data.frame(SW_test_2)
  
  write.csv(SW_test_1, file = paste(folder, "/age_SW_test_1.csv")) 
  
  write.csv(SW_test_2, file = paste(folder, "/age_SW_test_2.csv")) 
  
  
  #teting that US sample and UK sample have the same variance 
  res.ftest_age <- var.test(age ~ country_cat, data = data_both_countries)
  res.ftest_age
  
  write.csv(res.ftest_age, file = paste(folder, "/age_ftest.csv")) 
  
  
  #t-test comparing differences in age between US sample and UK sample 
  res_age <- t.test(age ~ country_cat, data = data_both_countries, var.equal = TRUE)
  res_age
  
  write.csv(res_age, file = paste(folder, "/age_compare.csv")) 
  
#############
  
  
  #plots for wealth
  
  
  pdf(file = paste(folder, "/wealth_compare_plot.pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  
 wealth_plot =  ggboxplot(data_both_countries, x = "country_cat", y = "wealth", 
            color = "country_cat", palette = c("#00AFBB", "#E7B800"),
            ylab = "wealth", xlab = "country_cat")
  
  print(wealth_plot)
  
  dev.off()
  
  #testing assumption of normality test 
  # Shapiro-Wilk normality test 
  SW_test_1 = with(data_both_countries, shapiro.test(wealth[country_cat == 0]))# should be >0.05
  # Shapiro-Wilk normality test 
  SW_test_1 = with(data_both_countries, shapiro.test(wealth[country_cat == 1])) # should be >0.05
  
  SW_test_1 = as.data.frame(SW_test_1)
  SW_test_2 = as.data.frame(SW_test_2)
  
  write.csv(SW_test_1, file = paste(folder, "/wealth_SW_test_1.csv")) 
  
  write.csv(SW_test_2, file = paste(folder, "/wealth_SW_test_2.csv")) 
  
  
  #teting that US sample and UK sample have the same variance 
  res.ftest_wealth <- var.test(wealth ~ country_cat, data = data_both_countries)
  res.ftest_wealth
  
  write.csv(res.ftest_wealth, file = paste(folder, "/wealth_ftest.csv")) 
  
  
  #t-test comparing differences in wealth between US sample and UK sample 
  res_wealth <- t.test(wealth ~ country_cat, data = data_both_countries, var.equal = TRUE)
  res_wealth
  
  write.csv(res_wealth, file = paste(folder, "/wealth_compare.csv")) 
  
  
  #plots for BMI
  
  
  pdf(file = paste(folder, "/BMI_compare_plot.pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  
  
  
  BMI_plot =  ggboxplot(data_both_countries, x = "country_cat", y = "BMI", 
                           color = "country_cat", palette = c("#00AFBB", "#E7B800"),
                           ylab = "BMI", xlab = "country_cat")
  
  print(BMI_plot)
  
  dev.off()
  
  
  #testing assumption of normality test 
  # Shapiro-Wilk normality test 
  SW_test_1 = with(data_both_countries, shapiro.test(BMI[country_cat == 0]))# should be >0.05
  # Shapiro-Wilk normality test 
  SW_test_1 = with(data_both_countries, shapiro.test(BMI[country_cat == 1])) # should be >0.05
  
  SW_test_1 = as.data.frame(SW_test_1)
  SW_test_2 = as.data.frame(SW_test_2)
  
  write.csv(SW_test_1, file = paste(folder, "/BMI_SW_test_1.csv")) 
            
  write.csv(SW_test_2, file = paste(folder, "/BMI_SW_test_2.csv")) 
                      
            
  #teting that US sample and UK sample have the same variance 
  res.ftest_BMI <- var.test(BMI ~ country_cat, data = data_both_countries)
  res.ftest_BMI
  
  write.csv(res.ftest_BMI, file = paste(folder, "/BMI_ftest.csv")) 
            
  
  #t-test comparing differences in BMI between US sample and UK sample 
  res_BMI <- t.test(BMI ~ country_cat, data = data_both_countries, var.equal = TRUE)
  res_BMI
  
  write.csv(res_BMI, file = paste(folder, "/BMI_compare.csv")) 
  
 
}