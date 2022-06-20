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
  
  
  
  path <- OUTPUT_ROOT
  
  version = version
  subset_name = subset_name
  
  folder = paste(subset_name, version, "/", sep = "")
  
  dir.create(paste(path, folder, sep = ""))
  # calculate the number of cases for this subset 
  N_ELSA_subset = nrow(data_ELSA_subset)
  N_HRS_subset = nrow(data_HRS_subset)
  
  
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  #country_cat = as.factor(country_cat)
  
  data_both_countries = data.frame(country_cat)
  
  print("ELSA n: ")
  print(nrow(data_ELSA_subset)) 
  
  
  print("HRS n: ")
  print(nrow(data_HRS_subset)) 
  
  print("both added")
  both_n = nrow(data_ELSA_subset) + nrow(data_HRS_subset)
  print(both_n)
  
  print("data_both_countries n: ")
  print(nrow(data_both_countries)) 
  
  
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
  
  
  
  
  data_both_countries$SES = c(data_ELSA_subset$median_wealth_bin_ELSA, 
                              data_HRS_subset$median_wealth_bin_HRS) 
  
  
  mean_ELSA_age = mean(data_ELSA_subset$age)
  mean_HRS_age = mean(data_HRS_subset$age)
  
  sd_ELSA_age = sd(data_ELSA_subset$age)
  sd_HRS_age = sd(data_HRS_subset$age) 
  
  data_ELSA_subset$w4bmi_clean = as.numeric(data_ELSA_subset$w4bmi_clean)
  mean_ELSA_BMI =  mean(data_ELSA_subset$w4bmi_clean,  na.rm = TRUE) 
  mean_HRS_BMI = mean(data_HRS_subset$HRS2010_BMI,  na.rm = TRUE)
  
  sd_ELSA_BMI =  sd(data_ELSA_subset$w4bmi_clean,  na.rm = TRUE) 
  sd_HRS_BMI = sd(data_HRS_subset$HRS2010_BMI,  na.rm = TRUE)
  
  data_ELSA_subset$wealth = as.numeric(data_ELSA_subset$wealth)
  mean_ELSA_wealth = mean(data_ELSA_subset$wealth, na.rm = TRUE)
  mean_HRS_wealth = mean(data_HRS_subset$wealth, na.rm = TRUE)
  
  sd_ELSA_wealth = sd(data_ELSA_subset$wealth , na.rm = TRUE)
  sd_HRS_wealth = sd(data_HRS_subset$wealth, na.rm = TRUE) 
  
  ######## 
  low_SES_ELSA = subset(data_ELSA_subset, data_ELSA_subset$median_wealth_bin_ELSA == 1) 
  low_SES_ELSA_n = nrow(low_SES_ELSA)
  
  #quantile(low_SES_ELSA_n, na.rm = TRUE, 1/4)
  #qnorm(low_SES_ELSA_n)
  #data_ELSA_subset$median_wealth_bin_ELSA = as.factor(data_ELSA_subset$median_wealth_bin_ELSA)
  #quantile(data_ELSA_subset$median_wealth_bin_ELSA,na.rm = TRUE, 3/4)
  
  low_SES_HRS = subset(data_HRS_subset, data_HRS_subset$median_wealth_bin_HRS == 1) 
  low_SES_HRS_n =nrow(low_SES_HRS)
  
  female_ELSA = subset(data_ELSA_subset, data_ELSA_subset$sex == 1) 
  female_ELSA_n = nrow(female_ELSA)
  
  female_HRS = subset(data_HRS_subset, data_HRS_subset$sex == 1) 
  female_HRS_n = nrow(female_HRS)
  
  
  #data_ELSA_subset$age
  #data_ELSA_subset$sex
  #data_ELSA_subset$wealth
  #data_ELSA_subset$w4bmi_clean
  #data_ELSA_subset$w5bmi_clean
  
  
  #data_HRS_subset$age
  #data_HRS_subset$sex
  #data_HRS_subset$wealth
  #data_HRS_subset$HRS2010_BMI
  
  
  
  ######
  ######
  ######
  
  
  Participant_char_sample_by_HRS_ELSA = cbind(mean_ELSA_age,
                                              mean_HRS_age ,
                                              
                                              sd_ELSA_age ,
                                              sd_HRS_age,
                                              
                                              mean_ELSA_BMI ,
                                              mean_HRS_BMI,
                                              
                                              sd_ELSA_BMI,
                                              sd_HRS_BMI,
                                              
                                              
                                              mean_ELSA_wealth,
                                              mean_HRS_wealth,
                                              
                                              sd_ELSA_wealth ,
                                              sd_HRS_wealth, 
                                              
                                              low_SES_ELSA_n ,
                                              
                                              
                                              low_SES_HRS_n ,
                                              
                                              female_ELSA_n,
                                              
                                              female_HRS_n) 
  
  
  
  write.csv(Participant_char_sample_by_HRS_ELSA, file = paste(path, folder, "Participant_char_sample_by_HRS_ELSA.csv", sep = "")) 
  
  
  #summary descriptive statistic 
  summary_descriptive_statistic = by(data_both_countries, data_both_countries$country_cat, summary)
  
  
  summary_descriptive_statistic_ELSA = summary_descriptive_statistic$`0`
  
  write.csv(summary_descriptive_statistic_ELSA, file = paste(path, folder, "summary_descriptive_statistic_ELSA.csv", sep = "")) 
  
  summary_descriptive_statistic_HRS = summary_descriptive_statistic$`1`
  
  write.csv(summary_descriptive_statistic_HRS, file = paste(path, folder, "summary_descriptive_statistic_HRS.csv", sep = "")) 
  
  
  
 
  
  #pdf(file = paste(folder, "age_compare_plot.pdf", sep = ""),   # The directory you want to save the file in
  #    width = 4, # The width of the plot in inches
  #    height = 4)
  
  pdf(file = paste(path, folder, "age_compare_plot.pdf", sep = ""),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  
  #plots for age
  age_plot = ggboxplot(data_both_countries, x = "country_cat", y = "age", 
                       color = "country_cat", palette = c("#00AFBB", "#E7B800"),
                       ylab = "age", xlab = "country_cat")
  
  
  print(age_plot)
  
  dev.off()
  
  
  
  
  #teting that US sample and UK sample have the same variance 
  res.ftest_age <- var.test(age ~ country_cat, data = data_both_countries)
  res.ftest_age
  
  ftest_names = c("F value", 
                  "95% CI ", 
                  "95% CI", 
                  "p-value")
  
  age_ftest_age = c(res.ftest_age$statistic, 
                    res.ftest_age$conf.int[1], 
                    res.ftest_age$conf.int[2], 
                    res.ftest_age$p.value) 
  
  age_ftest_age = rbind(ftest_names, age_ftest_age)
  age_ftest_age = as.data.frame(age_ftest_age)
  
  write.csv(age_ftest_age, file = paste(path, folder, "age_ftest.csv", sep = "")) 
  
  
  #t-test comparing differences in age between US sample and UK sample 
  res_age <- t.test(age ~ country_cat, data = data_both_countries, var.equal = TRUE)
  
  T_test_names = c("t value", 
                   "df",
                   "p-value", 
                   "95% CI ", 
                   "95% CI",
                   "samples 1 mean",
                   "samples 2 mean")
  


  res_age = c(res_age$statistic, 
              res_age$parameter, 
              res_age$p.value, 
              res_age$conf.int[1], 
              res_age$conf.int[2], 
              res_age$estimate)
  
  res_age = rbind(T_test_names, res_age)
  
  res_age = as.data.frame(res_age)
  
  write.csv(res_age, file = paste(path, folder, "age_compare.csv", sep = "")) 
  
  #############
  
  #############
  
  
  sex_diff = chisq.test(data_both_countries$country_cat, data_both_countries$sex)
  sex_diff_names = c("chi sq",
                     "df",
                     "p value")
  
  sex_diff = cbind(sex_diff$statistic,
                   sex_diff$parameter,
                   sex_diff$p.value) 
  
  
  sex_diff = rbind(sex_diff_names,
                   sex_diff) 
  
  table_freq = table(data_both_countries$country_cat, data_both_countries$sex)
  
  table_freq_names = c("0_0",
                       "0_1",
                       "1_0",
                       "1_1")
  
  table_freq_all = c(table_freq[1,1],
                     table_freq[1,2],
                     table_freq[2,1],
                     table_freq[2,2]) 
  
  table_frequencies = rbind(table_freq_names,
                            table_freq_all)
  
  sex_diff = cbind(sex_diff, table_frequencies)
  
  write.csv(sex_diff, file = paste(path, folder, "sex_diff.csv", sep = "")) 
  
  #plots for wealth
  
  
  pdf(file = paste(path, folder, "wealth_compare_plot.pdf", sep = ""),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  
  wealth_plot =  ggboxplot(data_both_countries, x = "country_cat", y = "wealth", 
                           color = "country_cat", palette = c("#00AFBB", "#E7B800"),
                           ylab = "wealth", xlab = "country_cat")
  
  print(wealth_plot)
  
  dev.off()
  
  
  
  
  
  #teting that US sample and UK sample have the same variance 
  res.ftest_wealth <- var.test(wealth ~ country_cat, data = data_both_countries)
  res.ftest_wealth
  
  res.ftest_wealth = c(res.ftest_wealth$statistic, 
                       res.ftest_wealth$conf.int[1], 
                       res.ftest_wealth$conf.int[2], 
                       res.ftest_wealth$p.value) 
  
  res.ftest_wealth = rbind(ftest_names, res.ftest_wealth)
  res.ftest_wealth = as.data.frame(res.ftest_wealth)
  
  write.csv(res.ftest_wealth, file = paste(path, folder, "wealth_ftest.csv", sep = "")) 
  
  
  #t-test comparing differences in wealth between US sample and UK sample 
  res_wealth <- t.test(wealth ~ country_cat, data = data_both_countries, var.equal = TRUE)
  res_wealth
  
  res_wealth = c(res_wealth$statistic, 
                 res_wealth$parameter, 
                 res_wealth$p.value, 
                 res_wealth$conf.int[1], 
                 res_wealth$conf.int[2], 
                 res_wealth$estimate)
  
  res_wealth = rbind(T_test_names, res_wealth)
  
  
  res_wealth = as.data.frame(res_wealth)
  
  write.csv(res_wealth, file = paste(path, folder, "wealth_compare.csv",  sep = "")) 
  
  
  #plots for BMI
  
  
  pdf(file = paste(path, folder, "BMI_compare_plot.pdf", sep = ""),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4)
  
  
  
  BMI_plot =  ggboxplot(data_both_countries, x = "country_cat", y = "BMI", 
                        color = "country_cat", palette = c("#00AFBB", "#E7B800"),
                        ylab = "BMI", xlab = "country_cat")
  
  print(BMI_plot)
  
  dev.off()
  
  
  
  #teting that US sample and UK sample have the same variance 
  res.ftest_BMI <- var.test(BMI ~ country_cat, data = data_both_countries)
  res.ftest_BMI
  
  res.ftest_BMI = c(res.ftest_BMI$statistic, 
                    res.ftest_BMI$conf.int[1], 
                    res.ftest_BMI$conf.int[2], 
                    res.ftest_BMI$p.value) 
  
  res.ftest_BMI = rbind(ftest_names, res.ftest_BMI)
  res.ftest_BMI = as.data.frame(res.ftest_BMI)
  
  write.csv(res.ftest_BMI, file = paste(path, folder, "BMI_ftest.csv", sep = "")) 
  
  
  #t-test comparing differences in BMI between US sample and UK sample 
  res_BMI <- t.test(BMI ~ country_cat, data = data_both_countries, var.equal = TRUE)
  res_BMI
  
  
  res_BMI = c(res_BMI$statistic, 
              res_BMI$parameter, 
              res_BMI$p.value, 
              res_BMI$conf.int[1], 
              res_BMI$conf.int[2], 
              res_BMI$estimate)
  
  res_BMI = rbind(T_test_names, res_BMI)
  
  
  res_BMI = as.data.frame(res_BMI)
  
  write.csv(res_BMI, file = paste(path, folder, "BMI_compare.csv", sep = "")) 
  

  
  
 
}