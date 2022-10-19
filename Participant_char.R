#source("/Users/aliya/my_docs/proj/Cumulative_effects_HRS/Version_2_analysis/participant_char_function.R") 



# Participant Characteristics table 

#I think this paper would benefit from a Sample Characteristics table where age, sex, different aspects of ‘wealth’ and other factors relating to subgroups (i.e. make up of ethnic minority groups) are described. 
#### rename some vars for the participant characteristics table 

#Disability (physical limitation), Financial status, Race (ethnic minority), Sex (female), Sexual orientation, Weight (BMI>30)



Unadjusted_results = data.frame()

library("stats")

Participant_char =  function (data){
  
  data = data 
  
  ## All sex
  
  data$sex_1_2
  data_female = subset(data, data$sex_1_2 == 2)
  n_female_all = nrow(data_female)
  percent_female = nrow(data_female)/nrow(data)*100
  
  ## ALL wealth 
  #create wealth quantile variable 
  
  data$wealth
  
  bound_1 = quantile(data$wealth,
                     probs = seq(0, 0.25), na.rm = TRUE)
  
  
  bound_2 = quantile(data$wealth,
                     probs = seq(0.25, 0.5), na.rm = TRUE)
  
  
  bound_3 = quantile(data$wealth,
                     probs = seq(0.50, 0.75), na.rm = TRUE)
  
  
  bound_4 = quantile(data$wealth,
                     probs = seq(0.75, 1), na.rm = TRUE)
  
  
  
  
  q1_wealth_baseline_all = subset(data, data$wealth > bound_1 & data$wealth <= bound_2)
  n_q1 = nrow(q1_wealth_baseline_all)
  percent_q1 = n_q1/nrow(data)*100
  
  
  
  q2_wealth_baseline_all = subset(data, data$wealth > bound_2 & data$wealth <= bound_3)
  n_q2 = nrow(q2_wealth_baseline_all)
  percent_q2 = n_q2/nrow(data)*100
  
  
  q3_wealth_baseline_all = subset(data, data$wealth > bound_3 & data$wealth <= bound_4)
  n_q3 = nrow(q3_wealth_baseline_all)
  percent_q3 = n_q3/nrow(data)*100
  
  q4_wealth_baseline_all = subset(data, data$wealth > bound_4)
  n_q4 = nrow(q4_wealth_baseline_all)
  percent_q4 = n_q4/nrow(data)*100
  
  ######## wealth quantiles 
  data$wealth = as.numeric(data$wealth)
  median_wealth = median(data$wealth, na.rm	 = TRUE)
  
  
  data$ses = case_when(data$wealth <=median(data$wealth, na.rm = TRUE) ~ 1,
                       data$wealth > median(data$wealth, na.rm = TRUE) ~ 2)
  
  low_ses = subset(data, data$ses == 1)
  low_ses_n = nrow(low_ses)
  low_ses_percent = low_ses_n/nrow(data) *100 
  
  
  high_ses = subset(data, data$ses == 2)
  high_ses_n = nrow(high_ses)
  high_ses_percent = high_ses_n/nrow(data) *100 
  
  summary(data$wealth)
  
  data_n = nrow(data)
  # 
  # subset_1 = subset(data, data$wealth<summary_stat[2])
  # subset_1_n = nrow(subset_1)
  # subset_1_percent = subset_1_n/data_n *100 
  # subset_1_mean_wealth = mean(subset_1$wealth, na.rm	 = TRUE)
  # subset_1_SD_wealth = sd(subset_1$wealth, na.rm	 = TRUE)
  # 
  # 
  # 
  # subset_2 = subset(data, data$wealth>summary_stat[2] & data$wealth<summary_stat[3])
  # subset_2_n =nrow(subset_2)
  # subset_2_percent = subset_2_n/data_n *100 
  # 
  # subset_2_mean_wealth = mean(subset_2$wealth, na.rm	 = TRUE)
  # subset_2_SD_wealth = sd(subset_2$wealth, na.rm	 = TRUE)
  # 
  # 
  # subset_3 = subset(data, data$wealth>summary_stat[3] & data$wealth<summary_stat[5])
  # subset_3_n =nrow(subset_3)
  # subset_3_percent = subset_3_n/data_n *100 
  # 
  # 
  # subset_3_mean_wealth = mean(subset_3$wealth, na.rm	 = TRUE)
  # subset_3_SD_wealth = sd(subset_3$wealth, na.rm	 = TRUE)
  # 
  # 
  # subset_4 = subset(data, data$wealth>summary_stat[5])
  # subset_4_n = nrow(subset_4)
  # subset_4_percent = subset_4_n/data_n *100 
  # 
  # subset_4_mean_wealth = mean(subset_4$wealth, na.rm	 = TRUE)
  # subset_4_SD_wealth = sd(subset_4$wealth, na.rm	 = TRUE)
  
  #########
  ## All age 
  data$age = as.numeric(data$age)
  data = tibble(data)
  data = data %>% drop_na(age)
  unique(data$age)
  
  mean_age_all = mean(data$age) 
  sd_age_all = sd(data$age)
  range_age = range(data$age)
  
  
  ## All CVD
  
  # #CVD_new
  # #CVD_ever
  # data$CVD = factor(data$CVD)
  # 
  # data_CVD = subset(data, data$CVD == 1) 
  # nrow_CVD = nrow(data_CVD)
  # percent_CVD = ((nrow_CVD)/nrow(data))*100
  
  #unique(data$CVD_new)
  
  #data_CVD_new = subset(data, data$CVD_new == 1)
  #n_data_CVD_new = nrow(data_CVD_new)
  
  #data_CVD_ever = subset(data, data$CVD_ever == 1)
  #n_data_CVD_ever = nrow(data_CVD_ever)
  
  # unique(data$CVD_new)
  
  
  
  #n_CVD_baseline =  n_data_CVD_ever + data_CVD_new
  
  
  ## All healh behaviours
  
  # mean_days_alchohol_baseline_all = mean(data$alcohol_days_week, na.rm = TRUE)
  # sd_days_alchohol_baseline_all = sd(data$alcohol_days_week, na.rm = TRUE)
  # range_alchohol_baseline_all = range(data$alcohol_days_week, na.rm = TRUE) 
  # 
  # 
  # data$smokes_now_bin
  # data_smokes_now_bin_all = subset(data, data$smokes_now_bin == 1)
  # n_baseline_smokes_now_bin_all = nrow(data_smokes_now_bin_all)
  # 
  # 
  # median_PA_baseline_all = median(data$vigarious_physical_activity, na.rm = TRUE)
  # q2_PA_baseline_all = quantile(data$vigarious_physical_activity,
  #                               probs = seq(0.25, 0.5), na.rm = TRUE)
  # 
  # q3_PA_baseline_all = quantile(data$vigarious_physical_activity,
  #                               probs = seq(0.5, 0.75), na.rm = TRUE)
  # 
  # range_PA_baseline_all = range(data$vigarious_physical_activity, na.rm = TRUE)
  # 
  
  
  # ## All CVD risks
  # 
  # data$hypertension_new_bin
  # data_hypertension_bin_all = subset(data, data$hypertension_new_bin == 1)
  # n_baseline_hypertension_bin_all = nrow(data_hypertension_bin_all)
  # percent_hypertension = n_baseline_hypertension_bin_all/nrow(data)*100
  # 
  # 
  
  ## All BMI 
  data$assessed_BMI = as.numeric(data$assessed_BMI)
  
  mean_BMI_all = mean(data$assessed_BMI, na.rm = TRUE)
  sd_BMI_all =  sd(data$assessed_BMI, na.rm = TRUE)
  range_BMI_all = range(data$assessed_BMI, na.rm = TRUE) 
  
  
  ############ frequencies for different bounds of BMI 
  
  
  
  
  
  
  
  
  #####
  #data = na.omit(data)
  
  #data$assessed_BMI<-data$data[!is.na(data$assessed_BMI)]
  
  data$assessed_BMI = as.integer(data$assessed_BMI)
  data = tibble(data)
  data = data %>% drop_na(assessed_BMI)
  unique(data$assessed_BMI)
  
  data_obese = subset(data, data$assessed_BMI >30) 
  data_overweight = subset(data, data$assessed_BMI >=25 & data$assessed_BMI <=30) 
  data_healthy_weight = subset(data, data$assessed_BMI <25 & data$assessed_BMI >18) 
  data_under_weight = subset(data, data$assessed_BMI <18) 
  
  
  nrow_obese = nrow(data_obese)
  nrow_overweight = nrow(data_overweight)
  nrow_healthy_weight = nrow(data_healthy_weight)
  nrow_under_weight = nrow(data_under_weight)
  
  
  
  
  percent_all_obese = (nrow(data_obese)/nrow(data))*100
  percent_all_overweight = (nrow(data_overweight)/nrow(data))*100
  percent_all_healthy_weight = (nrow(data_healthy_weight)/nrow(data))*100
  percent_all_under_weight = (nrow(data_under_weight)/nrow(data))*100
  
  # 
  # ## All depression 
  # data$checklist_depression_bin
  # data_depression_bin_all = subset(data, data$checklist_depression_bin == 1)
  # n_baseline_depression_bin_all = nrow(data_depression_bin_all)
  # percent_depression = n_baseline_depression_bin_all/nrow(data)*100
  # 
  
  result_participant_char = rbind(mean_age_all, 
                                  sd_age_all, 
                                  range_age, 
                                  
                                  n_female_all, 
                                  percent_female,
                                  
                                  
                                  n_q1, 
                                  percent_q1,
                                  
                                  n_q1, 
                                  percent_q1,
                                  
                                  n_q2, 
                                  percent_q2,
                                  
                                  n_q3, 
                                  percent_q3, 
                                  
                                  n_q4, 
                                  percent_q4, 
                                  
                                  # subset_1_n, 
                                  # subset_1_percent, 
                                  # subset_1_mean_wealth, 
                                  # subset_1_SD_wealth, 
                                  # 
                                  # subset_2_n, 
                                  # subset_2_percent, 
                                  # subset_2_mean_wealth, 
                                  # subset_2_SD_wealth,
                                  # 
                                  # subset_3_n,
                                  # subset_3_percent, 
                                  # 
                                  # subset_3_mean_wealth,
                                  # subset_3_SD_wealth,
                                  # 
                                  # subset_4_n,
                                  # subset_4_percent,
                                  # 
                                  # subset_4_mean_wealth,
                                  # subset_4_SD_wealth,
                                  # 
                                  # low_ses_n,
                                  # low_ses_percent,
                                  # high_ses_n,
                                  # high_ses_percent, 
                                  
                                  mean_BMI_all, 
                                  sd_BMI_all, 
                                  
                                  
                                  nrow_obese,
                                  percent_all_obese, 
                                  
                                  nrow_overweight,
                                  percent_all_overweight,
                                  
                                  nrow_healthy_weight,
                                  percent_all_healthy_weight,
                                  
                                  nrow_under_weight,
                                  percent_all_under_weight)
                                  
                                  
                                  # nrow_CVD, 
                                  # percent_CVD, 
                                  # n_baseline_hypertension_bin_all, 
                                  # percent_hypertension, 
                                  # 
                                  # n_baseline_depression_bin_all,
                                  # percent_depression, 
                                  # 
                                  # 
                                  # mean_days_alchohol_baseline_all, 
                                  # sd_days_alchohol_baseline_all, 
                                  # range_alchohol_baseline_all, 
                                  # 
                                  # n_baseline_smokes_now_bin_all,
                                  # 
                                  # median_PA_baseline_all, 
                                  # q2_PA_baseline_all, 
                                  # q3_PA_baseline_all, 
                                  # range_PA_baseline_all)
  
  return(result_participant_char)
  
  
}
