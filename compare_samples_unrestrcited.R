
#set the source and output so you can read the files from any computer
library(dplyr)
library(car)
library(stats)
library(ggplot2)
library(scales)
library(arm)
library(stats)

library("ggpubr")



library(tidyverse)



###### Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
######  Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"


###### Set the source location on the user's local machine  for sourcing functions 
#SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"



###### sourcing code for the unadjusted analysis 
#source(paste(SOURCE_ROOT, "subsetting_sample_char.R", sep=""))



###### read data files for ELSA and HRS
ELSAdiscrimination_data_wave5_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep=""))
HRS2010_discrimination_dataset_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv", sep=""))


###### subset HRS and ELSA dataset to those who are 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 
HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)

###### subet to those who responded to the discrimination items: 
ELSAdiscrimination_data_wave5_before_subsetting = subset(ELSAdiscrimination_data_wave5_age50, ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 0 | ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 1)

###### drop rows where the responses across all situations are all NAs in the HRS study 
HRS2010_discrimination_dataset_before_subsetting = subset(HRS2010_discrimination_dataset_age50,HRS2010_discrimination_dataset_age50$HRS2010_discrim_lessrespect!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_harassed!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_poorerservice!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_notclever!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_medical!= "NA")
ELSAdiscrimination_data_wave5_before_subsetting$w5age = as.integer(ELSAdiscrimination_data_wave5_before_subsetting$w5age)

###### check these are correct answers in the bin var: 0 or 1
Total_N_ELSA = nrow(ELSAdiscrimination_data_wave5_before_subsetting)
mean_age_ELSA = mean(ELSAdiscrimination_data_wave5_before_subsetting$w5age)
sd_age_ELSA = sd(ELSAdiscrimination_data_wave5_before_subsetting$w5age)

N_women_ELSA = nrow(subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0 == 0))

Total_N_HRS = nrow(HRS2010_discrimination_dataset_before_subsetting)
mean_age_HRS = mean(HRS2010_discrimination_dataset_before_subsetting$continious_age)
sd_age_HRS = sd(HRS2010_discrimination_dataset_before_subsetting$continious_age)
N_women_HRS = nrow(subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$sex_1_0 == 0))

total_sample = cbind(Total_N_ELSA, mean_age_ELSA, sd_age_ELSA, N_women_ELSA,
                     Total_N_HRS, mean_age_HRS, sd_age_HRS, N_women_HRS)

total_sample



write.csv(total_sample, file = paste(OUTPUT_ROOT, "total_sample_characteristics.csv", sep=""))

######  dummy code the countries 
ELSAdiscrimination_data_wave5_before_subsetting$country = rep(1, times = nrow(ELSAdiscrimination_data_wave5_before_subsetting))
HRS2010_discrimination_dataset_before_subsetting$country = rep(0, times = nrow(HRS2010_discrimination_dataset_before_subsetting))



###### to be able to subset to low ses and high ses creaate new var: 

ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds = ELSAdiscrimination_data_wave5_before_subsetting$w5wealth
#median in ELSA: £239600  OR $324211.1 

median(ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds, na.rm = TRUE)

######### for the plots and ajusted analysis convert wealth to dollars, however when creating binary SES var it is irrelevent as it is a relative wealth within the sample that matters for that analysis 
ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$wealth_pounds * 1.353135
median(ELSAdiscrimination_data_wave5_before_subsetting$wealth, na.rm = TRUE)


HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010

########################################################

ELSAdiscrimination_data_wave5_before_subsetting$medianWealth_ELSA = median(ELSAdiscrimination_data_wave5_before_subsetting$w5wealth)
#ELSA median: 652000
#creating a new binary variable: 
ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA = case_when(ELSAdiscrimination_data_wave5_before_subsetting$w5wealth >= 652000 ~'2',
                                                                                   ELSAdiscrimination_data_wave5_before_subsetting$w5wealth < 652000 ~ '1')

#average national wealth excluding pension for age >52 for year 2021: 410100
#average national wealth for the entire nation median = 238,500

HRS2010_discrimination_dataset_before_subsetting$medianWealth_HRS = median(HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010)
#HRS median: 787500
#creating a new binary variable: 
HRS2010_discrimination_dataset_before_subsetting$median_wealth_bin_HRS = case_when(HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010 >=787500 ~ '2', 
                                                                                   HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010 < 787500 ~ '1')



#rename the covariates so the names are consistant between ELSA and HRS
# the new names are: 
#age 
#sex 
#education 
#employment 
#wealth 
#wealth_quantiles
#married 
#marital_statusELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age

ELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age
HRS2010_discrimination_dataset_before_subsetting$age  = HRS2010_discrimination_dataset_before_subsetting$continious_age


ELSAdiscrimination_data_wave5_before_subsetting$sex = ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0
HRS2010_discrimination_dataset_before_subsetting$sex = HRS2010_discrimination_dataset_before_subsetting$sex_1_0

ELSAdiscrimination_data_wave5_before_subsetting$education = ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Education
HRS2010_discrimination_dataset_before_subsetting$education = HRS2010_discrimination_dataset_before_subsetting$education_levels


ELSAdiscrimination_data_wave5_before_subsetting$employment = ELSAdiscrimination_data_wave5_before_subsetting$employment
HRS2010_discrimination_dataset_before_subsetting$employment =  HRS2010_discrimination_dataset_before_subsetting$employment_allCategories


ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$wealth
HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth

ELSA_wealth = (ELSAdiscrimination_data_wave5_before_subsetting$wealth)
sd(ELSA_wealth)

sd(HRS2010_discrimination_dataset_before_subsetting$wealth)

ELSAdiscrimination_data_wave5_before_subsetting$wealth_quantiles = ELSAdiscrimination_data_wave5_before_subsetting$w5wealthq
HRS2010_discrimination_dataset_before_subsetting$wealth_quantiles =  HRS2010_discrimination_dataset_before_subsetting$Percentile_wealth_noIRA_HRS2010

ELSAdiscrimination_data_wave5_before_subsetting$married = ELSAdiscrimination_data_wave5_before_subsetting$w5married
HRS2010_discrimination_dataset_before_subsetting$married =  HRS2010_discrimination_dataset_before_subsetting$married_bin

#covariates pooled from ELSA and HRS  (make sure the order as above)
#Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
ELSAdiscrimination_data_wave5_before_subsetting$marital_status = ELSAdiscrimination_data_wave5_before_subsetting$w5married4
HRS2010_discrimination_dataset_before_subsetting$marital_status = HRS2010_discrimination_dataset_before_subsetting$marital_status


data_ELSA_subset = ELSAdiscrimination_data_wave5_before_subsetting 
data_HRS_subset = HRS2010_discrimination_dataset_before_subsetting

version = "version_1"
subset_name = "ALL"

country_cat = c(data_ELSA_subset$country, 
                data_HRS_subset$country)

country_cat = as.factor(country_cat)

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

folder = paste(subset_name, version, "/", sep = "")

dir.create(paste(path, folder, sep = ""))

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
                "samples means")

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

sex_diff = chisq.test(data_both_countries$country_cat, data_both_countries$sex)
sex_diff_names = c("chi sq",
                       "df",
                       "p value")

sex_diff = cbind(sex_diff$statistic,
                 sex_diff$parameter,
                 sex_diff$p.value) 


sex_diff = rbind(sex_diff_names,
                 sex_diff) 

sex_diff = as.data.frame(sex_diff)

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


############


#testing assumption of normality test 
# Shapiro-Wilk normality test 
#SW_test_1age = with(data_both_countries, shapiro.test(age[country_cat == 0]))# should be >0.05
# Shapiro-Wilk normality test 
#SW_test_2age = with(data_both_countries, shapiro.test(age[country_cat == 1])) # should be >0.05

#SW_test_1age = as.data.frame(SW_test_1age)
#SW_test_2age = as.data.frame(SW_test_2age)

#write.csv(SW_test_1age, file = paste(path, folder, "age_SW_test_1.csv", sep = "")) 

#write.csv(SW_test_2age, file = paste(path, folder, "age_SW_test_2.csv", sep = "")) 

#testing assumption of normality test 
# Shapiro-Wilk normality test 
#SW_test_1wealth = with(data_both_countries, shapiro.test(wealth[country_cat == 0]))# should be >0.05
# Shapiro-Wilk normality test 
#SW_test_2wealth = with(data_both_countries, shapiro.test(wealth[country_cat == 1])) # should be >0.05

#SW_test_1wealth = as.data.frame(SW_test_1wealth)
#SW_test_2wealth = as.data.frame(SW_test_2wealth)

#write.csv(SW_test_1wealth, file = paste(path, folder, "wealth_SW_test_1.csv", sep = "")) 

#write.csv(SW_test_2wealth, file = paste(path, folder, "wealth_SW_test_2.csv", sep = "")) 

#testing assumption of normality test 
# Shapiro-Wilk normality test 
#SW_test_1BMI = with(data_both_countries, shapiro.test(BMI[country_cat == 0]))# should be >0.05
# Shapiro-Wilk normality test 
#SW_test_2BMI = with(data_both_countries, shapiro.test(BMI[country_cat == 1])) # should be >0.05

#SW_test_1BMI = as.data.frame(SW_test_1BMI)
#SW_test_2BMI = as.data.frame(SW_test_2BMI)

#write.csv(SW_test_1BMI, file = paste(path, folder, "BMI_SW_test_1.csv",  sep = "")) 

#write.csv(SW_test_2BMI, file = paste(path, folder, "BMI_SW_test_2.csv",  sep = "")) 

