
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)

ELSAdiscrimination_data_wave5_before_restricting = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv")
HRS2010_discrimination_dataset_before_restricting = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset.csv")


ELSAdiscrimination_data_WAVE6_before_subsetting = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/DATA_ELSA/ELSA_data_all/wave_6_elsa_data_v2.csv")
ELSAdiscrimination_data_wave5_before_subsetting = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv")
HRS2010_discrimination_datase_before_subsetting = read.csv("/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset.csv")


ELSAdiscrimination_data_WAVE6_before_subsetting$LGB = case_when(ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 1 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==1 ~ 0,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 5 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==2 ~ 0,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 5 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==1 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 1 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==2 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 2 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==1 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 3 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==1 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 4 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==1 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 2 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==2 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 3 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==2 ~ 1,
                                                                ELSAdiscrimination_data_WAVE6_before_subsetting$q34m == 4 & ELSAdiscrimination_data_WAVE6_before_subsetting$DhSex ==2 ~ 1)

ELSAdiscrimination_data_WAVE6 = subset(ELSAdiscrimination_data_WAVE6_before_subsetting, ELSAdiscrimination_data_WAVE6_before_subsetting$LGB ==1)


ELSAdiscrimination_data_wave5 = list(ELSAdiscrimination_data_WAVE6,
                                     ELSAdiscrimination_data_wave5_before_subsetting) %>%
  reduce(inner_join, by = "idauniq")

nrow(ELSAdiscrimination_data_wave5)

#subset to LGB in ELSA w5

#subset to LGB in wave 2010, however this question was only asked in 2016 and 2018, 
###so 2010 data for those who reported LGB in 2016 or 2018, was restricted to...
supset_participant_1 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 16566010)


supset_participant_2 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 16566020)


supset_participant_3 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 31670010)



supset_participant_4 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 35019010)



supset_participant_5 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 54185020)


supset_participant_6 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 139278010)


supset_participant_7 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 186112020)



supset_participant_8 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 502156010)



supset_participant_9 = subset(HRS2010_discrimination_dataset_before_restricting,
                              HRS2010_discrimination_dataset_before_restricting$HHIDPN == 903912010)


supset_participant_10 = subset(HRS2010_discrimination_dataset_before_restricting,
                               HRS2010_discrimination_dataset_before_restricting$HHIDPN == 914449010)


HRS2010_discrimination_dataset = rbind(supset_participant_1, 
                                       supset_participant_2, 
                                       supset_participant_3, 
                                       supset_participant_4, 
                                       supset_participant_5, 
                                       supset_participant_6, 
                                       supset_participant_7,
                                       supset_participant_8, 
                                       supset_participant_9, 
                                       supset_participant_10)

HRS2010_discrimination_dataset = as.data.frame(HRS2010_discrimination_dataset)
######***********•••••••••••••• SEXUALITY ######***********••••••••••••••
discrim_sexuality_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_sexuality 
ELSAdiscrimination_data_wave5$w5discrim_sexuality2 

### ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived sexuality discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived discrimination is a binary variable 

#continious_age
contengency_table_sexuality_ANDage  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$continious_age)
print(contengency_table_sexuality_ANDage)
test_sexuality_ANDage = chisq.test(contengency_table_sexuality_ANDage)
test_sexuality_ANDage


contengency_table_sexuality_ANDage  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$age_groups)
print(contengency_table_sexuality_ANDage)
test_sexuality_ANDage = chisq.test(contengency_table_sexuality_ANDage)
test_sexuality_ANDage 

# there are ... and perceived sexuality is a binary variable 
contengency_table_sexuality_ANDsex  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$sex_1_2)
print(contengency_table_sexuality_ANDsex)
test_table_sexuality_ANDsex = chisq.test(contengency_table_sexuality_ANDsex)
test_table_sexuality_ANDsex 

# there are .... and perceived sexuality is a binary variable 
contengency_table_sexuality_AND_income  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$annual_income_self_employment)
print(contengency_table_sexuality_AND_income)
test_sexuality_AND_income = chisq.test(contengency_table_sexuality_AND_income)
test_sexuality_AND_income

# there are ....and perceived sexuality is a binary variable 
contengency_table_sexuality_ANDeducation_level  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$education_levels)
print(contengency_table_sexuality_ANDeducation_level)
test_sexuality_ANDeducation_level = chisq.test(contengency_table_sexuality_ANDeducation_level)
test_sexuality_ANDeducation_level

# there are .... and perceived sexuality is a binary variable 
contengency_table_sexuality_ANDemployment  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$employment_allCategories)
print(contengency_table_sexuality_ANDemployment)
test_sexuality_ANDemployment = chisq.test(contengency_table_sexuality_ANDemployment)
test_sexuality_ANDemployment

# there are .... and perceived sexuality is a binary variable 
contengency_table_sexuality_ANDmarital_status  = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$marital_status)
print(contengency_table_sexuality_ANDmarital_status)
test_sexuality_ANDmarital_status = chisq.test(contengency_table_sexuality_ANDmarital_status)
test_sexuality_ANDmarital_status

#wealth and sex discrimination correlation in HRS 
contengency_table_sexuality_ANDwealth = table(discrim_sexuality_HRS, HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)
print(contengency_table_sexuality_ANDwealth)
test_sexuality_ANDwealth = chisq.test(contengency_table_sexuality_ANDwealth)
test_sexuality_ANDwealth

#wealth quantile and sex discrimination !!!!!!!!!!!!!!
wealth_noPension_HRS2010 = HRS2010_discrimination_dataset$H10ATOTW
quantile(wealth_noPension_HRS2010, probs =c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1))
Percentile_wealth_HRS2010 = recode(wealth_noPension_HRS2010, 
                                   "-2760000:5000='20%';
                                   5001:69000='40%';
                                   69001:180000='60%';
                                   180000:425400='80%';
                                   425401:27662000='100%'")

HRS2010_discrimination_dataset$Percentile_wealth_HRS2010 = Percentile_wealth_HRS2010
contengency_table_sexuality_ANDwealthPercentile = table(discrim_sexuality_HRS, Percentile_wealth_HRS2010)
print(contengency_table_sexuality_ANDwealthPercentile)
test_sexuality_ANDwealth_percentile = chisq.test(contengency_table_sexuality_ANDwealthPercentile)
test_sexuality_ANDwealth_percentile 


########### correlates for ELSA study 
contengency_table_sexuality_ANDage_ELSA =table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
                                          ELSAdiscrimination_data_wave5$w5age)
test_contengency_table_sexuality_ANDage_ELSA = chisq.test(contengency_table_sexuality_ANDage_ELSA)
test_contengency_table_sexuality_ANDage_ELSA 

contengency_table_sexuality_ANDsex_ELSA = table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
                                           ELSAdiscrimination_data_wave5$w5sex) 
test_contengency_table_sexuality_ANDsex_ELSA = chisq.test(contengency_table_sexuality_ANDsex_ELSA)
test_contengency_table_sexuality_ANDsex_ELSA #0.053...

contengency_table_sexuality_ANDemployment_ELSA =table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
                                                 ELSAdiscrimination_data_wave5$employment)
test_contengency_table_sexuality_ANDemployment_ELSA = chisq.test(contengency_table_sexuality_ANDemployment_ELSA)
test_contengency_table_sexuality_ANDemployment_ELSA

#contengency_table_sexuality_AND_educaiton_ELSA = table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
#                                                  ELSAdiscrimination_data_wave5$ELSA_Education)
#test_contengency_table_sexuality_ANDeducation_ELSA = chisq.test(contengency_table_sexuality_AND_educaiton_ELSA)
#test_contengency_table_sexuality_ANDeducation_ELSA
# too few cases to run nine categories of education vs sexuality 

contengency_table_sexuality_AND_wealth_ELSA = table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
                                               ELSAdiscrimination_data_wave5$w5wealth)
test_contengency_table_sexuality_ANDwealth_ELSA = chisq.test(contengency_table_sexuality_AND_wealth_ELSA)
test_contengency_table_sexuality_ANDwealth_ELSA

ELSAdiscrimination_data_wave5$w5wealthq
contengency_table_sexuality_AND_wealthq_ELSA = table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
                                                ELSAdiscrimination_data_wave5$w5wealthq)
test_contengency_table_sexuality_ANDwealthq_ELSA = chisq.test(contengency_table_sexuality_AND_wealthq_ELSA)
test_contengency_table_sexuality_ANDwealthq_ELSA

contengency_table_sexuality_ANDmarital_status_ELSA=table(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2,
                                                    ELSAdiscrimination_data_wave5$w5married)

test_contengency_table_sexuality_ANDmarital_status_ELSA= chisq.test(contengency_table_sexuality_ANDmarital_status_ELSA)
test_contengency_table_sexuality_ANDmarital_status_ELSA

######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: sex, education level and employment 
####################################################### sexuality and
################################################################# 1) sex, 
################################################################# 2) education level, 
################################################################# 3) employment,
########## sexuality covariate model ##############


sexuality_covariates_model <- glm(HRS2010_reason_discrim1_reason_sexuality ~ sex_1_2, HRS2010_discrimination_dataset, family = binomial)
sexuality_covariates_model
summary(sexuality_covariates_model)


ELSA_log_reg = glm(w5sexualitydiscrimination2 ~ w5sex, data =ELSAdiscrimination_data_wave5)
summary(ELSA_log_reg)
######################### OR and 95% CI

#ELSA
ELSA_OR = exp(cbind(OR = coef(ELSA_log_reg), confint(ELSA_log_reg)))
ELSA_OR_value = ELSA_OR[1]
ELSA_CI1 = ELSA_OR[3]
ELSA_CI2 = ELSA_OR[5]

#HRS 
HRS_OR = exp(cbind(OR = coef(sexuality_covariates_model), confint(sexuality_covariates_model)))
HRS_OR_value = HRS_OR[1]
HRS_CI1 = HRS_OR[3]
HRS_CI2 = HRS_OR[5]

############# plot covarialte (e., g, employment category) and discrimination sexuality  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

nrow(ELSAdiscrimination_data_wave5)
nrow(HRS2010_discrimination_dataset)

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#dummy variable: 
ELSAdiscrimination_data_wave5$country = rep(1, times = 139)
HRS2010_discrimination_dataset$country = rep(2, times = 10)



#binary variable names below:
unique(ELSAdiscrimination_data_wave5$w5agediscrimination2)
unique(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2)

#predictor dummy varibale: country (UK vs USA)
country_cat = c(ELSAdiscrimination_data_wave5$country, 
                HRS2010_discrimination_dataset$country)

#outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)


ELSAdiscrimination_data_wave5$discrim_sexuality_bothCountries = ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2
HRS2010_discrimination_dataset$discrim_sexuality_bothCountries = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_sexuality

discrimination_sexuality = c(ELSAdiscrimination_data_wave5$discrim_sexuality_bothCountries, 
                        HRS2010_discrimination_dataset$discrim_sexuality_bothCountries)

#covariates pooled from ELSA and HRS  (make sure the order as above)
#DONE: in data merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
unique(ELSAdiscrimination_data_wave5$w5sex)
unique(HRS2010_discrimination_dataset$sex_1_2)
print("check in code books that 1 and 0 in sexuality categories mean the same")

age = c(ELSAdiscrimination_data_wave5$w5age,
        HRS2010_discrimination_dataset$continious_age)


sex = c(ELSAdiscrimination_data_wave5$w5sex,
        HRS2010_discrimination_dataset$sex_1_2)


education = c(ELSAdiscrimination_data_wave5$ELSA_Education, 
              HRS2010_discrimination_dataset$education_levels)


employment = c(ELSAdiscrimination_data_wave5$employment,
               HRS2010_discrimination_dataset$employment_allCategories)


wealth = c(ELSAdiscrimination_data_wave5$w5wealth,
           HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)


wealth_quantiles = c(ELSAdiscrimination_data_wave5$w5wealthq,
                     HRS2010_discrimination_dataset$Percentile_wealth_noIRA_HRS2010) 

married = c(ELSAdiscrimination_data_wave5$w5married,
            HRS2010_discrimination_dataset$married_bin)

data_both_countries_sexuality_discrim <- data.frame(country_cat, 
                                               discrimination_sexuality, 
                                               age,
                                               sex,
                                               education,
                                               employment,
                                               wealth,
                                               wealth_quantiles,
                                               married)


################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
################ ####like in Rippon:  in  order  to  determine  any  cross-national  differences  in  sexuality  discrimination. 

fm1_sexuality <- glm(discrimination_sexuality ~ sex, 
                data = data_both_countries_sexuality_discrim)

fm2_sexuality <- glm(discrimination_sexuality ~  country_cat + sex, 
                data = data_both_countries_sexuality_discrim)


## various equivalent specifications of the LR test
sexuality_cross_national_diff = lrtest(fm1_sexuality, fm2_sexuality)

sexuality_chi_value_cross_national = sexuality_cross_national_diff$stats[1]

sexuality_pvalue_cross_national = sexuality_cross_national_diff$stats[3]

cross_sexuality_findings = cbind(sexuality_chi_value_cross_national,
                            sexuality_pvalue_cross_national, 
                            ELSA_OR_value, 
                            ELSA_CI1, 
                            ELSA_CI2, 
                            HRS_OR_value, 
                            HRS_CI1, 
                            HRS_CI2)


sexuality_results = rbind(cross_sexuality_findings)

write.csv(sexuality_results, file = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/Restricted_sig_cov_model_or/sexuality_results_restricted_sig_cov_model.csv")




