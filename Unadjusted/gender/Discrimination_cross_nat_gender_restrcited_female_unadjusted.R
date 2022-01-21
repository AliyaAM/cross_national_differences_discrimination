
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
gender_restrcited_female_unadjusted = function(data_ELSA,
                                               data_HRS){
  
ELSAdiscrimination_data_wave5 = subset(data_ELSA, data_ELSA$w5sex_1_0 == 0)
HRS2010_discrimination_dataset = subset(data_HRS, data_HRS$sex_1_0 == 0)

N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
N_HRS_subset = nrow(HRS2010_discrimination_dataset)

######***********•••••••••••••• GENDER ######***********••••••••••••••
HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_gender
discrim_gender_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_gender
unique(discrim_gender_HRS)

######## ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived GENDER discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived discrimination is a binary variable 



#continious_age
contengency_table_gender_ANDage  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$continious_age)
print(contengency_table_gender_ANDage)
test_gender_ANDage = chisq.test(contengency_table_gender_ANDage)
test_gender_ANDage #sig


contengency_table_gender_ANDage  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$age_groups)
print(contengency_table_gender_ANDage)
test_gender_ANDage = chisq.test(contengency_table_gender_ANDage)
test_gender_ANDage #sig

# there are ... and perceived gender is a binary variable 
contengency_table_gender_ANDsex  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$sex_1_0)
print(contengency_table_gender_ANDsex)
test_table_gender_ANDsex = chisq.test(contengency_table_gender_ANDsex)
test_table_gender_ANDsex #sig

# there are .... and perceived gender is a binary variable 
contengency_table_gender_AND_income  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$annual_income_self_employment)
print(contengency_table_gender_AND_income)
test_gender_AND_income = chisq.test(contengency_table_gender_AND_income)
test_gender_AND_income

# there are ....and perceived gender is a binary variable 
contengency_table_gender_ANDeducation_level  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$education_levels)
print(contengency_table_gender_ANDeducation_level)
test_gender_ANDeducation_level = chisq.test(contengency_table_gender_ANDeducation_level)
test_gender_ANDeducation_level

# there are .... and perceived gender is a binary variable 
contengency_table_gender_ANDemployment  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$employment_allCategories)
print(contengency_table_gender_ANDemployment)
test_gender_ANDemployment = chisq.test(contengency_table_gender_ANDemployment)
test_gender_ANDemployment

# there are .... and perceived gender is a binary variable 
contengency_table_gender_ANDmarital_status  = table(discrim_gender_HRS, HRS2010_discrimination_dataset$marital_status)
print(contengency_table_gender_ANDmarital_status)
test_gender_ANDmarital_status = chisq.test(contengency_table_gender_ANDmarital_status)
test_gender_ANDmarital_status

#wealth and sex discrimination correlation in HRS 
contengency_table_gender_ANDwealth = table(discrim_gender_HRS, HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)
print(contengency_table_gender_ANDwealth)
test_gender_ANDwealth = chisq.test(contengency_table_gender_ANDwealth)
test_gender_ANDwealth




########### correlates for ELSA study 
contengency_table_gender_ANDage_ELSA =table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                            ELSAdiscrimination_data_wave5$w5age)
test_contengency_table_gender_ANDage_ELSA = chisq.test(contengency_table_gender_ANDage_ELSA)
test_contengency_table_gender_ANDage_ELSA #sig


contengency_table_gender_ANDsex_ELSA = table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                             ELSAdiscrimination_data_wave5$w5sex_1_0) 
test_contengency_table_gender_ANDsex_ELSA = chisq.test(contengency_table_gender_ANDsex_ELSA)
test_contengency_table_gender_ANDsex_ELSA #sig

contengency_table_gender_ANDemployment_ELSA =table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                                   ELSAdiscrimination_data_wave5$employment)
test_contengency_table_gender_ANDemployment_ELSA = chisq.test(contengency_table_gender_ANDemployment_ELSA)
test_contengency_table_gender_ANDemployment_ELSA

contengency_table_gender_AND_educaiton_ELSA = table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                                    ELSAdiscrimination_data_wave5$ELSA_Education)
test_contengency_table_gender_ANDeducation_ELSA = chisq.test(contengency_table_gender_AND_educaiton_ELSA)
test_contengency_table_gender_ANDeducation_ELSA

contengency_table_gender_AND_wealth_ELSA = table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                                 ELSAdiscrimination_data_wave5$w5wealth)
test_contengency_table_gender_ANDwealth_ELSA = chisq.test(contengency_table_gender_AND_wealth_ELSA)
test_contengency_table_gender_ANDwealth_ELSA

ELSAdiscrimination_data_wave5$w5wealthq
contengency_table_gender_AND_wealthq_ELSA = table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                                  ELSAdiscrimination_data_wave5$w5wealthq)
test_contengency_table_gender_ANDwealthq_ELSA = chisq.test(contengency_table_gender_AND_wealthq_ELSA)
test_contengency_table_gender_ANDwealthq_ELSA

contengency_table_gender_ANDmarital_status_ELSA=table(ELSAdiscrimination_data_wave5$w5sexdiscrimination2,
                                                      ELSAdiscrimination_data_wave5$w5married)

test_contengency_table_gender_ANDmarital_status_ELSA= chisq.test(contengency_table_gender_ANDmarital_status_ELSA)
test_contengency_table_gender_ANDmarital_status_ELSA

######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: sex, education level and employment 
####################################################### GENDER and
################################################################# 1) sex, 
################################################################# 2) education level, 
################################################################# 3) employment,
########## gender covariate model ##############


gender_covariates_model <- glm(HRS2010_reason_discrim1_reason_gender ~ sex_1_0 + continious_age  + wealth_noIRA_HRS2010, HRS2010_discrimination_dataset, family = binomial)
gender_covariates_model
summary(gender_covariates_model)


ELSA_log_reg = glm(w5sexdiscrimination2 ~  w5age + w5sex_1_0, data =ELSAdiscrimination_data_wave5)
summary(ELSA_log_reg)
######################### OR and 95% CI

#ELSA
ELSA_OR = exp(cbind(OR = coef(ELSA_log_reg), confint(ELSA_log_reg)))
ELSA_OR_value = ELSA_OR[1]
ELSA_CI1 = ELSA_OR[4]
ELSA_CI2 = ELSA_OR[7]

#HRS 
HRS_OR = exp(cbind(OR = coef(gender_covariates_model), confint(gender_covariates_model)))
HRS_OR_value = HRS_OR[1]
HRS_CI1 = HRS_OR[5]
HRS_CI2 = HRS_OR[9]

############# plot covarialte (e., g, employment category) and discrimination GENDER  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#dummy variable: 
nrow(ELSAdiscrimination_data_wave5)
nrow(HRS2010_discrimination_dataset)

ELSAdiscrimination_data_wave5$country = rep(1, times = 5705)
HRS2010_discrimination_dataset$country = rep(0, times = 12803)



#binary variable names below:
unique(ELSAdiscrimination_data_wave5$w5agediscrimination2)
unique(ELSAdiscrimination_data_wave5$w5sexdiscrimination2)
unique(ELSAdiscrimination_data_wave5$w5racediscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2)

#predictor dummy varibale: country (UK vs USA)
country_cat = c(ELSAdiscrimination_data_wave5$country, 
                HRS2010_discrimination_dataset$country)

#outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)


ELSAdiscrimination_data_wave5$discrim_gender_bothCountries = ELSAdiscrimination_data_wave5$w5sexdiscrimination2
HRS2010_discrimination_dataset$discrim_gender_bothCountries = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_gender

discrimination_gender = c(ELSAdiscrimination_data_wave5$discrim_gender_bothCountries, 
                          HRS2010_discrimination_dataset$discrim_gender_bothCountries)

#covariates pooled from ELSA and HRS  (make sure the order as above)
#DONE: in data merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
unique(ELSAdiscrimination_data_wave5$w5sex_1_0)
unique(HRS2010_discrimination_dataset$sex_1_0)

age = c(ELSAdiscrimination_data_wave5$w5age,
        HRS2010_discrimination_dataset$continious_age)


sex = c(ELSAdiscrimination_data_wave5$w5sex_1_0,
        HRS2010_discrimination_dataset$sex_1_0)


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

data_both_countries_gender_discrim <- data.frame(country_cat, 
                                                 discrimination_gender, 
                                                 age,
                                                 sex,
                                                 education,
                                                 employment,
                                                 wealth,
                                                 wealth_quantiles,
                                                 married)


################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
################ ####like in Rippon:  in  order  to  determine  any  cross-national  differences  in  gender  discrimination. 



contengency_table_gender_AND_country = table(data_both_countries_gender_discrim$discrimination_gender,
                                                data_both_countries_gender_discrim$country_cat)
test_contengency_table_gender_AND_country = chisq.test(contengency_table_gender_AND_country)
test_contengency_table_gender_AND_country



## various equivalent specifications of the LR test
#gender_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)

summary(test_contengency_table_gender_AND_country)

OR_gender_cross_national = oddsratio.wald(contengency_table_gender_AND_country)
OR_gender_cross_national_values = OR_gender_cross_national$measure

###############################################

#result table: 
gender_chi_value_cross_national = test_contengency_table_gender_AND_country$statistic

gender_pvalue_cross_national = test_contengency_table_gender_AND_country$p.value

cross_gender_findings = cbind(N_ELSA_subset, 
                              N_HRS_subset, 
                              gender_chi_value_cross_national,
                              gender_pvalue_cross_national,
                              OR_gender_cross_national_values,
                              ELSA_OR_value, 
                              ELSA_CI1, 
                              ELSA_CI2, 
                              HRS_OR_value, 
                              HRS_CI1, 
                              HRS_CI2)


gender_results = rbind(cross_gender_findings)

return(gender_results)
}


