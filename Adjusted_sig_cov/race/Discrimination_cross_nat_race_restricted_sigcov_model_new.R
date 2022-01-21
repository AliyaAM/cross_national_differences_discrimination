
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
Adjusted_race = function(data_ELSA, data_HRS){
  
ELSAdiscrimination_data_wave5 = subset(data_ELSA, data_ELSA$w5ethnicity==2)
HRS2010_discrimination_dataset = subset(data_HRS, data_HRS$HRS2010_race_nonwhite == 1)

N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
N_HRS_subset = nrow(HRS2010_discrimination_dataset)

######***********•••••••••••••• RACE ######***********••••••••••••••
discrim_race_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race

######## ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived race discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived discrimination is a binary variable 

#continious_age
contengency_table_race_ANDage  = table(discrim_race_HRS, HRS2010_discrimination_dataset$continious_age)
print(contengency_table_race_ANDage)
test_race_ANDage = chisq.test(contengency_table_race_ANDage)
test_race_ANDage


contengency_table_race_ANDage  = table(discrim_race_HRS, HRS2010_discrimination_dataset$age_groups)
print(contengency_table_race_ANDage)
test_race_ANDage = chisq.test(contengency_table_race_ANDage)
test_race_ANDage 

# there are ... and perceived race is a binary variable 
contengency_table_race_ANDsex  = table(discrim_race_HRS, HRS2010_discrimination_dataset$sex_1_2)
print(contengency_table_race_ANDsex)
test_table_race_ANDsex = chisq.test(contengency_table_race_ANDsex)
test_table_race_ANDsex 

# there are .... and perceived race is a binary variable 
contengency_table_race_AND_income  = table(discrim_race_HRS, HRS2010_discrimination_dataset$annual_income_self_employment)
print(contengency_table_race_AND_income)
test_race_AND_income = chisq.test(contengency_table_race_AND_income)
test_race_AND_income

# there are ....and perceived race is a binary variable 
contengency_table_race_ANDeducation_level  = table(discrim_race_HRS, HRS2010_discrimination_dataset$education_levels)
print(contengency_table_race_ANDeducation_level)
test_race_ANDeducation_level = chisq.test(contengency_table_race_ANDeducation_level)
test_race_ANDeducation_level

# there are .... and perceived race is a binary variable 
contengency_table_race_ANDemployment  = table(discrim_race_HRS, HRS2010_discrimination_dataset$employment_allCategories)
print(contengency_table_race_ANDemployment)
test_race_ANDemployment = chisq.test(contengency_table_race_ANDemployment)
test_race_ANDemployment

# there are .... and perceived race is a binary variable 
contengency_table_race_ANDmarital_status  = table(discrim_race_HRS, HRS2010_discrimination_dataset$marital_status)
print(contengency_table_race_ANDmarital_status)
test_race_ANDmarital_status = chisq.test(contengency_table_race_ANDmarital_status)
test_race_ANDmarital_status

#wealth and sex discrimination correlation in HRS 
contengency_table_race_ANDwealth = table(discrim_race_HRS, HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)
print(contengency_table_race_ANDwealth)
test_race_ANDwealth = chisq.test(contengency_table_race_ANDwealth)
test_race_ANDwealth



########### correlates for ELSA study 
contengency_table_race_ANDage_ELSA =table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                            ELSAdiscrimination_data_wave5$w5age)
test_contengency_table_race_ANDage_ELSA = chisq.test(contengency_table_race_ANDage_ELSA)
test_contengency_table_race_ANDage_ELSA 

contengency_table_race_ANDsex_ELSA = table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                             ELSAdiscrimination_data_wave5$w5sex) 
test_contengency_table_race_ANDsex_ELSA = chisq.test(contengency_table_race_ANDsex_ELSA)
test_contengency_table_race_ANDsex_ELSA #0.053...

contengency_table_race_ANDemployment_ELSA =table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                                   ELSAdiscrimination_data_wave5$employment)
test_contengency_table_race_ANDemployment_ELSA = chisq.test(contengency_table_race_ANDemployment_ELSA)
test_contengency_table_race_ANDemployment_ELSA

contengency_table_race_AND_educaiton_ELSA = table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                                    ELSAdiscrimination_data_wave5$ELSA_Education)
test_contengency_table_race_ANDeducation_ELSA = chisq.test(contengency_table_race_AND_educaiton_ELSA)
test_contengency_table_race_ANDeducation_ELSA

contengency_table_race_AND_wealth_ELSA = table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                                 ELSAdiscrimination_data_wave5$w5wealth)
test_contengency_table_race_ANDwealth_ELSA = chisq.test(contengency_table_race_AND_wealth_ELSA)
test_contengency_table_race_ANDwealth_ELSA

ELSAdiscrimination_data_wave5$w5wealthq
contengency_table_race_AND_wealthq_ELSA = table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                                  ELSAdiscrimination_data_wave5$w5wealthq)
test_contengency_table_race_ANDwealthq_ELSA = chisq.test(contengency_table_race_AND_wealthq_ELSA)
test_contengency_table_race_ANDwealthq_ELSA

contengency_table_race_ANDmarital_status_ELSA=table(ELSAdiscrimination_data_wave5$w5racediscrimination2,
                                                      ELSAdiscrimination_data_wave5$w5married)

test_contengency_table_race_ANDmarital_status_ELSA= chisq.test(contengency_table_race_ANDmarital_status_ELSA)
test_contengency_table_race_ANDmarital_status_ELSA

######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: sex, education level and employment 
####################################################### race and
################################################################# 1) sex, 
################################################################# 2) education level, 
################################################################# 3) employment,
########## race covariate model ##############


race_covariates_model <- glm(HRS2010_reason_discrim1_reason_race ~ sex_1_2, HRS2010_discrimination_dataset, family = binomial)
race_covariates_model
summary(race_covariates_model)


ELSA_log_reg = glm(w5racediscrimination2 ~ w5sex, data =ELSAdiscrimination_data_wave5)
summary(ELSA_log_reg)
######################### OR and 95% CI

#ELSA
ELSA_OR = exp(cbind(OR = coef(ELSA_log_reg), confint(ELSA_log_reg)))
ELSA_OR_value = ELSA_OR[1]
ELSA_CI1 = ELSA_OR[3]
ELSA_CI2 = ELSA_OR[5]

#HRS 
HRS_OR = exp(cbind(OR = coef(race_covariates_model), confint(race_covariates_model)))
HRS_OR_value = HRS_OR[1]
HRS_CI1 = HRS_OR[3]
HRS_CI2 = HRS_OR[5]

############# plot covarialte (e., g, employment category) and discrimination race  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

nrow(ELSAdiscrimination_data_wave5)
nrow(HRS2010_discrimination_dataset)

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#dummy variable: 
ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
HRS2010_discrimination_dataset$country = rep(2, times = N_HRS_subset)



#binary variable names below:
unique(ELSAdiscrimination_data_wave5$w5agediscrimination2)
unique(ELSAdiscrimination_data_wave5$w5racediscrimination2)
unique(ELSAdiscrimination_data_wave5$w5racediscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2)

#predictor dummy varibale: country (UK vs USA)
country_cat = c(ELSAdiscrimination_data_wave5$country, 
                HRS2010_discrimination_dataset$country)

#outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)


ELSAdiscrimination_data_wave5$discrim_race_bothCountries = ELSAdiscrimination_data_wave5$w5racediscrimination2
HRS2010_discrimination_dataset$discrim_race_bothCountries = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race

discrimination_race = c(ELSAdiscrimination_data_wave5$discrim_race_bothCountries, 
                          HRS2010_discrimination_dataset$discrim_race_bothCountries)

#covariates pooled from ELSA and HRS  (make sure the order as above)
#DONE: in data merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
unique(ELSAdiscrimination_data_wave5$w5sex)
unique(HRS2010_discrimination_dataset$sex_1_2)

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

data_both_countries_race_discrim <- data.frame(country_cat, 
                                                 discrimination_race, 
                                                 age,
                                                 sex,
                                                 education,
                                                 employment,
                                                 wealth,
                                                 wealth_quantiles,
                                                 married)


################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
################ ####like in Rippon:  in  order  to  determine  any  cross-national  differences  in  race  discrimination. 

fm1_race <- glm(discrimination_race ~ sex, 
                  data = data_both_countries_race_discrim)

fm2_race <- glm(discrimination_race ~  country_cat + sex, 
                  data = data_both_countries_race_discrim)


## various equivalent specifications of the LR test
race_cross_national_diff = lrtest(fm1_race, fm2_race)

race_chi_value_cross_national = race_cross_national_diff$stats[1]

race_pvalue_cross_national = race_cross_national_diff$stats[3]

cross_race_findings = cbind(N_ELSA_subset, 
                            N_HRS_subset, 
                            race_chi_value_cross_national,
                              race_pvalue_cross_national, 
                              ELSA_OR_value, 
                              ELSA_CI1, 
                              ELSA_CI2, 
                              HRS_OR_value, 
                              HRS_CI1, 
                              HRS_CI2)

return(cross_race_findings)
}




