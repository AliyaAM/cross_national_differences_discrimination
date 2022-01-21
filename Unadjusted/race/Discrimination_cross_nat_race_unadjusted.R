
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)

Unadjusted_race = function(data_ELSA, 
                           data_HRS){


ELSAdiscrimination_data_wave5= data_ELSA
HRS2010_discrimination_dataset = data_HRS

# the file rows are (frop top to bottom: NA, 0, 1, Total)


######***********•••••••••••••• RACE ######***********••••••••••••••
HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race
discrim_race_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race

######## ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived RACE discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived disability is a binary variable 

# there are four age groups (52-59;60-69 , 70-79, >80), and perceived discrimination is a binary variable 

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


######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: 
####################################################### race and
################################################################# 1) employment, 
################################################################# 2) XXXXXXX,
################################################################# 3) XXXXXXX,
########## RACE covariate model ##############
discrim_race_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race


race_covariates_model <- glm(discrim_race_HRS ~  employment_allCategories, HRS2010_discrimination_dataset, family = binomial)
race_covariates_model
summary(race_covariates_model)




ELSA_log_reg = glm(w5racediscrimination2 ~  employment, data =ELSAdiscrimination_data_wave5)
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


##### plot covariates discrimination RACE #####


############# plot covarialte (e., g, employment category) and discrimination race  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#dummy variable:
N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
N_HRS_subset = nrow(HRS2010_discrimination_dataset)


ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
HRS2010_discrimination_dataset$country = rep(0, times = N_HRS_subset)


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
ELSAdiscrimination_data_wave5$w5racediscrimination2
HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race 

discrimination_race = c(ELSAdiscrimination_data_wave5$w5racediscrimination2, 
                        HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_race)

#covariates pooled from ELSA and HRS  (make sure the order as above)
ELSAdiscrimination_data_wave5$w5racediscrimination2
#HRS2010_discrimination_dataset$employment_allCategories
#ELSAdiscrimination_data_wave5$employment

employment = c(ELSAdiscrimination_data_wave5$employment,
               HRS2010_discrimination_dataset$employment_allCategories)

################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 

data_both_countries_race_discrim <- data.frame(country_cat, 
                                               discrimination_race, 
                                               employment)


################ ####Rippon:  in  order  to  determine  any  cross-national  differences  in  race  discrimination. 

contengency_table_race_AND_country = table(data_both_countries_race_discrim$discrimination_race,
                                             data_both_countries_race_discrim$country_cat)
test_contengency_table_race_AND_country = chisq.test(contengency_table_race_AND_country)
test_contengency_table_race_AND_country



## various equivalent specifications of the LR test
#race_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)

summary(test_contengency_table_race_AND_country)

OR_race_cross_national = oddsratio.wald(contengency_table_race_AND_country)
OR_race_cross_national_values = OR_race_cross_national$measure


test_contengency_table_race_AND_country 

summary(test_contengency_table_race_AND_country)

OR_race_cross_national = oddsratio.wald(contengency_table_race_AND_country)
OR_race_cross_national_values = OR_race_cross_national$measure

###############################################

#result table: 
race_chi_value_cross_national = test_contengency_table_race_AND_country$statistic

race_pvalue_cross_national = test_contengency_table_race_AND_country$p.value

cross_race_findings = cbind(N_ELSA_subset, 
                            N_HRS_subset, 
                            race_chi_value_cross_national,
                              race_pvalue_cross_national,
                              
                              OR_race_cross_national_values,
                              
                              ELSA_OR_value, 
                              ELSA_CI1, 
                              ELSA_CI2, 
                              HRS_OR_value, 
                              HRS_CI1, 
                              HRS_CI2)

return(cross_race_findings)
}
