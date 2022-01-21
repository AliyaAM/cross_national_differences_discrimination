
library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)


weight_OBESE_unadjusted = function(data_ELSA, 
                                   data_HRS){

# BMI from wave 4 was used for subsetting the sample becuause there is no bmi at wave 5
ELSAdiscrimination_data_wave5 = subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$w4bmi_clean >29.9)
HRS2010_discrimination_dataset = subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$HRS2010_BMI>29.9)

######***********•••••••••••••• weight ######***********••••••••••••••
discrim_weight_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_weight 

N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
N_HRS_subset = nrow(HRS2010_discrimination_dataset)

### ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived weight discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived discrimination is a binary variable 

#continious_age

contengency_table_weight_ANDage  = table(discrim_weight_HRS, HRS2010_discrimination_dataset$age_groups)
print(contengency_table_weight_ANDage)
test_weight_ANDage = chisq.test(contengency_table_weight_ANDage)
test_weight_ANDage #sig for obese 

# there are ... and perceived weight is a binary variable 
contengency_table_weight_ANDsex  = table(discrim_weight_HRS, HRS2010_discrimination_dataset$sex_1_0)
print(contengency_table_weight_ANDsex)
test_table_weight_ANDsex = chisq.test(contengency_table_weight_ANDsex)
test_table_weight_ANDsex # sig for obese 

# there are .... and perceived weight is a binary variable 
contengency_table_weight_AND_income  = table(discrim_weight_HRS, HRS2010_discrimination_dataset$annual_income_self_employment)
print(contengency_table_weight_AND_income)
test_weight_AND_income = chisq.test(contengency_table_weight_AND_income)
test_weight_AND_income

# there are ....and perceived weight is a binary variable 
contengency_table_weight_ANDeducation_level  = table(discrim_weight_HRS, HRS2010_discrimination_dataset$education_levels)
print(contengency_table_weight_ANDeducation_level)
test_weight_ANDeducation_level = chisq.test(contengency_table_weight_ANDeducation_level)
test_weight_ANDeducation_level

# there are .... and perceived weight is a binary variable 
contengency_table_weight_ANDemployment  = table(discrim_weight_HRS, HRS2010_discrimination_dataset$employment_allCategories)
print(contengency_table_weight_ANDemployment)
test_weight_ANDemployment = chisq.test(contengency_table_weight_ANDemployment)
test_weight_ANDemployment #sig for obese 

# there are .... and perceived weight is a binary variable 
contengency_table_weight_ANDmarital_status  = table(discrim_weight_HRS, HRS2010_discrimination_dataset$marital_status)
print(contengency_table_weight_ANDmarital_status)
test_weight_ANDmarital_status = chisq.test(contengency_table_weight_ANDmarital_status)
test_weight_ANDmarital_status

#wealth and sex discrimination correlation in HRS 
contengency_table_weight_ANDwealth = table(discrim_weight_HRS, HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)
print(contengency_table_weight_ANDwealth)
test_weight_ANDwealth = chisq.test(contengency_table_weight_ANDwealth)
test_weight_ANDwealth

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
contengency_table_weight_ANDwealthPercentile = table(discrim_weight_HRS, Percentile_wealth_HRS2010)
print(contengency_table_weight_ANDwealthPercentile)
test_weight_ANDwealth_percentile = chisq.test(contengency_table_weight_ANDwealthPercentile)
test_weight_ANDwealth_percentile #sig for overweight


########### correlates for ELSA study 
contengency_table_weight_ANDage_ELSA =table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                            ELSAdiscrimination_data_wave5$w5age)
test_contengency_table_weight_ANDage_ELSA = chisq.test(contengency_table_weight_ANDage_ELSA)
test_contengency_table_weight_ANDage_ELSA #sig for obese 

contengency_table_weight_ANDsex_ELSA = table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                             ELSAdiscrimination_data_wave5$w5sex_1_0) 
test_contengency_table_weight_ANDsex_ELSA = chisq.test(contengency_table_weight_ANDsex_ELSA)
test_contengency_table_weight_ANDsex_ELSA 

contengency_table_weight_ANDemployment_ELSA =table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                                   ELSAdiscrimination_data_wave5$employment)
test_contengency_table_weight_ANDemployment_ELSA = chisq.test(contengency_table_weight_ANDemployment_ELSA)
test_contengency_table_weight_ANDemployment_ELSA #sig for obese 

contengency_table_weight_AND_educaiton_ELSA = table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                                    ELSAdiscrimination_data_wave5$ELSA_Education)
test_contengency_table_weight_ANDeducation_ELSA = chisq.test(contengency_table_weight_AND_educaiton_ELSA)
test_contengency_table_weight_ANDeducation_ELSA
# too few cases to run nine categories of education vs weight 

contengency_table_weight_AND_wealth_ELSA = table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                                 ELSAdiscrimination_data_wave5$w5wealth)
test_contengency_table_weight_ANDwealth_ELSA = chisq.test(contengency_table_weight_AND_wealth_ELSA)
test_contengency_table_weight_ANDwealth_ELSA

ELSAdiscrimination_data_wave5$w5wealthq
contengency_table_weight_AND_wealthq_ELSA = table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                                  ELSAdiscrimination_data_wave5$w5wealthq)
test_contengency_table_weight_ANDwealthq_ELSA = chisq.test(contengency_table_weight_AND_wealthq_ELSA)
test_contengency_table_weight_ANDwealthq_ELSA #sig for obese


contengency_table_weight_ANDmarital_status_ELSA=table(ELSAdiscrimination_data_wave5$w5weightdiscrimination2,
                                                      ELSAdiscrimination_data_wave5$w5married)

test_contengency_table_weight_ANDmarital_status_ELSA= chisq.test(contengency_table_weight_ANDmarital_status_ELSA)
test_contengency_table_weight_ANDmarital_status_ELSA

######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: sex, education level and employment 
####################################################### weight and
################################################################# 1) sex, 
################################################################# 2) education level, 
################################################################# 3) employment,
########## weight covariate model ##############


weight_covariates_model <- glm(HRS2010_reason_discrim1_reason_weight ~continious_age + sex_1_0 + employment_allCategories,  HRS2010_discrimination_dataset, family = binomial)
weight_covariates_model
summary(weight_covariates_model)


ELSA_log_reg = glm(w5weightdiscrimination2 ~ w5age + employment + w5wealthq, data =ELSAdiscrimination_data_wave5)
summary(ELSA_log_reg)
######################### OR and 95% CI

#ELSA
ELSA_OR = exp(cbind(OR = coef(ELSA_log_reg), confint(ELSA_log_reg)))
ELSA_OR_value = ELSA_OR[1]
ELSA_CI1 = ELSA_OR[5]
ELSA_CI2 = ELSA_OR[9]

#HRS 
HRS_OR = exp(cbind(OR = coef(weight_covariates_model), confint(weight_covariates_model)))
HRS_OR_value = HRS_OR[1]
HRS_CI1 = HRS_OR[5]
HRS_CI2 = HRS_OR[9]

############# plot covarialte (e., g, employment category) and discrimination weight  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

nrow(ELSAdiscrimination_data_wave5)
nrow(HRS2010_discrimination_dataset)

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#dummy variable: 
ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
HRS2010_discrimination_dataset$country = rep(0, times = N_HRS_subset)



#binary variable names below:
unique(ELSAdiscrimination_data_wave5$w5agediscrimination2)
unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2)

#predictor dummy varibale: country (UK vs USA)
country_cat = c(ELSAdiscrimination_data_wave5$country, 
                HRS2010_discrimination_dataset$country)

#outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)


ELSAdiscrimination_data_wave5$discrim_weight_bothCountries = ELSAdiscrimination_data_wave5$w5weightdiscrimination2
HRS2010_discrimination_dataset$discrim_weight_bothCountries = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_weight

discrimination_weight = c(ELSAdiscrimination_data_wave5$discrim_weight_bothCountries, 
                          HRS2010_discrimination_dataset$discrim_weight_bothCountries)

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

data_both_countries_weight_discrim <- data.frame(country_cat, 
                                                 discrimination_weight, 
                                                 age,
                                                 sex,
                                                 education,
                                                 employment,
                                                 wealth,
                                                 wealth_quantiles,
                                                 married)


################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
################ ####like in Rippon:  in  order  to  determine  any  cross-national  differences  in  weight  discrimination. 
# for overweight the significant correlate was wealth only 
# for obese the significant correlates were wealth, age, sex, employment 



contengency_table_weight_AND_country = table(data_both_countries_weight_discrim$discrimination_weight,
                                             data_both_countries_weight_discrim$country_cat)
test_contengency_table_weight_AND_country = chisq.test(contengency_table_weight_AND_country)
test_contengency_table_weight_AND_country



## various equivalent specifications of the LR test
#weight_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)

summary(test_contengency_table_weight_AND_country)

OR_weight_cross_national = oddsratio.wald(contengency_table_weight_AND_country)
OR_weight_cross_national_values = OR_weight_cross_national$measure

###############################################

#result table: 
weight_chi_value_cross_national = test_contengency_table_weight_AND_country$statistic

weight_pvalue_cross_national = test_contengency_table_weight_AND_country$p.value

cross_weight_findings = cbind(N_ELSA_subset, 
                              N_HRS_subset, 
                              weight_chi_value_cross_national,
                              weight_pvalue_cross_national,
                              
                              OR_weight_cross_national_values,
                              
                              ELSA_OR_value, 
                              ELSA_CI1, 
                              ELSA_CI2, 
                              HRS_OR_value, 
                              HRS_CI1, 
                              HRS_CI2)


return(cross_weight_findings)
}
