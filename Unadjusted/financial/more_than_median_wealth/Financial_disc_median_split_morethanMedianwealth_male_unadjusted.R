library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
library(dplyr)


Financial_disc_adjusted_morethan_median_male = function(data_ELSA, 
                                                       data_HRS){
  
  
  print("Financial_disc_adjusted_morethan_median_male")
  
#ELSAdiscrimination_data_wave5 = subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$w5wealthq ==5)
#HRS2010_discrimination_dataset = subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$Percentile_wealth_noIRA_HRS2010 == 5)

#ELSAdiscrimination_data_wave5$w5wealth
#HRS2010_discrimination_dataset$wealth_noIRA_HRS2010
#Office for national statistic report: For households with an HRP of 55 to 64 years, average net property wealth was £255,800 in April 2016 to March 2018. This was slightly higher (7%) for the households where the HRP was over 65 years, where average property wealth was £272,900.
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
HRS2010_discrimination_dataset_before_subsetting$meadian_wealth_bin_HRS = case_when(HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010 >=787500 ~ '2', 
                                                                                    HRS2010_discrimination_dataset_before_subsetting$wealth_noIRA_HRS2010 < 787500 ~ '1')

ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0



#subsetting by median wealth (less than median/more than wealth distribution's median) and sex = female (0 is male and 1 is female in both datasets):

ELSAdiscrimination_data_wave5 = subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$median_wealth_bin_ELSA == 2 & ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0 == 1) 
HRS2010_discrimination_dataset = subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$meadian_wealth_bin_HRS == 2 & HRS2010_discrimination_dataset_before_subsetting$sex_1_0 == 1)



N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
N_HRS_subset = nrow(HRS2010_discrimination_dataset)
#####***********•••••••••••••• FINANCIAL ######***********••••••••••••••
discrim_financial_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_financial  

ELSAdiscrimination_data_wave5$w5discrim_financial2
######## ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived FINANCIAL discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived FINANCIAL is a binary variable 


contengency_table_financial_ANDage  = table(discrim_financial_HRS, HRS2010_discrimination_dataset$age_groups)
print(contengency_table_financial_ANDage)
test_financial_ANDage = chisq.test(contengency_table_financial_ANDage)
test_financial_ANDage 

# there are ... and perceived financial is a binary variable 
contengency_table_financial_ANDsex  = table(discrim_financial_HRS, HRS2010_discrimination_dataset$sex_1_0)
print(contengency_table_financial_ANDsex)
test_table_financial_ANDsex = chisq.test(contengency_table_financial_ANDsex)
test_table_financial_ANDsex 

# there are .... and perceived financial is a binary variable 
contengency_table_financial_AND_income  = table(discrim_financial_HRS, HRS2010_discrimination_dataset$annual_income_self_employment)
print(contengency_table_financial_AND_income)
test_financial_AND_income = chisq.test(contengency_table_financial_AND_income)
test_financial_AND_income

# there are ....and perceived financial is a binary variable 
contengency_table_financial_ANDeducation_level  = table(discrim_financial_HRS, HRS2010_discrimination_dataset$education_levels)
print(contengency_table_financial_ANDeducation_level)
test_financial_ANDeducation_level = chisq.test(contengency_table_financial_ANDeducation_level)
test_financial_ANDeducation_level  

# there are .... and perceived financial is a binary variable 
contengency_table_financial_ANDemployment  = table(discrim_financial_HRS, HRS2010_discrimination_dataset$employment_allCategories)
print(contengency_table_financial_ANDemployment)
test_financial_ANDemployment = chisq.test(contengency_table_financial_ANDemployment)
test_financial_ANDemployment 

# there are .... and perceived financial is a binary variable 
contengency_table_financial_ANDmarital_status  = table(discrim_financial_HRS, HRS2010_discrimination_dataset$marital_status)
print(contengency_table_financial_ANDmarital_status)
test_financial_ANDmarital_status = chisq.test(contengency_table_financial_ANDmarital_status)
test_financial_ANDmarital_status


#wealth and sex discrimination correlation in HRS 
contengency_table_financial_ANDwealth = table(discrim_financial_HRS, HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)
print(contengency_table_financial_ANDwealth)
test_financial_ANDwealth = chisq.test(contengency_table_financial_ANDwealth)
test_financial_ANDwealth



#wealth quantile and sex discrimination !!!!!!!!!!!!!!
wealth_noPension_HRS2010 = HRS2010_discrimination_dataset$H10ATOTW
quantile(wealth_noPension_HRS2010, probs =c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1))
Percentile_wealth_HRS2010 = recode(wealth_noPension_HRS2010, 
                                   "-2760000:5000='20%';
                                   5001:69000='40%';
                                   69001:180000='60%';
                                   180000:425400='80%';
                                   425401:27662000='100%'")

#HRS2010_discrimination_dataset$Percentile_wealth_HRS2010 = Percentile_wealth_HRS2010
#contengency_table_financial_ANDwealthPercentile = table(discrim_financial_HRS, Percentile_wealth_HRS2010)
#print(contengency_table_financial_ANDwealthPercentile)
#test_financial_ANDwealth_percentile = chisq.test(contengency_table_financial_ANDwealthPercentile)
#test_financial_ANDwealth_percentile 


#######################


########### correlates for ELSA study 
contengency_table_financial_ANDsex_ELSA = table(ELSAdiscrimination_data_wave5$w5discrim_financial2,
                                                ELSAdiscrimination_data_wave5$w5sex_1_0) 
test_contengency_table_financial_ANDsex_ELSA = chisq.test(contengency_table_financial_ANDsex_ELSA)
test_contengency_table_financial_ANDsex_ELSA 

contengency_table_financial_ANDemployment_ELSA =table(ELSAdiscrimination_data_wave5$w5discrim_financial2,
                                                      ELSAdiscrimination_data_wave5$employment)
test_contengency_table_financial_ANDemployment_ELSA = chisq.test(contengency_table_financial_ANDemployment_ELSA)
test_contengency_table_financial_ANDemployment_ELSA 

contengency_table_financial_AND_educaiton_ELSA = table(ELSAdiscrimination_data_wave5$w5discrim_financial2,
                                                       ELSAdiscrimination_data_wave5$ELSA_Education)
test_contengency_table_financial_ANDeducation_ELSA = chisq.test(contengency_table_financial_AND_educaiton_ELSA)
test_contengency_table_financial_ANDeducation_ELSA

contengency_table_financial_AND_wealth_ELSA = table(ELSAdiscrimination_data_wave5$w5discrim_financial2,
                                                    ELSAdiscrimination_data_wave5$w5wealth)

test_contengency_table_financial_ANDwealth_ELSA = chisq.test(contengency_table_financial_AND_wealth_ELSA)
test_contengency_table_financial_ANDwealth_ELSA


contengency_table_financial_ANDmarital_status_ELSA=table(ELSAdiscrimination_data_wave5$w5discrim_financial2,
                                                         ELSAdiscrimination_data_wave5$w5married)

test_contengency_table_financial_ANDmarital_status_ELSA= chisq.test(contengency_table_financial_ANDmarital_status_ELSA)
test_contengency_table_financial_ANDmarital_status_ELSA

#wealth 
ELSAdiscrimination_data_wave5$w5wealthq
contengency_table_financial_AND_wealthq_ELSA = table(ELSAdiscrimination_data_wave5$w5discrim_financial2,
                                                     ELSAdiscrimination_data_wave5$w5wealthq)
test_contengency_table_financial_ANDwealthq_ELSA = chisq.test(contengency_table_financial_AND_wealthq_ELSA)
test_contengency_table_financial_ANDwealthq_ELSA 

######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: 
####################################################### financial and
################################################################# 1) age
################################################################# 2) education level,
################################################################# 3) employment,
################################################################# 4) sex,
########## financial covariate model ##############
#HRS2010_discrimination_dataset$age_groups
#HRS2010_discrimination_dataset$continious_age

financial_covariates_model <- glm(HRS2010_reason_discrim1_reason_financial ~ wealth_noIRA_HRS2010, HRS2010_discrimination_dataset, family = binomial)
financial_covariates_model
summary(financial_covariates_model)

#ELSA glm 
ELSA_log_reg = glm(w5discrim_financial2 ~ w5wealth, data =ELSAdiscrimination_data_wave5)
summary(ELSA_log_reg)

######################### OR and 95% CI

#ELSA
ELSA_OR = exp(cbind(OR = coef(ELSA_log_reg), confint(ELSA_log_reg)))
ELSA_OR_value = ELSA_OR[1]
ELSA_CI1 = ELSA_OR[3]
ELSA_CI2 = ELSA_OR[5]

#HRS 
HRS_OR = exp(cbind(OR = coef(financial_covariates_model), confint(financial_covariates_model)))
HRS_OR_value = HRS_OR[1]
HRS_CI1 = HRS_OR[3]
HRS_CI2 = HRS_OR[5]

##### plot marital status x discrimination FINANCIAL #####


############# plot covarialte (e., g, employment category) and discrimination financial  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#check number of rows: 
nrow(ELSAdiscrimination_data_wave5)
nrow(HRS2010_discrimination_dataset)
#dummy variable: 

ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
HRS2010_discrimination_dataset$country = rep(0, times = N_HRS_subset)


#binary variable names below:


#predictor dummy varibale: country (UK vs USA)
country_cat = c(ELSAdiscrimination_data_wave5$country, 
                HRS2010_discrimination_dataset$country)

#outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)

ELSAdiscrimination_data_wave5$discrim_financial_bothCountries = ELSAdiscrimination_data_wave5$w5discrim_financial2
HRS2010_discrimination_dataset$discrim_financial_bothCountries = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_financial

discrimination_financial = c(ELSAdiscrimination_data_wave5$discrim_financial_bothCountries, 
                             HRS2010_discrimination_dataset$discrim_financial_bothCountries)

#covariates pooled from ELSA and HRS  (make sure the order as above)


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

data_both_countries_financial_discrim <- data.frame(country_cat, 
                                                    discrimination_financial, 
                                                    age,
                                                    sex,
                                                    education,
                                                    employment,
                                                    wealth,
                                                    wealth_quantiles,
                                                    married)


################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
################ ####Rippon:  in  order  to  determine  any  cross-national  differences  in  age  discrimination. 

contengency_table_financial_AND_country = table(data_both_countries_financial_discrim$discrimination_financial,
                                                data_both_countries_financial_discrim$country_cat)
test_contengency_table_financial_AND_country = chisq.test(contengency_table_financial_AND_country)
test_contengency_table_financial_AND_country



## various equivalent specifications of the LR test
#financial_cross_national_diff= lrtest(fm1_finacial, fm2_finacial)

summary(test_contengency_table_financial_AND_country)

OR_financial_cross_national = oddsratio.wald(contengency_table_financial_AND_country)
OR_financial_cross_national_values = OR_financial_cross_national$measure

###############################################

#result table: 
financial_chi_value_cross_national = test_contengency_table_financial_AND_country$statistic

financial_pvalue_cross_national = test_contengency_table_financial_AND_country$p.value

cross_financial_findings = cbind(N_ELSA_subset, 
                                 N_HRS_subset, 
                                 financial_chi_value_cross_national,
                                 financial_pvalue_cross_national,
                                 
                                 OR_financial_cross_national_values,
                                 
                                 ELSA_OR_value, 
                                 ELSA_CI1, 
                                 ELSA_CI2, 
                                 HRS_OR_value, 
                                 HRS_CI1, 
                                 HRS_CI2)

financial_results = rbind(cross_financial_findings)
return(financial_results)
}

