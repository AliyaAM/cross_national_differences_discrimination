library(epitools) #for odds ratio wald 

library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)

DIscrimination_cross_nat_sexuality = function(data_ELSA, 
                                   data_HRS){

######***********•••••••••••••• SEXUALITY ######***********••••••••••••••
discrim_sexuality_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_sexuality 
ELSAdiscrimination_data_wave5$w5discrim_sexuality2 

N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
N_HRS_subset = nrow(HRS2010_discrimination_dataset)

######## ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
##################################### to assess the bivariate relationships between:
##################################### ####   1. perceived SEXUALITY discrimination
##################################### ####   2.  and individual covariates 
##################################### ####    ###  in both the United States and England. 
# there are four age groups (52-59;60-69 , 70-79, >80), and perceived SEXUALITY is a binary variable 

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


# nothing correlated with sexuality discrimination therefore the OR nd CI are calculated as follows: 

a = table(discrim_sexuality_HRS)
var_0 = a[names(a)==0]
var_1 = a[names(a)==1]
var1_prob_HRS = var_1/(var_0+var_1)
var0_prob_HRS = var_0/(var_0+var_1)


OR_HRS = var1_prob_HRS/var0_prob_HRS
Upper_95_CI_OR_HRS = exp(log(OR_HRS) + 1.96 * sqrt(1/var_0 + 1/var_1))
Lower_95_CI_OR_HRS = exp(log(OR_HRS) - 1.96 * sqrt(1/var_0 + 1/var_1))

########################################

c = table(ELSAdiscrimination_data_wave5$w5discrim_sexuality2)

var_0_ELSA = c[names(c)==0]
var_1_ELSA = c[names(c)==1]
var1_prob_ELSA = var_1_ELSA/(var_0_ELSA+var_1_ELSA)
var0_prob_ELSA = var_0_ELSA/(var_0_ELSA+var_1_ELSA)


OR_ELSA = var1_prob_ELSA/var0_prob_ELSA
Upper_95_CI_OR_ELSA = exp(log(OR_ELSA) + 1.96 * sqrt(1/var_0 + 1/var_1))
Lower_95_CI_OR_ELSA = exp(log(OR_ELSA) - 1.96 * sqrt(1/var_0 + 1/var_1))


######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
# the covariates that came out as sig associated are: 
####################################################### SEXUALITY and
################################################################# 1) XXXXXXX, 
################################################################# 2) XXXXXXX,
################################################################# 3) XXXXXXX,
########## SEXUALITY covariate model ##############



# NO COVARIATES FOR SEXUALITY !!!!!!!!!! @@@@@@@@@@@ @ @@@@@@@@@@@@@@@

##### plot marital status x discrimination SEXUALITY #####




############# plot covarialte (e., g, employment category) and discrimination SEXUALITY  
#### ADDD! 

##################################### #### for each country separately, 
##################################### #### with perceived age discrimination  as  the  dependent  variable,  
##################################### #### adjusting  for  all  covariates. 

######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
#dummy variable: 
ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
HRS2010_discrimination_dataset$country = rep(0, times = N_HRS_subset)



#binary variable names below:
unique(ELSAdiscrimination_data_wave5$w5agediscrimination2)
unique(ELSAdiscrimination_data_wave5$w5sexdiscrimination2)
unique(ELSAdiscrimination_data_wave5$w5sexualitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2) 
unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2)

#predictor dummy varibale: country (UK vs USA)
country_cat = c(ELSAdiscrimination_data_wave5$country, 
                HRS2010_discrimination_dataset$country)

#outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)

discrimination_sexuality = c(ELSAdiscrimination_data_wave5$w5discrim_sexuality2,
                             HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_sexuality)


data_both_countries_sexuality_discrim = data.frame(discrimination_sexuality,
                                                   country_cat)

#covariates pooled from ELSA and HRS  (make sure the order as above)
#check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS


################ ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
################ ####Rippon:  in  order  to  determine  any  cross-national  differences  in  age  discrimination. 


contengency_table_sexuality_country  = table(data_both_countries_sexuality_discrim$discrimination_sexuality,
                                             data_both_countries_sexuality_discrim$country_cat)
print(contengency_table_sexuality_country)
test_sexuality_country = chisq.test(contengency_table_sexuality_country)
test_sexuality_country


ch_sq_sexuality = test_sexuality_country$statistic
pvalue_sexuality = test_sexuality_country$p.value



OR_sexuality_cross_national = oddsratio.wald(contengency_table_sexuality_country)
OR_sexuality_cross_national_values = OR_sexuality_cross_national$measure



summary(test_sexuality_country)


sexuality_cross_national_diff = c(ch_sq_sexuality, pvalue_sexuality)

#result table: 
#sexuality_chi_value_cross_national = sexuality_cross_national_diff$stats[1]

#sexuality_pvalue_cross_national = sexuality_cross_national_diff$stats[3]

#cross_sexuality_findings = cbind(sexuality_chi_value_cross_national,
#                            sexuality_pvalue_cross_national)


sexuality_results = cbind(N_ELSA_subset, 
                          N_HRS_subset, 
                          sexuality_cross_national_diff, 
                          OR_sexuality_cross_national_values, 
                          
                          OR_ELSA,
                          Lower_95_CI_OR_ELSA,
                          Upper_95_CI_OR_ELSA,
                          OR_HRS,
                          Lower_95_CI_OR_HRS,
                          Upper_95_CI_OR_HRS)

#write.csv(sexuality_results, file = "/Users/aliyaamirova/Documents/KCL_postDoc/Data_analysis/sexuality_results.csv")

return(sexuality_results)
}
