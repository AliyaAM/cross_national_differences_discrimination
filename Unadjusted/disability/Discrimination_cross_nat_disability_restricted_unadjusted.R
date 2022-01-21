library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
library(epitools) # for OR and CI 


#/Users/aliyaamirova/Documents/KCL_postDoc/


Disability_unadjusted_restricted = function(data_HRS, data_ELSA){
  

  ELSAdiscrimination_data_wave5 = subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$w5limill == 1)
  HRS2010_discrimination_dataset = subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$limiting_condition_bin == 1)
  
  N_ELSA_subset = nrow(ELSAdiscrimination_data_wave5)
  N_HRS_subset = nrow(HRS2010_discrimination_dataset)
  
  ######***********•••••••••••••• DISABILITY ######***********••••••••••••••
  discrim_disability_HRS = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability
  unique(HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability)

  ######## ••• Firstly,  we  used  CHI-SQUARE TEST  ••••••
  ##################################### to assess the bivariate relationships between:
  ##################################### ####   1. perceived DISABILITY discrimination
  ##################################### ####   2.  and individual covariates 
  ##################################### ####    ###  in both the United States and England. 
  # there are four age groups (52-59;60-69 , 70-79, >80), and perceived disability is a binary variable 
  contengency_table_disability_ANDage  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$age_groups)
  print(contengency_table_disability_ANDage)
  test_disability_ANDage = chisq.test(contengency_table_disability_ANDage)
  test_disability_ANDage
  
  # there are ... and perceived disability is a binary variable 
  contengency_table_disability_ANDsex  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$sex_1_0)
  print(contengency_table_disability_ANDsex)
  test_table_disability_ANDsex = chisq.test(contengency_table_disability_ANDsex)
  test_table_disability_ANDsex
  
  # there are .... and perceived disability is a binary variable 
  contengency_table_disability_AND_income  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$annual_income_self_employment)
  print(contengency_table_disability_AND_income)
  test_disability_AND_income = chisq.test(contengency_table_disability_AND_income)
  test_disability_AND_income
  
  
  #All responses for the items about education in the subset physical limitation = yes, are NAs (missing) 
  #there are ....and perceived disability is a binary variable 
  #contengency_table_disability_ANDeducation_level  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$education_all_categories)
  #print(contengency_table_disability_ANDeducation_level)
  #test_disability_ANDeducation_level = chisq.test(contengency_table_disability_ANDeducation_level)
  #test_disability_ANDeducation_level
  
  # there are .... and perceived disability is a binary variable 
  contengency_table_disability_ANDemployment  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$employment_allCategories)
  print(contengency_table_disability_ANDemployment)
  test_disability_ANDemployment = chisq.test(contengency_table_disability_ANDemployment)
  test_disability_ANDemployment
  
  # there are .... and perceived disability is a binary variable 
  contengency_table_disability_ANDmarital_status  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$marital_status)
  print(contengency_table_disability_ANDmarital_status)
  test_disability_ANDmarital_status = chisq.test(contengency_table_disability_ANDmarital_status)
  test_disability_ANDmarital_status
  
  ##### cool chi square bar charts to put all type of discrimination: ADD!!!https://indrajeetpatil.github.io/ggstatsplot/
  
  
  ######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
  # the covariates that came out as sig associated are: 
  ####################################################### disability and
  ################################################################# 1) marital status, 
  ################################################################# 2) employment category,
  ################################################################# 3) education level,
  ########## disability covariate model ##############
  
  #unadjusted OR for HRS: 
  
  a = table(discrim_disability_HRS)
  var_0 = a[names(a)==0]
  var_1 = a[names(a)==1]
  var1_prob_HRS = var_1/(var_0+var_1)
  var0_prob_HRS = var_0/(var_0+var_1)
  
  
  OR_HRS = var1_prob_HRS/var0_prob_HRS
  Upper_95_CI_OR_HRS = exp(log(OR_HRS) + 1.96 * sqrt(1/var_0 + 1/var_1))
  Lower_95_CI_OR_HRS = exp(log(OR_HRS) - 1.96 * sqrt(1/var_0 + 1/var_1))
  
  
  
  
  ##### plot marital status x discrimination disability #####
  #disability_marital_status_plot = ggplot(HRS2010_discrimination_dataset, 
  #                                        aes(x=marital_status, 
  #                                            y=as.numeric(HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability) 
  #                                        )) + 
  #  geom_point(alpha=.5) +
  #  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
  #  ylab("discrimination (disability)") 
  # print(disability_marital_status_plot)
  ############# plot employment category and discrimination disability 
  #### ADDD! 
  
  ##################################### #### for each country separately, 
  ##################################### #### with perceived age discrimination  as  the  dependent  variable,  
  ##################################### #### adjusting  for  all  covariates. 
  
  ######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
  #dummy variable
  ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
  HRS2010_discrimination_dataset$country = rep(0, times = N_HRS_subset)
  
  
  #binary variable names below:
  
  
  c = table(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2)
  
  var_0_ELSA = c[names(c)==0]
  var_1_ELSA = c[names(c)==1]
  var1_prob_ELSA = var_1_ELSA/(var_0_ELSA+var_1_ELSA)
  var0_prob_ELSA = var_0_ELSA/(var_0_ELSA+var_1_ELSA)
  
  
  OR_ELSA = var1_prob_ELSA/var0_prob_ELSA
  Upper_95_CI_OR_ELSA = exp(log(OR_ELSA) + 1.96 * sqrt(1/var_0 + 1/var_1))
  Lower_95_CI_OR_ELSA = exp(log(OR_ELSA) - 1.96 * sqrt(1/var_0 + 1/var_1))
  
  
  

  
  
  ELSAdiscrimination_data_wave5$discrim_disability_bothCountries = ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2
  
  HRS2010_discrimination_dataset$discrim_disability_bothCountries = HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability
  
  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(ELSAdiscrimination_data_wave5$country, 
                  HRS2010_discrimination_dataset$country)
  
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  discrim_disability = c(ELSAdiscrimination_data_wave5$discrim_disability_bothCountries,
                         HRS2010_discrimination_dataset$discrim_disability_bothCountries)
  
  #covariates pooled from ELSA and HRS  (make sure the order as above)
  #Done in gender merging file check that they are coded correctly: 0 -retired, 1 - Employed in ELSA..etc, match to HRS
  unique(ELSAdiscrimination_data_wave5$w5married)
  ELSAdiscrimination_data_wave5$marital_status = ELSAdiscrimination_data_wave5$w5married4
  HRS2010_discrimination_dataset$marital_status
  
  marital_status = c(ELSAdiscrimination_data_wave5$marital_status, 
                     HRS2010_discrimination_dataset$marital_status)
  
  
  #HRS2010_discrimination_dataset$employment_allCategories
  #ELSAdiscrimination_data_wave5$employment
  
  employment = c(ELSAdiscrimination_data_wave5$employment,
                 HRS2010_discrimination_dataset$employment_allCategories)
  
  
  #education_levels
  #not in ELSA ADD, cannot find in the codebook 
  data_both_countries <- data.frame(country_cat, 
                                    discrim_disability, 
                                    marital_status,
                                    employment)
  
  #data_both_countries = ELSAdiscrimination_data_wave5
  ##################################### ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
  ##################################### ####Rippon:  in  order  to  determine  any  cross-national  differences  in  age  discrimination. 
  
  
  contengency_table_disability_AND_country  = table(data_both_countries$discrim_disability, data_both_countries$country_cat)
  print(contengency_table_disability_AND_country)
  test_disability_AND_country = chisq.test(contengency_table_disability_AND_country)
  summary(test_disability_AND_country)
  
  OR_disability_cross_national = oddsratio.wald(contengency_table_disability_AND_country)
  
  OR_disability_cross_national_values = OR_disability_cross_national$measure
  
  
  
  #test_disability_AND_country$statistic
  #test_disability_AND_country$p.value
  
  #cross_country_OR = exp(cbind(OR = coef(test_disability_AND_country), confint(test_disability_AND_country)))
  #cross_country_OR_UK = cross_country_OR[1]
  #HRS_CI1_UK = cross_country_OR[3]
  #HRS_CI2_UK = cross_country_OR[5]
  
  
  #cross_country_OR = exp(cbind(OR = coef(test_disability_AND_country), confint(test_disability_AND_country)))
  #cross_country_OR_USA = cross_country_OR[2]
  #HRS_CI1_USA = cross_country_OR[4]
  #HRS_CI2_USA = cross_country_OR[6]
  
  
  # results of the regression model country x disability discrimination (adjusting for sig. covariates):
  
  disability_chi_value_cross_national = test_disability_AND_country$statistic
  disability_pvalue_cross_national = test_disability_AND_country$p.value
  
  
  cross_national_disability_findings = cbind(N_ELSA_subset, 
                                             N_HRS_subset, 
                                             disability_chi_value_cross_national,
                                             disability_pvalue_cross_national,
                                             OR_disability_cross_national_values,
                                             OR_ELSA,
                                             Lower_95_CI_OR_ELSA,
                                             Upper_95_CI_OR_ELSA,
                                             OR_HRS,
                                             Lower_95_CI_OR_HRS,
                                             Upper_95_CI_OR_HRS)
  
  
return(cross_national_disability_findings)
  } 
