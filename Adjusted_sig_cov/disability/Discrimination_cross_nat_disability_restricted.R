library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)

Disability_adjusted_restricted = function(data_HRS, data_ELSA){
  
  print("Disability_adjusted_restricted")
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
    contengency_table_disability_AND_income  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$wealth_noIRA_HRS2010)
    print(contengency_table_disability_AND_income)
    test_disability_AND_income = chisq.test(contengency_table_disability_AND_income)
    test_disability_AND_income
    
    # there are ....and perceived disability is a binary variable 
    #contengency_table_disability_ANDeducation_level  = table(discrim_disability_HRS, HRS2010_discrimination_dataset$education_levels)
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
    
    
    
    ########### correlates for ELSA study 
    contengency_table_disability_ANDsex_ELSA = table(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2,
                                                     ELSAdiscrimination_data_wave5$w5sex_1_0) 
    test_contengency_table_disability_ANDsex_ELSA = chisq.test(contengency_table_disability_ANDsex_ELSA) #sig
    
    contengency_table_disability_ANDemployment_ELSA =table(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2,
                                                           ELSAdiscrimination_data_wave5$employment)
    test_contengency_table_disability_ANDemployment_ELSA = chisq.test(contengency_table_disability_ANDemployment_ELSA)
    
    
    contengency_table_disability_AND_educaiton_ELSA = table(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2,
                                                            ELSAdiscrimination_data_wave5$ELSA_Education)
    test_contengency_table_disability_ANDeducation_ELSA = chisq.test(contengency_table_disability_AND_educaiton_ELSA)
    
    
    contengency_table_disability_AND_wealth_ELSA = table(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2,
                                                         ELSAdiscrimination_data_wave5$w5wealth)
    
    test_contengency_table_disability_ANDwealth_ELSA = chisq.test(contengency_table_disability_AND_wealth_ELSA)
    
    
    contengency_table_disability_ANDmarital_status_ELSA=table(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2,
                                                              ELSAdiscrimination_data_wave5$w5married)
    
    test_contengency_table_disability_ANDmarital_status_ELSA= chisq.test(contengency_table_disability_ANDmarital_status_ELSA)
    
    
    
    disability_covariates_model_HRS <- glm(HRS2010_reason_discrim1_reason_disability ~  sex_1_0 + employment_allCategories, HRS2010_discrimination_dataset, family = binomial)
    
    summary_HRS_gender_disc = summary(disability_covariates_model_HRS)
    
    summary_HRS_gender_disc_coeff = summary_HRS_gender_disc$coefficients
    
    
    
    HRS_OR = exp(cbind(OR = coef(disability_covariates_model_HRS), confint(disability_covariates_model_HRS)))
    HRS_OR_value = HRS_OR[1]
    HRS_CI1 = HRS_OR[4]
    HRS_CI2 = HRS_OR[7]
    
    
    
    ###### ELSA model including significant correlates: sex, employment, marital status
    
    
    ELSA_log_reg = glm(w5disabilitydiscrimination2 ~ w5sex_1_0+ employment, data =ELSAdiscrimination_data_wave5)
    summary(ELSA_log_reg)
    
    
    ELSA_OR = exp(cbind(OR = coef(ELSA_log_reg), confint(ELSA_log_reg)))
    ELSA_OR_value = ELSA_OR[1]
    ELSA_CI1 = ELSA_OR[4]
    ELSA_CI2 = ELSA_OR[7]
    
    ##### cool chi square bar charts to put all type of discrimination: ADD!!!https://indrajeetpatil.github.io/ggstatsplot/
    
    
    ######## ••• Secondly,  we  conducted MULTIVARIATE LOGISTIC REGRESSION ANALYSIS (Rippon et al)  ••••••
    
    
    ##### plot marital status x discrimination disability #####
    disability_marital_status_plot = ggplot(HRS2010_discrimination_dataset, 
                                            aes(x=marital_status, 
                                                y=as.numeric(HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability) 
                                            )) + 
      geom_point(alpha=.5) +
      stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + 
      ylab("discrimination (disability)") 
    print(disability_marital_status_plot)
    ############# plot employment category and discrimination disability 
    #### ADDD! 
    unique(HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability)
    typeof(HRS2010_discrimination_dataset$HRS2010_reason_discrim1_reason_disability)
    unique(HRS2010_discrimination_dataset$employment_allCategories)
    unique(HRS2010_discrimination_dataset$education_levels)
    unique(HRS2010_discrimination_dataset$marital_status)
    
    ##################################### #### for each country separately, 
    ##################################### #### with perceived age discrimination  as  the  dependent  variable,  
    ##################################### #### adjusting  for  all  covariates. 
    
    ######## ••• Next,  the data from the HRS and ELSA samples were then pooled,  ••••••
    #dummy variable
    ELSAdiscrimination_data_wave5$country = rep(1, times = N_ELSA_subset)
    HRS2010_discrimination_dataset$country = rep(0, times = N_HRS_subset)
    
    
    #binary variable names below:
    unique(ELSAdiscrimination_data_wave5$w5agediscrimination2)
    unique(ELSAdiscrimination_data_wave5$w5sexdiscrimination2)
    unique(ELSAdiscrimination_data_wave5$w5racediscrimination2) 
    unique(ELSAdiscrimination_data_wave5$w5disabilitydiscrimination2) 
    unique(ELSAdiscrimination_data_wave5$w5weightdiscrimination2)
    
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
    
    sex = c(ELSAdiscrimination_data_wave5$w5sex_1_0,
            HRS2010_discrimination_dataset$sex_1_0)
    
    #education_levels
    #not in ELSA ADD, cannot find in the codebook 
    data_both_countries <- data.frame(country_cat, 
                                      discrim_disability, 
                                      marital_status,
                                      employment, 
                                      sex)
    
    #data_both_countries = ELSAdiscrimination_data_wave5
    ##################################### ####Rippon:  and a dummy  variable  indicating  country  was  included  in  the  regression  model 
    ##################################### ####Rippon:  in  order  to  determine  any  cross-national  differences  in  age  discrimination. 
    
    fm1 <- glm(discrim_disability ~  employment + sex, 
               #+ employment, 
               data = data_both_countries)
    fm2 <- glm(discrim_disability ~ country_cat + employment + sex, 
               #+employment, 
               data = data_both_countries)
    
    
    cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
    cross_country_OR_UK = cross_country_OR[2]
    HRS_CI1_UK = cross_country_OR[6]
    HRS_CI2_UK = cross_country_OR[10]
    
    
    cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
    cross_country_OR_USA = cross_country_OR[1]
    HRS_CI1_USA = cross_country_OR[5]
    HRS_CI2_USA = cross_country_OR[9]
    
    
    ## various equivalent specifications of the LR test
    disability_cross_national_diff = lrtest(fm1, fm2)
    disability_cross_national_diff$stats[3]
    
    
    
    # results of the regression model country x disability discrimination (adjusting for sig. covariates):
    
    disability_chi_value_cross_national = disability_cross_national_diff$stats[1]
    
    
    disability_pvalue_cross_national = disability_cross_national_diff$stats[3]
    
    cross_national_disability_findings = cbind(N_ELSA_subset, 
                                               N_HRS_subset, 
                                               disability_chi_value_cross_national,
                                               disability_pvalue_cross_national,
                                               
                                               cross_country_OR_UK, 
                                               HRS_CI1_UK, 
                                               HRS_CI2_UK, 
                                               
                                               cross_country_OR_USA, 
                                               HRS_CI1_USA, 
                                               HRS_CI2_USA, 
                                               
                                               ELSA_OR_value,
                                               ELSA_CI1,
                                               ELSA_CI2,
                                               HRS_OR_value,
                                               HRS_CI1,
                                               HRS_CI2)
    return(cross_national_disability_findings)
}