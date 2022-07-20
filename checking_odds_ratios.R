
#set the source and output so you can read the files from any computer
library(dplyr)
library(car)
library(stats)
library(ggplot2)
library(scales)
library(arm)
library(stats)


library(tidyverse)



###### Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
######  Set the root location on the user's local machine to save output files.s
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/revisions_frontiers/ageism/"
###### Set the source location on the user's local machine  for sourcing functions 
SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"



###### sourcing code for the unadjusted analysis 
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_comparison_weight.R", sep=""))


###### sourcing the analysis for adjusted models 
source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "adjusted_cross_nat_comparison_weight.R", sep=""))


###### sourcing the analysis for cross-national comparison stratified by SES
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison.R", sep=""))

###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_unadjusted_cros_nat_comparison_weight.R", sep=""))


##### sourcing the analysis for cross-naitonal comparison stritified by SES (adjusted models)
source(paste(SOURCE_ROOT, "SES_adjusted_cros_nat_comparison.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SES_adjusted_cros_nat_comparison_weight.R", sep=""))



###### within country comparison of low SES to high SES 
source(paste(SOURCE_ROOT, "Wcountry_unadjusted_SES.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Wcountry_unadjusted_SES_weight.R", sep=""))


source(paste(SOURCE_ROOT, "Wcountry_adjusted_SES.R", sep=""))
source(paste(SOURCE_ROOT, "Wcountry_adjusted_SES_weight.R", sep=""))


###### sourcing code for the anlaysis of SES x country interaction 
source(paste(SOURCE_ROOT, "SESxCountry_interaction.R", sep=""))
###### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "SESxCountry_interaction_weight.R", sep=""))


########### source file that runs analysis for cross-national comparison between England and USA in numbers of people who experienced discrimination in each of the five situations (eg, less respect), 
# the analysis is restricted to the relevent subset and those who said that they experienced discriminaiton attributed to a type (eg., sex discrim)

source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_Situations.R", sep=""))
##### sourcing separately the SES-stratified comparison code for weight, becuase weight has a threshold such as > and <=
source(paste(SOURCE_ROOT, "Unadjusted_cross_nat_Situations_weight.R", sep=""))


source(paste(SOURCE_ROOT, "Adjusted_cross_nat_Situations.R", sep=""))
source(paste(SOURCE_ROOT, "Adjusted_cross_nat_Situations_weight.R", sep=""))





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



#######################


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


ELSAdiscrimination_data_wave5_before_subsetting$age = ELSAdiscrimination_data_wave5_before_subsetting$w5age
HRS2010_discrimination_dataset_before_subsetting$age  = HRS2010_discrimination_dataset_before_subsetting$continious_age


ELSAdiscrimination_data_wave5_before_subsetting$sex = ELSAdiscrimination_data_wave5_before_subsetting$w5sex_1_0
HRS2010_discrimination_dataset_before_subsetting$sex = HRS2010_discrimination_dataset_before_subsetting$sex_1_0


ELSAdiscrimination_data_wave5_before_subsetting$wealth = ELSAdiscrimination_data_wave5_before_subsetting$wealth
HRS2010_discrimination_dataset_before_subsetting$wealth = HRS2010_discrimination_dataset_before_subsetting$wealth



ELSAdiscrimination_data_wave5_before_subsetting$education = ELSAdiscrimination_data_wave5_before_subsetting$ELSA_Education
HRS2010_discrimination_dataset_before_subsetting$education = HRS2010_discrimination_dataset_before_subsetting$education_levels

unique(ELSAdiscrimination_data_wave5_before_subsetting$education)
unique(HRS2010_discrimination_dataset_before_subsetting$education)

HRS2010_discrimination_dataset_before_subsetting$education = case_when(HRS2010_discrimination_dataset_before_subsetting$education == 0 ~ 0, 
                                                                       HRS2010_discrimination_dataset_before_subsetting$education == 1 ~ 1, 
                                                                       HRS2010_discrimination_dataset_before_subsetting$education == 2 ~ 2,
                                                                       HRS2010_discrimination_dataset_before_subsetting$education == 3 ~ 3)

HRS2010_discrimination_dataset_before_subsetting$education = as.factor(HRS2010_discrimination_dataset_before_subsetting$education)
ELSAdiscrimination_data_wave5_before_subsetting$education = as.factor(ELSAdiscrimination_data_wave5_before_subsetting$education)


ELSAdiscrimination_data_wave5_before_subsetting$employment = ELSAdiscrimination_data_wave5_before_subsetting$employment
HRS2010_discrimination_dataset_before_subsetting$employment =  HRS2010_discrimination_dataset_before_subsetting$employment_allCategories

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

unique(ELSAdiscrimination_data_wave5_before_subsetting$w5agediscrimination2)
unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_reason_discrim1_reason_age)

ELSAdiscrimination_data_wave5_before_subsetting$age_discrim =  ELSAdiscrimination_data_wave5_before_subsetting$w5agediscrimination2 
HRS2010_discrimination_dataset_before_subsetting$age_discrim = HRS2010_discrimination_dataset_before_subsetting$HRS2010_reason_discrim1_reason_age



######  dummy code the countries 


ELSAdiscrimination_data_wave5_before_subsetting$country = rep(0, times = nrow(ELSAdiscrimination_data_wave5_before_subsetting))
HRS2010_discrimination_dataset_before_subsetting$country = rep(1, times = nrow(HRS2010_discrimination_dataset_before_subsetting))


######################### testing physical lim 

unique(ELSAdiscrimination_data_wave5_before_subsetting$w5limill)
unique(HRS2010_discrimination_dataset_before_subsetting$limiting_condition_bin)


ELSA_lim_dataset = subset(ELSAdiscrimination_data_wave5_before_subsetting, ELSAdiscrimination_data_wave5_before_subsetting$w5limill == 1)
HRS_lim_dataset = subset(HRS2010_discrimination_dataset_before_subsetting, HRS2010_discrimination_dataset_before_subsetting$limiting_condition_bin == 1) 



lim_country_cat_both = c(ELSA_lim_dataset$country,
                         HRS_lim_dataset$country)


disability_discrim = c(ELSA_lim_dataset$w5disabilitydiscrimination2,
                       HRS_lim_dataset$HRS2010_reason_discrim1_reason_disability)



#################### testing physical lim adjusted 

lim_age_both_countries = c(ELSA_lim_dataset$age, 
                           HRS_lim_dataset$age)


lim_sex_both_countries = c(ELSA_lim_dataset$sex,
                           HRS_lim_dataset$sex)


lim_wealth_both_countries = c(ELSA_lim_dataset$wealth,
                              HRS_lim_dataset$wealth) 


fm1_lim <- glm(disability_discrim ~  lim_age_both_countries
           + lim_sex_both_countries
           + lim_wealth_both_countries)

fm2_lim <- glm(disability_discrim ~ lim_country_cat_both 
           +  lim_age_both_countries
           + lim_sex_both_countries
           + lim_wealth_both_countries) 


lim_cross_country_OR = exp(cbind(OR = coef(fm2_lim), confint(fm2_lim)))
lim_cross_country_OR_UK = lim_cross_country_OR[2, 1]
lim_CI1_UK = lim_cross_country_OR[2, 2]
lim_CI2_UK = lim_cross_country_OR[2, 3]

#####
#####

lim_cross_country_OR_USA = lim_cross_country_OR[1, 1]
lim_CI1_USA = lim_cross_country_OR[1, 2]
lim_CI2_USA = lim_cross_country_OR[1, 3]

## various equivalent specifications of the LR test
lim_cross_national_diff = lrtest(fm1_lim, fm2_lim)

lim_chi_value_cross_national = lim_cross_national_diff$stats[1]
lim_pvalue_cross_national = lim_cross_national_diff$stats[3]



#############################################

######################## testing ageism 

country_cat_both = c(ELSAdiscrimination_data_wave5_before_subsetting$country,
                     HRS2010_discrimination_dataset_before_subsetting$country)


ageism = c(ELSAdiscrimination_data_wave5_before_subsetting$age_discrim,
           HRS2010_discrimination_dataset_before_subsetting$age_discrim) 


contengency_table_discrimination_AND_country  = table(country_cat_both, ageism)
print(contengency_table_discrimination_AND_country)

test_discrimination_AND_country = chisq.test(contengency_table_discrimination_AND_country)
summary(test_discrimination_AND_country)

OR_discrimination_cross_national = oddsratio(contengency_table_discrimination_AND_country)

OR_discrimination_cross_national_value = OR_discrimination_cross_national$measure[2, 1]
OR_discrimination_cross_national_values_CI_lower = OR_discrimination_cross_national$measure[2, 2]
OR_discrimination_cross_national_values_CI_upper = OR_discrimination_cross_national$measure[2, 3]



#################### testing ageism adjusted 


age_both_countries = c(ELSAdiscrimination_data_wave5_before_subsetting$age, 
                      HRS2010_discrimination_dataset_before_subsetting$age)


sex_both_countries = c(ELSAdiscrimination_data_wave5_before_subsetting$sex,
                       HRS2010_discrimination_dataset_before_subsetting$sex)


wealth_both_countries = c(ELSAdiscrimination_data_wave5_before_subsetting$wealth,
                          HRS2010_discrimination_dataset_before_subsetting$wealth) 



fm1 <- glm(ageism ~  age_both_countries
           + sex_both_countries
           + wealth_both_countries)

fm2 <- glm(ageism ~ country_cat_both 
           +  age_both_countries
           + sex_both_countries
           + wealth_both_countries) 
           

cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
cross_country_OR_UK = cross_country_OR[2, 1]
CI1_UK = cross_country_OR[2, 2]
CI2_UK = cross_country_OR[2, 3]

#####
#####

cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
cross_country_OR_USA = cross_country_OR[1, 1]
CI1_USA = cross_country_OR[1, 2]
CI2_USA = cross_country_OR[1, 3]

## various equivalent specifications of the LR test
cross_national_diff = lrtest(fm1, fm2)

chi_value_cross_national = cross_national_diff$stats[1]
pvalue_cross_national = cross_national_diff$stats[3]
