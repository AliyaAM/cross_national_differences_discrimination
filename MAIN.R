
#set the source and output so you can read the files from any computer
library(dplyr)
## Set the root directory to look for source code.
SOURCE_data_ROOT = "/Users/aliya/my_docs/KCL_postDoc/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/Cross_national_diffs_results/"

SOURCE_ROOT = "/Users/aliya/my_docs/proj/cross_national_differences_discrimination/"

#read old files, call them something else 
ELSAdiscrimination_data_wave5_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/DATA_ELSA/ELSAdiscrimination_data_wave5.csv", sep=""))
HRS2010_discrimination_dataset_ALL = read.csv(paste(SOURCE_data_ROOT, "Data_analysis/HRS_2010_data/HRS2010_discrimination_dataset_new.csv", sep=""))

#subset to those 50 years old and older
ELSAdiscrimination_data_wave5_age50 = subset(ELSAdiscrimination_data_wave5_ALL, w5age >= 50) 

HRS2010_discrimination_dataset_age50 = subset(HRS2010_discrimination_dataset_ALL, HRS2010_discrimination_dataset_ALL$continious_age >=50)

#subet to those who responded to the discrimination items: 
ELSAdiscrimination_data_wave5_before_subsetting = subset(ELSAdiscrimination_data_wave5_age50, ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 0 | ELSAdiscrimination_data_wave5_age50$w5discrim_bin2 == 1)

#drop rows where the responses across all situations are all NAs in the HRS study 
HRS2010_discrimination_dataset_before_subsetting = subset(HRS2010_discrimination_dataset_age50,HRS2010_discrimination_dataset_age50$HRS2010_discrim_lessrespect!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_harassed!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_poorerservice!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_notclever!= "NA" | HRS2010_discrimination_dataset_age50$HRS2010_discrim_medical!= "NA")
ELSAdiscrimination_data_wave5_before_subsetting$w5age = as.integer(ELSAdiscrimination_data_wave5_before_subsetting$w5age)

#check these are correct answers in the bin var: 0 or 1
Total_N_ELSA = nrow(ELSAdiscrimination_data_wave5_before_subsetting)
mean_age_ELSA = mean(ELSAdiscrimination_data_wave5_before_subsetting$w5age)
sd_age_ELSA = sd(ELSAdiscrimination_data_wave5_before_subsetting$w5age)
N_women_ELSA = nrow(ELSAdiscrimination_data_wave5_before_subsetting$sex_1_0 == 0)
  
Total_N_HRS = nrow(HRS2010_discrimination_dataset_before_subsetting)
mean_age_HRS = mean(HRS2010_discrimination_dataset_before_subsetting$continious_age)
sd_age_HRS = sd(HRS2010_discrimination_dataset_before_subsetting$continious_age)
N_women_HRS = nrow(HRS2010_discrimination_dataset_before_subsetting$sex_1_0 == 0)

total_sample = cbind(Total_N_ELSA, mean_age_ELSA, sd_age_ELSA, N_women_ELSA,
                     Total_N_HRS, mean_age_HRS, sd_age_HRS, N_women_HRS)

total_sample

write.csv(total_sample, file = paste(OUTPUT_ROOT, "total_sample_characteristics.csv", sep=""))

unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_discrim_lessrespect)
unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_discrim_harassed)
unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_discrim_poorerservice)
unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_discrim_notclever)
unique(HRS2010_discrimination_dataset_before_subsetting$HRS2010_discrim_medical)

#source the file that runs nested logistic regression and also calcualtes the percentage the resitricted sample makes up of the of the total sample 
model_adjusted = c('adjusted')
model_unadjusted = c('unadjusted')


Unadjusted_results = data.frame()
Adjusted_results = data.frame()


source(paste(SOURCE_ROOT, "Unadjusted/disability/Discrimination_cross_nat_disability_restricted_unadjusted.R", sep=""))
Disability_unadjusted_restricted_results = Disability_unadjusted_restricted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                            data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model = model_unadjusted
discrimination_type =c('Disability')
Disability_unadjusted_restricted_results = data.frame(discrimination_type, model, Disability_unadjusted_restricted_results) 


Unadjusted_results = rbind(Unadjusted_results, Disability_unadjusted_restricted_results)

source(paste(SOURCE_ROOT, "Adjusted_sig_cov/disability/Discrimination_cross_nat_disability_restricted.R", sep=""))
Disability_adjusted_restricted_results = Disability_adjusted_restricted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                        data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model2 = model_adjusted
discrimination_type2 =c('Disability')
Disability_adjusted_restricted_results  = data.frame(discrimination_type2,
                                                     model2,  
                                                     Disability_adjusted_restricted_results)  

Adjusted_results = rbind(Adjusted_results, Disability_adjusted_restricted_results)


##########################
##########################
source(paste(SOURCE_ROOT, "Unadjusted/financial/less_than_median_wealth/financial_discrimination_median_split_LessThanMedian_ALL_unadjusted.R", sep=""))
Financial_unadjusted_lowWealth_ALL_results = Financial_unadjusted_lowWealth_ALL(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model3 = model_unadjusted
discrimination_type3 =c('Financial LOW SES, F & M')
Financial_unadjusted_lowWealth_ALL_results = data.frame(discrimination_type3,
                                                        model3,
                                                        Financial_unadjusted_lowWealth_ALL_results)

Unadjusted_results = rbind(Unadjusted_results, Financial_unadjusted_lowWealth_ALL_results)


source(paste(SOURCE_ROOT, "Adjusted_sig_cov/financial/less_than_median_wealth/financial_discriination_median_split_lessthna_median_sigCov_excluding_wealth.R", sep=""))
Financial_adjusted_lowWealth_ALL_results = Financial_adjusted_lowWealth_ALL(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model3_adjusted = model_adjusted
discrimination_type3_adjusted =c('Financial LOW SES, F & M')
Financial_adjusted_lowWealth_ALL_results = data.frame(discrimination_type3_adjusted,
                                                      model3_adjusted,
                                                      Financial_adjusted_lowWealth_ALL_results)

Adjusted_results = rbind(Adjusted_results, Financial_adjusted_lowWealth_ALL_results)

##########################
##########################

source(paste(SOURCE_ROOT, "Unadjusted/financial/less_than_median_wealth/Financial_disc_lessThan_medianWealth_unadjusted_female.R", sep=""))
Financial_unadjusted_lowWealth_female_results  = Financial_unadjusted_lowWealth_female(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model4= model_unadjusted
discrimination_type4 =c('Financial LOW SES, F')
Financial_unadjusted_lowWealth_female_results = data.frame(discrimination_type4,
                                                        model4,
                                                        Financial_unadjusted_lowWealth_female_results)


Unadjusted_results = rbind(Unadjusted_results, Financial_unadjusted_lowWealth_female_results)



source(paste(SOURCE_ROOT,"Adjusted_sig_cov/financial/less_than_median_wealth/Financial_disc_lessThan_medianWealth_female.R" , sep=""))
Financial_adjusted_lessThan_medianWealth_female_results = Financial_adjusted_lessThan_medianWealth_female(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model5 = model_adjusted   
discrimination_type5=c('Financial LOW SES, F')
Financial_adjusted_lessThan_medianWealth_female_results = data.frame(model5, 
                                                                     discrimination_type5,
                                                                     Financial_adjusted_lessThan_medianWealth_female_results)

Adjusted_results = rbind(Adjusted_results, Financial_adjusted_lessThan_medianWealth_female_results)



##########################
##########################
source(paste(SOURCE_ROOT, "Unadjusted/financial/less_than_median_wealth/FinancialDisc_LessThanMedianWealth_male_unadjusted.R", sep=""))

Financial_unadjusted_lowWealth_male_results  = Financial_unadjusted_lowWealth_male(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model6 = model_unadjusted
discrimination_type6 =c('Financial LOW SES, M')
Financial_unadjusted_lowWealth_male_results = data.frame(discrimination_type6,
                                                        model6,
                                                        Financial_unadjusted_lowWealth_male_results)


Unadjusted_results = rbind(Unadjusted_results, Financial_unadjusted_lowWealth_male_results)



source(paste(SOURCE_ROOT, "Adjusted_sig_cov/financial/less_than_median_wealth/financial_discrim_lessthan_median_sigCov_male", sep=""))

Financial_adjusted_lowWealth_male_results  = Financial_adjusted_lowWealth_male(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model6_adjusted = model_adjusted
discrimination_type6_adjusted =c('Financial LOW SES, M')
Financial_unadjusted_lowWealth_male_results = data.frame(discrimination_type6_adjusted,
                                                         model6_adjusted,
                                                         Financial_adjusted_lowWealth_male_results)

Adjusted_results = rbind(Adjusted_results, Financial_unadjusted_lowWealth_male_results)


##########################
##########################
##########################
##########################

source(paste(SOURCE_ROOT,"Unadjusted/financial/more_than_median_wealth/Financial_disc_morethan_median_ALL_unadjusted.R", sep=""))
Financial_disc_unadjusted_morethan_median_ALL_results = Financial_disc_unadjusted_morethan_median_ALL(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                       data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model7 = model_unadjusted
discrimination_type7 =  c("financial high SES, M & F")

Financial_disc_unadjusted_morethan_median_ALL_results = data.frame(model7,
                                                                   discrimination_type7,
                                                                   Financial_disc_unadjusted_morethan_median_ALL_results)


Unadjusted_results = rbind(Unadjusted_results, Financial_disc_unadjusted_morethan_median_ALL_results)



source(paste(SOURCE_ROOT,"Adjusted_sig_cov/financial/more_than_median_wealth/Financial_disc_morethanMedian_All_adjusted.R" , sep=""))
Financial_disc_adjusted_morethan_median_ALL_results =Financial_disc_adjusted_morethan_median_ALL(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model8 = model_adjusted
discrimination_type8 =  c("financial high SES, M & F")


Financial_disc_adjusted_morethan_median_ALL_results =data.frame(model8, 
                                                                discrimination_type8, 
                                                                Financial_disc_adjusted_morethan_median_ALL_results)


Adjusted_results = rbind(Adjusted_results, Financial_disc_adjusted_morethan_median_ALL_results)

##########################
##########################

source(paste(SOURCE_ROOT,"Unadjusted/financial/more_than_median_wealth/Financial_disc_median_split_morethanMedianwealth_male_unadjusted.R", sep=""))
Financial_disc_adjusted_morethan_median_male_results =  Financial_disc_adjusted_morethan_median_male(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                     data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model9 = model_unadjusted

discrimination_type9 =  c("financial high SES, M")

Financial_disc_unadjusted_morethan_median_male_results = data.frame(model9,
                                                                 discrimination_type9, 
                                                                 Financial_disc_unadjusted_morethan_median_male_results)

Unadjusted_results = rbind(Unadjusted_results, Financial_disc_unadjusted_morethan_median_male_results)

source(paste(SOURCE_ROOT,"Adjusted_sig_cov/financial/more_than_median_wealth/Financial_disc_median_split_moreThanMedian_male.R" , sep=""))

Financial_adjusted_moreThanMedian_male_results = Financial_disc_median_split_moreThanMedian_male(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model10 = model_adjusted
discrimination_type10 =  c("financial high SES, M")


Financial_adjusted_moreThanMedian_male_results = data.frame(model10, discrimination_type10,
                                                            Financial_adjusted_moreThanMedian_male_results)


Adjusted_results = rbind(Adjusted_results, Financial_adjusted_moreThanMedian_male_results)

##########################
##########################
source(paste(SOURCE_ROOT,"Unadjusted/financial/more_than_median_wealth/financial_disc_MoreThan_medianwealth_female_unadjusted.R", sep=""))
Financial_unadjusted_moreThanMedian_female_results = Financial_unadjusted_moreThanMedian_female(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)


model11 = model_unadjusted
discrimination_type11 =  c("financial high SES, F")

Financial_unadjusted_moreThanMedian_female_results =data.frame(model11, 
                                                               discrimination_type11, 
                                                               Financial_unadjusted_moreThanMedian_female_results)


Unadjusted_results = rbind(Unadjusted_results, Financial_unadjusted_moreThanMedian_female_results)


source(paste(SOURCE_ROOT,"Adjusted_sig_cov/financial/more_than_median_wealth/Financial_disc_MoreThan_medianwealth_female.R" , sep=""))
Financial_adjusted_moreThanMedian_female_results = Financial_adjusted_moreThanMedian_female(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting)
                                                                                                
model12 = model_adjusted
discrimination_type12 =  c("financial high SES, F")

Financial_adjusted_moreThanMedian_female_results = data.frame(model12,
                                                                discrimination_type12,
                                                                Financial_adjusted_moreThanMedian_female_results)

Adjusted_results = rbind(Adjusted_results, Financial_adjusted_moreThanMedian_female_results)

##########################
##########################

source(paste(SOURCE_ROOT, "Unadjusted/gender/Discrimination_cross_nat_gender_restrcited_female_unadjusted.R", sep=""))

gender_restrcited_female_unadjusted_results = gender_restrcited_female_unadjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                   data_HRS = HRS2010_discrimination_dataset_before_subsetting)
                                

model13 = model_unadjusted
discrimination_type13 =  c("gender, F")

gender_restrcited_female_unadjusted_results = data.frame(model13,
                                                         discrimination_type13, 
                                                         gender_restrcited_female_unadjusted_results) 

Unadjusted_results = rbind(Unadjusted_results, gender_restrcited_female_unadjusted_results)


source(paste(SOURCE_ROOT,"Adjusted_sig_cov/gender/Discrimination_cross_nat_gender_restricted_sig_cov.R" , sep=""))
gender_restrcited_female_adjusted_results = gender_restrcited_female_adjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                  data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model14 = model_adjusted
discrimination_type14 =  c("gender, F")

gender_restrcited_female_adjusted_results = data.frame(model14,
                                                         discrimination_type14, 
                                                         gender_restrcited_female_adjusted_results) 

Adjusted_results = rbind(Adjusted_results, gender_restrcited_female_adjusted_results)

##########################
##########################
source(paste(SOURCE_ROOT,"Unadjusted/gender/Discrimination_cross_nat_gender_restrcited_unadjusted_male.R" , sep=""))
gender_restrcited_unadjusted_male_results = gender_restrcited_unadjusted_male(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                              data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model15 = model_unadjusted
discrimination_model15 =  c("gender, M")


gender_restrcited_unadjusted_male_results = data.frame(model15, 
                                                       discrimination_model15, 
                                                       gender_restrcited_unadjusted_male_results)

Unadjusted_results = rbind(Unadjusted_results, gender_restrcited_unadjusted_male_results)


source(paste(SOURCE_ROOT, "Adjusted_sig_cov/gender/Discrimination_cross_nat_male_restricted_adjusted.R", sep=""))

gender_male_adjusted_results = gender_male_adjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                    data_HRS = HRS2010_discrimination_dataset_before_subsetting)

model16 = model_adjusted
discrimination_model16 =  c("gender, M")

gender_male_adjusted_results = data.frame(model16, 
                                          discrimination_model16,
                                          gender_male_adjusted_results)


Adjusted_results = rbind(Adjusted_results, gender_male_adjusted_results)

##########################
##########################

source(paste(SOURCE_ROOT, "Unadjusted/sexuality/DIscrimination_cross_nat_sexuality.R", sep=""))

DIscrimination_cross_nat_sexuality_results = DIscrimination_cross_nat_sexuality(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                 data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model17 = model_unadjusted
discrimination_model17 =  c("sexuality")

DIscrimination_cross_nat_sexuality_results = data.frame(model17, 
                                                        discrimination_model17, 
                                                        DIscrimination_cross_nat_sexuality_results)

Unadjusted_results = rbind(Unadjusted_results, DIscrimination_cross_nat_sexuality_results)


# no significant covariates to adjust for sexuality discrimination 
DIscrimination_cross_nat_sexuality_NO_ADJUSTED_results =  rep("NA", 18)

Adjusted_results = rbind(Adjusted_results, DIscrimination_cross_nat_sexuality_NO_ADJUSTED_results)


##########################
##########################
source(paste(SOURCE_ROOT,"Unadjusted/race/Discrimination_cross_nat_race_unadjusted.R" , sep=""))

Unadjusted_race_results = Unadjusted_race(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model18 = model_unadjusted
discrimination_model18 =  c("race")

Unadjusted_race_results = data.frame(model18, 
                                     discrimination_model18, 
                                     Unadjusted_race_results)

Unadjusted_results = rbind(Unadjusted_results, Unadjusted_race_results)



source(paste(SOURCE_ROOT,"Adjusted_sig_cov/race/Discrimination_cross_nat_race_restricted_sigcov_model_new.R" , sep=""))

Adjusted_race_results = Adjusted_race(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                          data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model19 = model_adjusted
discrimination_model19 =  c("race")

Adjusted_race_results = data.frame(model19, 
                                     discrimination_model19, 
                                   Adjusted_race_results)

Adjusted_results = rbind(Adjusted_results, Adjusted_race_results)


##########################
##########################
source(paste(SOURCE_ROOT,"Unadjusted/weight/Cross_national_diff_weight_OBESE_unadjusted.R" , sep=""))

weight_OBESE_unadjusted_results = weight_OBESE_unadjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model20 = model_unadjusted
discrimination_model20 =  c("weight, BMI>29.9")

weight_OBESE_unadjusted_results = data.frame(model20, 
                                             discrimination_model20, 
                                             weight_OBESE_unadjusted_results)

Unadjusted_results = rbind(Unadjusted_results, weight_OBESE_unadjusted_results)



source(paste(SOURCE_ROOT,"Adjusted_sig_cov/weight/Cross_national_diff_weight_restricted_sig_cov_model_obese_only.R" , sep=""))

weight_adjusted_obese_results = weight_adjusted_obese(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model21 = model_adjusted
discrimination_model21 =  c("weight, BMI>29.9")
weight_adjusted_obese_results = data.frame(model21, 
                                   discrimination_model21, 
                                   weight_adjusted_obese_results)
 
Adjusted_results = rbind(Adjusted_results, weight_adjusted_obese_results)


##########################
##########################
source(paste(SOURCE_ROOT,"Unadjusted/weight/Cross_national_diff_weight_obeseANDoverweight_unadjusted.R" , sep=""))



weight_obeseANDoverweight_unadjusted_results = weight_obeseANDoverweight_unadjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model22 = model_unadjusted
discrimination_model22 =  c("weight, BMI > 25.0")

weight_obeseANDoverweight_unadjusted_results = data.frame(model22, 
                                                          discrimination_model22, 
                                                          weight_obeseANDoverweight_unadjusted_results)


Unadjusted_results = rbind(Unadjusted_results, weight_obeseANDoverweight_unadjusted_results)




source(paste(SOURCE_ROOT,"Adjusted_sig_cov/weight/Discrimination_cross_nat_diff_weight_restricted_obese_andOverweight.R" , sep=""))

weight_obeseANDoverweight_adjusted_results = weight_obeseANDoverweight_adjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model23 = model_adjusted
discrimination_model23 =  c("weight, BMI > 25.0")

weight_obeseANDoverweight_adjusted_results = data.frame(model23, 
                                                          discrimination_model23, 
                                                          weight_obeseANDoverweight_adjusted_results)

Adjusted_results = rbind(Adjusted_results, weight_obeseANDoverweight_adjusted_results)


##########################
##########################
source(paste(SOURCE_ROOT,"Unadjusted/weight/Discrimination_cross_nat_overweight_unadjusted.R" , sep=""))

overweight_unadjusted_results = overweight_unadjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                                                data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model24 = model_unadjusted
discrimination_model24 =  c("weight, BMI > 25, =<29.9")

overweight_unadjusted_results = data.frame(model24, 
                                           discrimination_model24, 
                                           overweight_unadjusted_results)



Unadjusted_results = rbind(Unadjusted_results, overweight_unadjusted_results)




source(paste(SOURCE_ROOT,"Adjusted_sig_cov/weight/Discrimination_cross_nat_weight_overweight_sigCov_model_.R" , sep=""))


overweight_adjusted_results = overweight_adjusted(data_ELSA = ELSAdiscrimination_data_wave5_before_subsetting,
                                                      data_HRS = HRS2010_discrimination_dataset_before_subsetting)
model25 = model_adjusted
discrimination_model25 =  c("weight, BMI > 25, =<29.9")

overweight_adjusted_results = data.frame(model25, 
                                         discrimination_model25, 
                                         overweight_adjusted_results)


Adjusted_results = rbind(Adjusted_results, overweight_adjusted_results)

##########################


Unadjusted_results
print(Unadjusted_results)

Adjusted_results
print(Adjusted_results)


Cross_nat_diff_adjusted_unadjusted_results = cbind(Unadjusted_results, Adjusted_results)

Cross_nat_diff_adjusted_unadjusted_results


write.csv(Cross_nat_diff_adjusted_unadjusted_results, file = paste(OUTPUT_ROOT, "Cross_nat_diff_adjusted_unadjusted_results.csv", sep=""))


