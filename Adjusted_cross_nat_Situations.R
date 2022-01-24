library(epitools)

library(foreign)
library(rms) # Used to extract p-value from logistic model
library(ggplot2) #plots 
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
library(dplyr)



Adjusted_cross_nat_Situations = function (data_ELSA, 
                                            data_HRS,
                                            
                                            analysis_variable_name, 
                                            
                                            subsetting_VAR1_ELSA, 
                                            subsetting_VAR1_HRS,
                                            
                                            subsetting_VAR2_ELSA, 
                                            subsetting_VAR2_HRS,
                                            
                                            ELSA_var1_value,
                                            HRS_var1_value,
                                            
                                            ELSA_var2_value,
                                            HRS_var2_value,
                                            
                                            covariate1, 
                                            covariate2,
                                            covariate3, 
                                            covariate4, 
                                            
                                            
                                            
                                            discrimination_VAR_elsa,
                                            discrimination_VAR_hrs){
  
  #list the subsetting var name inside the function 
  
  analysis_variable_name = analysis_variable_name
  
  
  #data_HRS <- data_HRS[ , subsetting_VAR_HRS]
  #data_ELSA <- data_ELSA[ , subsetting_VAR_ELSA]
  
  
  # subsetting data to the right variable for the analysis (eg, sex, physical lim.)
  # if there is only one subsetting var: subsetting_VAR1_ELSA and subsetting_VAR1_HRS
  if (subsetting_VAR1_ELSA == "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS == "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = data_ELSA 
    data_HRS_subset = data_HRS
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA =="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS == "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value)
    data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR1_HRS] == HRS_var1_value)
  } 
  
  if (subsetting_VAR1_ELSA != "NA" & subsetting_VAR2_ELSA !="NA" & subsetting_VAR1_HRS != "NA" & subsetting_VAR2_HRS != "NA"){
    
    data_ELSA_subset = subset(data_ELSA, data_ELSA[ , subsetting_VAR1_ELSA] == ELSA_var1_value & data_ELSA[subsetting_VAR2_ELSA] == ELSA_var2_value)
    data_HRS_subset = subset(data_HRS, data_HRS[ , subsetting_VAR1_HRS] == HRS_var1_value & data_HRS[ ,subsetting_VAR2_HRS] == HRS_var2_value)
  }
  
  # calculate the number of cases for this subset 
  N_ELSA_subset = nrow(data_ELSA_subset)
  N_HRS_subset = nrow(data_HRS_subset)
  
  
  #calculate the number of people who perceived this type of discrimination 
  
  ELSA_discrimYES_subset = subset(data_ELSA_subset, data_ELSA_subset[ , discrimination_VAR_elsa] == 1) 
  HRS_discrimYES_subset = subset(data_HRS_subset,  data_HRS_subset[ , discrimination_VAR_hrs] == 1)
  
  N_ELSA_discrimYES = nrow(ELSA_discrimYES_subset)
  N_HRS_discrimYES = nrow(HRS_discrimYES_subset)
  
  
  country_cat = c(ELSA_discrimYES_subset$country,
                  HRS_discrimYES_subset$country)
  
  
  discrim_lessrespect = c(ELSA_discrimYES_subset$w5discrim_lessrespect_bin2,
                          HRS_discrimYES_subset$HRS2010_discrim_lessrespect_bin)
  
  
  #Received poorer service or treatment than other people from doctors or hospitals
  unique(ELSA_discrimYES_subset$w5discrim_medical_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_medical_bin)
  
  discrim_medical = c(ELSA_discrimYES_subset$w5discrim_medical_bin2, 
                      HRS_discrimYES_subset$HRS2010_discrim_medical_bin)
  
  #People act as if they think you are not clever or smart
  unique(ELSA_discrimYES_subset$w5discrim_notclever_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_notclever_bin)
  
  discrim_notclever = c(ELSA_discrimYES_subset$w5discrim_notclever_bin2, 
                        HRS_discrimYES_subset$HRS2010_discrim_notclever_bin)
  
  #Received poorer service than others in a restaurant or shop
  unique(ELSA_discrimYES_subset$w5discrim_poorerservice_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_poorerservice_bin)
  
  discrim_poor_service = c(ELSA_discrimYES_subset$w5discrim_poorerservice_bin2,
                           HRS_discrimYES_subset$HRS2010_discrim_poorerservice_bin)
  
  #You are threatened or harassed
  unique(ELSA_discrimYES_subset$w5discrim_harassed_bin2)
  unique(HRS_discrimYES_subset$HRS2010_discrim_harassed_bin)
  
  discrim_harrased = c(ELSA_discrimYES_subset$w5discrim_harassed_bin2, 
                       HRS_discrimYES_subset$HRS2010_discrim_harassed_bin)
  
  
  data_both_countries <- data.frame(country_cat, 
                                            
                                    
                                            
                                            discrim_lessrespect, 
                                            discrim_medical,
                                            discrim_notclever, 
                                            discrim_poor_service, 
                                            discrim_harrased)
  
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 
  
  # when only covariate 1 is included (i.e, not NA, !=NA)  and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    
    data_both_countries[ ,   covariate1] = c(ELSA_discrimYES_subset[ ,   covariate1],
                                             HRS_discrimYES_subset[ ,   covariate1])
    
    
    fm1discrim_lessrespect <- glm(discrim_lessrespect ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2discrim_lessrespect <- glm(discrim_lessrespect ~ country_cat 
               + data_both_countries[ ,   covariate1] , 
               
               data = data_both_countries)
    
    
    fm1discrim_medical <- glm(discrim_medical ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2discrim_medical <- glm(discrim_medical ~ country_cat 
               + data_both_countries[ ,   covariate1] , 
               
               data = data_both_countries)
    
    fm1discrim_notclever <- glm(discrim_notclever ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2discrim_notclever <- glm(discrim_notclever ~ country_cat 
               + data_both_countries[ ,   covariate1] , 
               
               data = data_both_countries)
    fm1discrim_poor_service <- glm(discrim_poor_service ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2discrim_poor_service <- glm(discrim_poor_service ~ country_cat 
               + data_both_countries[ ,   covariate1] , 
               
               data = data_both_countries)
    fm1discrim_harrased <- glm(discrim_harrased ~  data_both_countries[ ,   covariate1], 
               
               data = data_both_countries)
    
    fm2discrim_harrased <- glm(discrim_harrased ~ country_cat 
               + data_both_countries[ ,   covariate1] , 
               
               data = data_both_countries)
    

  }
  
  # when  covariate 1 and covariate 2  (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 == "NA" &  covariate4 == "NA"){
    
    
    data_both_countries[ ,   covariate1] = c(ELSA_discrimYES_subset[ ,   covariate1],
                                             HRS_discrimYES_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(ELSA_discrimYES_subset[ ,   covariate2],
                                             HRS_discrimYES_subset[ ,   covariate2])

    
    fm1discrim_lessrespect <- glm(discrim_lessrespect ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2], 
               
               data = data_both_countries)
    
    fm2discrim_lessrespect <- glm(discrim_lessrespect ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2] , 
               
               data = data_both_countries)
    
    
    fm1discrim_medical <- glm(discrim_medical ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_medical <- glm(discrim_medical ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2] , 
                                  
                                  data = data_both_countries)
    fm1discrim_notclever <- glm(discrim_notclever ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_notclever <- glm(discrim_notclever ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2] , 
                                  
                                  data = data_both_countries)
    fm1discrim_poor_service <- glm(discrim_poor_service ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_poor_service <- glm(discrim_poor_service ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2] , 
                                  
                                  data = data_both_countries)
    fm1discrim_harrased <- glm(discrim_harrased ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_harrased <- glm(discrim_harrased ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2] , 
                                  
                                  data = data_both_countries)
  }
  
  # when  covariate 1 and covariate 2 and covariate 3 (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 == "NA"){
    
    data_both_countries[ ,   covariate1] = c(ELSA_discrimYES_subset[ ,   covariate1],
                                             HRS_discrimYES_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(ELSA_discrimYES_subset[ ,   covariate2],
                                             HRS_discrimYES_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(ELSA_discrimYES_subset[ ,   covariate3], 
                                            HRS_discrimYES_subset[ ,   covariate3])
    
    

    
    fm1discrim_lessrespect <- glm(discrim_lessrespect ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3], 
               
               data = data_both_countries)
    
    fm2discrim_lessrespect <- glm(discrim_lessrespect ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3], 
               
               data = data_both_countries)
    
    
    fm1discrim_medical<- glm(discrim_medical ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_medical <- glm(discrim_medical ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    
    
    
    fm1discrim_notclever <- glm(discrim_notclever ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_notclever <- glm(discrim_notclever ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    fm1discrim_poor_service <- glm(discrim_poor_service ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_poor_service <- glm(discrim_poor_service ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    fm1discrim_harrased <- glm(discrim_harrased ~  data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
    
    fm2discrim_harrased <- glm(discrim_harrased ~ country_cat 
                                  + data_both_countries[ ,   covariate1]
                                  + data_both_countries[ ,   covariate2]
                                  + data_both_countries[ ,   covariate3], 
                                  
                                  data = data_both_countries)
  } 
  
  # when  covariate 1 and covariate 2 and covariate 3 and covariate 4 (i.e, not NA, !=NA)  are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 != "NA"){
    data_both_countries[ ,   covariate1] = c(ELSA_discrimYES_subset[ ,   covariate1],
                                             HRS_discrimYES_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(ELSA_discrimYES_subset[ ,   covariate2],
                                             HRS_discrimYES_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(ELSA_discrimYES_subset[ ,   covariate3], 
                                            HRS_discrimYES_subset[ ,   covariate3])
    
    data_both_countries[ ,   covariate4]= c(ELSA_discrimYES_subset[ ,   covariate4], 
                                            HRS_discrimYES_subset[ ,   covariate4])
  
    
    
    fm1discrim_lessrespect <- glm(discrim_lessrespect ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2discrim_lessrespect <- glm(discrim_lessrespect ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    

    
    fm1discrim_medical <- glm(discrim_medical ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2discrim_medical <- glm(discrim_medical ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    
    
    fm1discrim_notclever <- glm(discrim_notclever ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2discrim_notclever <- glm(discrim_notclever ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    
    
    
    discrim_harrased
    
    fm1discrim_poor_service <- glm(discrim_poor_service ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2discrim_poor_service <- glm(discrim_poor_service ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    
    fm1discrim_harrased <- glm(discrim_harrased ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2discrim_harrased <- glm(discrim_harrased ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    
    
  }
  

  
  ##########################################################
discrim_lessrespectcross_country_OR = exp(cbind(OR = coef(fm2discrim_lessrespect), confint(fm2discrim_lessrespect)))

discrim_lessrespectcross_country_OR_UK = discrim_lessrespectcross_country_OR[2, 1]
discrim_lessrespectCI1_UK = discrim_lessrespectcross_country_OR[2, 2]
discrim_lessrespectCI2_UK = discrim_lessrespectcross_country_OR[2, 3]
  
  
discrim_lessrespectcross_country_OR = exp(cbind(OR = coef(fm2discrim_lessrespect), confint(fm2discrim_lessrespect)))

discrim_lessrespectcross_country_OR_USA = discrim_lessrespectcross_country_OR[1, 1]
discrim_lessrespectCI1_USA = discrim_lessrespectcross_country_OR[1, 2]
discrim_lessrespectCI2_USA = discrim_lessrespectcross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
discrim_lessrespectcross_national_diff = lrtest(fm1discrim_lessrespect, fm2discrim_lessrespect)
  
discrim_lessrespectchi_value_cross_national = discrim_lessrespectcross_national_diff$stats[1]
discrim_lessrespectpvalue_cross_national = discrim_lessrespectcross_national_diff$stats[3]
  
findings_discrim_lessrespect = cbind(discrim_lessrespectcross_country_OR_UK, 
                                     discrim_lessrespectCI1_UK,
                                     discrim_lessrespectCI2_UK,
                                     discrim_lessrespectcross_country_OR_USA,
                                     discrim_lessrespectCI1_USA,
                                     discrim_lessrespectCI2_USA,
                                     discrim_lessrespectchi_value_cross_national,
                                     discrim_lessrespectpvalue_cross_national)
  

  ##########################################################


discrim_medicalcross_country_OR = exp(cbind(OR = coef(fm2discrim_medical), confint(fm2discrim_medical)))
discrim_medicalcross_country_OR_UK = discrim_medicalcross_country_OR[2, 1]
discrim_medicalCI1_UK = discrim_medicalcross_country_OR[2, 2]
discrim_medicalCI2_UK = discrim_medicalcross_country_OR[2, 3]
  
  
discrim_medicalcross_country_OR = exp(cbind(OR = coef(fm2discrim_medical), confint(fm2discrim_medical)))
discrim_medicalcross_country_OR_USA = discrim_medicalcross_country_OR[1, 1]
discrim_medicalCI1_USA = discrim_medicalcross_country_OR[1, 2]
discrim_medicalCI2_USA = discrim_medicalcross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
discrim_medicalcross_national_diff = lrtest(fm1discrim_medical, fm2discrim_medical)
  
discrim_medicalchi_value_cross_national = discrim_medicalcross_national_diff$stats[1]
discrim_medicalpvalue_cross_national = discrim_medicalcross_national_diff$stats[3]

findings_discrim_medical = cbind(discrim_medicalcross_country_OR_UK, 
                                 discrim_medicalCI1_UK,
                                 discrim_medicalCI2_UK,
                                 discrim_medicalcross_country_OR_USA,
                                 discrim_medicalCI1_USA,
                                 discrim_medicalCI2_USA,
                                 discrim_medicalchi_value_cross_national,
                                 discrim_medicalpvalue_cross_national)

  ##########################################################

  
discrim_notclevercross_country_OR = exp(cbind(OR = coef(fm2discrim_notclever), confint(fm2discrim_notclever)))
discrim_notclevercross_country_OR_UK = discrim_notclevercross_country_OR[2, 1]
discrim_notcleverCI1_UK = discrim_notclevercross_country_OR[2, 2]
discrim_notcleverCI2_UK = discrim_notclevercross_country_OR[2, 3]
  
  
discrim_notclevercross_country_OR = exp(cbind(OR = coef(fm2discrim_notclever), confint(fm2discrim_notclever)))
discrim_notclevercross_country_OR_USA = discrim_notclevercross_country_OR[1, 1]
discrim_notcleverCI1_USA = discrim_notclevercross_country_OR[1, 2]
discrim_notcleverCI2_USA = discrim_notclevercross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
discrim_notclevercross_national_diff = lrtest(fm1discrim_notclever, fm2discrim_notclever)
  
discrim_notcleverchi_value_cross_national = discrim_notclevercross_national_diff$stats[1]
discrim_notcleverpvalue_cross_national = discrim_notclevercross_national_diff$stats[3]

findings_discrim_notclever = cbind(discrim_notclevercross_country_OR_UK, 
                                 discrim_notcleverCI1_UK,
                                 discrim_notcleverCI2_UK,
                                 discrim_notclevercross_country_OR_USA,
                                 discrim_notcleverCI1_USA,
                                 discrim_notcleverCI2_USA,
                                 discrim_notcleverchi_value_cross_national,
                                 discrim_notcleverpvalue_cross_national)

  ##########################################################
  
  
discrim_poor_servicecross_country_OR = exp(cbind(OR = coef(fm2discrim_poor_service), confint(fm2discrim_poor_service)))
discrim_poor_servicecross_country_OR_UK = discrim_poor_servicecross_country_OR[2, 1]
discrim_poor_serviceCI1_UK = discrim_poor_servicecross_country_OR[2, 2]
discrim_poor_serviceCI2_UK = discrim_poor_servicecross_country_OR[2, 3]
  
  
discrim_poor_servicecross_country_OR = exp(cbind(OR = coef(fm2discrim_poor_service), confint(fm2discrim_poor_service)))
discrim_poor_servicecross_country_OR_USA = discrim_poor_servicecross_country_OR[1, 1]
discrim_poor_serviceCI1_USA = discrim_poor_servicecross_country_OR[1, 2]
discrim_poor_serviceCI2_USA = discrim_poor_servicecross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
discrim_poor_servicecross_national_diff = lrtest(fm1discrim_poor_service, fm2discrim_poor_service)
  
discrim_poor_servicechi_value_cross_national = discrim_poor_servicecross_national_diff$stats[1]
discrim_poor_servicepvalue_cross_national = discrim_poor_servicecross_national_diff$stats[3]

findings_discrim_poor_service = cbind(discrim_poor_servicecross_country_OR_UK, 
                                      discrim_poor_serviceCI1_UK,
                                      discrim_poor_serviceCI2_UK,
                                      discrim_poor_servicecross_country_OR_USA,
                                      discrim_poor_serviceCI1_USA,
                                      discrim_poor_serviceCI2_USA,
                                      discrim_poor_servicechi_value_cross_national,
                                      discrim_poor_servicepvalue_cross_national)

  
  ##########################################################
  
discrim_harrasedcross_country_OR = exp(cbind(OR = coef(fm2discrim_harrased), confint(fm2discrim_harrased)))
discrim_harrasedcross_country_OR_UK = discrim_harrasedcross_country_OR[2, 1]
discrim_harrasedCI1_UK = discrim_harrasedcross_country_OR[2, 2]
discrim_harrasedCI2_UK = discrim_harrasedcross_country_OR[2, 3]
  
  
discrim_harrasedcross_country_OR = exp(cbind(OR = coef(fm2discrim_harrased), confint(fm2discrim_harrased)))
discrim_harrasedcross_country_OR_USA = discrim_harrasedcross_country_OR[1, 1]
discrim_harrasedCI1_USA = discrim_harrasedcross_country_OR[1, 2]
discrim_harrasedCI2_USA = discrim_harrasedcross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
discrim_harrasedcross_national_diff = lrtest(fm1discrim_harrased, fm2discrim_harrased)
  
discrim_harrasedchi_value_cross_national = discrim_harrasedcross_national_diff$stats[1]
discrim_harrasedpvalue_cross_national = discrim_harrasedcross_national_diff$stats[3]

findings_discrim_harrased  = cbind(discrim_harrasedcross_country_OR_UK, 
                                   discrim_harrasedCI1_UK,
                                   discrim_harrasedCI2_UK,
                                   discrim_harrasedcross_country_OR_USA,
                                   discrim_harrasedCI1_USA,
                                   discrim_harrasedCI2_USA,
                                   discrim_harrasedchi_value_cross_national,
                                   discrim_harrasedpvalue_cross_national)

rownames_situations = c("discrim_lessrespect",
                        "discrim_medical",
                        "discrim_notclever",
                        "discrim_poor_service",
                        "discrim_harrased")

findings1 = rbind(findings_discrim_lessrespect, 
                  findings_discrim_medical,
                  findings_discrim_notclever,
                  findings_discrim_poor_service, 
                  findings_discrim_harrased)



result = data.frame(analysis_variable_name, rownames_situations, 
                      findings1, stringsAsFactors = TRUE)


return(result)
}


