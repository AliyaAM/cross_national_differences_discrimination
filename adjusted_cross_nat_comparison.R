adjusted_cross_nat_comparison = function (data_ELSA, 
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

  #predictor dummy varibale: country (UK vs USA)
  country_cat = c(data_ELSA_subset$country, 
                  data_HRS_subset$country)
  
  country_cat = as.factor(country_cat)
  
  data_both_countries = data.frame(country_cat)
  #outcome concatinated into a new dataframe pooling ELSA and HRS (make sure the order as above)
  
  data_both_countries$discrimination = c(data_ELSA_subset[ , discrimination_VAR_elsa],
                                         data_HRS_subset[ , discrimination_VAR_hrs] )
  
  data_both_countries$wealth = c(data_ELSA_subset$wealth,
                                 data_HRS_subset$wealth)
  
  
  # if then rule for a number of covariates, if the covariates are NA then a different glm model is passed 

  # when only covariate 1 is included (i.e, not NA, !=NA)  and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 == "NA" &  covariate3 == "NA" &  covariate4 == "NA" ){
    
    print("we are in test 1")
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                              data_HRS_subset[ ,   covariate1])
    
  
  fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1], 
             
             data = data_both_countries)
  
  fm2 <- glm(discrimination ~ country_cat 
             + data_both_countries[ ,   covariate1] , 

             data = data_both_countries)
  
  #no empty cells in the contingency table
  xtabs(~discrimination + country_cat, data = data_both_countries)
  
  #
  data_both_countries$country_cat <- factor(data_both_countries$country_cat)
  mylogit <- fm2
  
  summary(mylogit)
  
  
  Number_cases = nrow(data_both_countries)
  
  if (Number_cases) 
    
    if((Number_cases %% 2) == 0) {
      print(paste(Number_cases,"is Even"))
      data_both_countries = data_both_countries 
      
    } else {
      print(paste(Number_cases,"is Odd"))
      data_both_countries = data_both_countries[-c(1),]
      
    }
  
  Number_cases = nrow(data_both_countries)
  print("new data frme")
  print(Number_cases)
  
  Number_cases_level = Number_cases/2 
  
  #below plot: from min wealth in the dataset to max wealth in the dataset, 
  newdata2 <- with(data_both_countries, data.frame(wealth = rep(seq(from = -10000, to = 400000, length.out = Number_cases_level),
                                                                2), 
                                                   #gpa = mean(gpa), 
                                                   country_cat = factor(rep(0:1, each = Number_cases_level))))
  print("test 1")
  
  newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                      se = TRUE))
  newdata3 <- within(newdata3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
  })
  
  ## view first few rows of final dataset
  head(newdata3)
  
  plot = ggplot(newdata3, aes(x = wealth, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                                ymax = UL, fill = country_cat), alpha = 0.02) + geom_line(aes(colour = country_cat),
                                                                                                                                     size = 1) + ggtitle(analysis_variable_name) + scale_fill_discrete(name = "Country", labels = c("United States", "United Kingdom"))
  
  print(plot)
  }
  
  # when  covariate 1 and covariate 2  (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 == "NA" &  covariate4 == "NA"){
    
    print("we are in test 2, test two covars")
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                              data_HRS_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2],
                                              data_HRS_subset[ ,   covariate2])
  
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2] , 
               
               data = data_both_countries)
    #no empty cells in the contingency table
    xtabs(~discrimination + country_cat, data = data_both_countries)
    
    #
    data_both_countries$country_cat <- factor(data_both_countries$country_cat)
    mylogit <- fm2
    
    summary(mylogit)
    
    Number_cases = nrow(data_both_countries)
    
    if (Number_cases) 
      
      if((Number_cases %% 2) == 0) {
        print(paste(Number_cases,"is Even"))
        data_both_countries = data_both_countries 
        
      } else {
        print(paste(Number_cases,"is Odd"))
        data_both_countries = data_both_countries[-c(1),]
        
      }
    
    Number_cases = nrow(data_both_countries)
    print("new data frme")
    print(Number_cases)
    
    Number_cases_level = Number_cases/2 
    #below plot: from min wealth in the dataset to max wealth in the dataset, 
    newdata2 <- with(data_both_countries, data.frame(wealth = rep(seq(from = -10000, to = 400000, length.out = Number_cases_level),
                                                                  2), 
                                                     #gpa = mean(gpa), 
                                                     country_cat = factor(rep(0:1, each = Number_cases_level))))
    
    print("test 2")
    
    
    
    newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                        se = TRUE))
    newdata3 <- within(newdata3, {
      PredictedProb <- plogis(fit)
      LL <- plogis(fit - (1.96 * se.fit))
      UL <- plogis(fit + (1.96 * se.fit))
    })
    
    ## view first few rows of final dataset
    head(newdata3)
    
    plot = ggplot(newdata3, aes(x = wealth, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                                  ymax = UL, fill = country_cat), alpha = 0.02) + geom_line(aes(colour = country_cat),
                                                                                                                                       size = 1) + ggtitle(analysis_variable_name) + scale_fill_discrete(name = "Country", labels = c("United States", "United Kingdom"))
    
    print(plot)
  }
  
  # when  covariate 1 and covariate 2 and covariate 3 (i.e, not NA, !=NA) are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 == "NA"){
    
    
    print("we are in test 3")
    
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                              data_HRS_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2],
                                              data_HRS_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(data_ELSA_subset[ ,   covariate3], 
                                             data_HRS_subset[ ,   covariate3])
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3], 
               
               data = data_both_countries)
    #no empty cells in the contingency table
    xtabs(~discrimination + country_cat, data = data_both_countries)
    
    #
    data_both_countries$country_cat <- factor(data_both_countries$country_cat)
    mylogit <- fm2
    
    summary(mylogit)
    
    
    Number_cases = nrow(data_both_countries)
    
    if (Number_cases) 
      
      if((Number_cases %% 2) == 0) {
        print(paste(Number_cases,"is Even"))
        data_both_countries = data_both_countries 
        
      } else {
        print(paste(Number_cases,"is Odd"))
        data_both_countries = data_both_countries[-c(1),]
        
      }
    
    Number_cases = nrow(data_both_countries)
    print("new data frme")
    print(Number_cases)
    
    Number_cases_level = Number_cases/2 
    
    #below plot: from min wealth in the dataset to max wealth in the dataset, 
    newdata2 <- with(data_both_countries, data.frame(wealth = rep(seq(from = -10000, to = 400000, length.out = Number_cases_level),
                                                                  2), 
                                                     #gpa = mean(gpa), 
                                                     country_cat = factor(rep(0:1, each = Number_cases_level))))
    
    print("test 3")
    
    
    newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                        se = TRUE))
    newdata3 <- within(newdata3, {
      PredictedProb <- plogis(fit)
      LL <- plogis(fit - (1.96 * se.fit))
      UL <- plogis(fit + (1.96 * se.fit))
    })
    
    ## view first few rows of final dataset
    head(newdata3)
    
    plot = ggplot(newdata3, aes(x = wealth, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                                  ymax = UL, fill = country_cat), alpha = 0.02) + geom_line(aes(colour = country_cat),
                                                                                                                                       size = 1) + ggtitle(analysis_variable_name) + scale_fill_discrete(name = "Country", labels = c("United States", "United Kingdom"))
    
    print(plot)
    
  } 
  
  # when  covariate 1 and covariate 2 and covariate 3 and covariate 4 (i.e, not NA, !=NA)  are included and the rest are NA then take the glm in the if statement below 
  
  if(covariate1 != "NA" & covariate2 != "NA" & covariate3 != "NA" & covariate4 != "NA"){
    print("we are in, test 4")
    data_both_countries[ ,   covariate1] = c(data_ELSA_subset[ ,   covariate1],
                                             data_HRS_subset[ ,   covariate1])
    
    
    data_both_countries[ ,   covariate2] = c(data_ELSA_subset[ ,   covariate2],
                                             data_HRS_subset[ ,   covariate2])
    
    data_both_countries[ ,   covariate3]= c(data_ELSA_subset[ ,   covariate3], 
                                            data_HRS_subset[ ,   covariate3])
    
    data_both_countries[ ,   covariate4]= c(data_ELSA_subset[ ,   covariate4], 
                                            data_HRS_subset[ ,   covariate4])
    
    
    fm1 <- glm(discrimination ~  data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
    
    fm2 <- glm(discrimination ~ country_cat 
               + data_both_countries[ ,   covariate1]
               + data_both_countries[ ,   covariate2]
               + data_both_countries[ ,   covariate3]
               + data_both_countries[ ,   covariate4], 
               
               data = data_both_countries)
   
  }
  

  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_UK = cross_country_OR[2, 1]
  CI1_UK = cross_country_OR[2, 2]
  CI2_UK = cross_country_OR[2, 3]
  
  
  cross_country_OR = exp(cbind(OR = coef(fm2), confint(fm2)))
  cross_country_OR_USA = cross_country_OR[1, 1]
  CI1_USA = cross_country_OR[1, 2]
  CI2_USA = cross_country_OR[1, 3]
  
  ## various equivalent specifications of the LR test
  cross_national_diff = lrtest(fm1, fm2)
  
  chi_value_cross_national = cross_national_diff$stats[1]
  pvalue_cross_national = cross_national_diff$stats[3]
  
  
  
  cross_national_findings = cbind(analysis_variable_name, 
                                  N_ELSA_subset, 
                                  N_HRS_subset, 
                                  
                                  N_ELSA_discrimYES, 
                                  N_HRS_discrimYES, 
                                
                                             
                                  cross_country_OR_UK, 
                                  CI1_UK, 
                                  CI2_UK, 
                                             
                                  cross_country_OR_USA, 
                                  CI1_USA, 
                                  CI2_USA,
                                  
                                  chi_value_cross_national,
                                  pvalue_cross_national)
                                             
                                  #ELSA_OR_value,
                                  #ELSA_CI1,
                                  #ELSA_CI2,
                                  #HRS_OR_value,
                                  #HRS_CI1,
                                  #HRS_CI2)
  
  
  return(cross_national_findings)
}