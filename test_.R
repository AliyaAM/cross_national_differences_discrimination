
library(emmeans)

vart_test = rbinom(100, 1, 0.5)
var1_test = rbinom(100, 1, 0.8)
 

contengency_table  = table(vart_test, var1_test)

print(contengency_table)

test_vart_test_AND_var1_test = chisq.test(contengency_table)

summary(test_vart_test_AND_var1_test)

OR_vart_test = oddsratio.wald(contengency_table)

OR_vart_testvalue = OR_vart_test$measure

OR_vart_test$measure

N_ELSA_discrim_YES = 40

ELSA_subset = 100 

N_ELSA_discrim_NO = 60 


probability_disc_ELSA = N_ELSA_discrim_YES/ELSA_subset 
probability_no_disc_ELSA = 1 - (probability_disc_ELSA) 

Odds_yes = probability_disc_ELSA/(1-probability_disc_ELSA)

Odds_no = probability_no_disc_ELSA/(1-probability_no_disc_ELSA)

Odds_ratio = Odds_yes/Odds_no

CI_lower = exp(log(Odds_ratio) - 1.96 * sqrt(1/N_ELSA_discrim_YES + 1/N_ELSA_discrim_NO))

CI_upper = exp(log(Odds_ratio) + 1.96 * sqrt(1/N_ELSA_discrim_YES + 1/N_ELSA_discrim_NO))



confint(Odds_ratio)
