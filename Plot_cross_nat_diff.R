#https://stats.oarc.ucla.edu/r/dae/logit-regression/

library(ggplot2)


mydata = country_comparison

#no empty cells in the contingency table
xtabs(~discrim_disability + country, data = mydata)

#
mydata$country <- factor(mydata$country)
mylogit <- glm(discrim_disability ~ wealth  + country, data = mydata, family = "binomial")

summary(mylogit)

newdata1 <- with(mydata, data.frame(wealth = mean(wealth),
                                    #gpa = mean(gpa), 
                                    country = factor(0:1)))

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

#below plot: from min wealth in the dataset to max wealth in the dataset, 
newdata2 <- with(mydata, data.frame(wealth = rep(seq(from = -10000, to = 400000, length.out = 100),
                                              2), 
                                    #gpa = mean(gpa), 
                                    country = factor(rep(0:1, each = 1000))))


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
                                                                    ymax = UL, fill = country), alpha = 0.2) + geom_line(aes(colour = country),
                                                                                                                      size = 1)
