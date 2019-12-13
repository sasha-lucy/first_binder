library(Hmisc)
library(ggplot2)
library(tidyverse)
library(car)
library(olsrr)
library(leaps)

library(readxl)

REMEDI <- read_excel("~/Desktop/CTQ ACC Project/REMEDI.xlsx", 
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric"))

View(REMEDI)


#Correlation 1- Total CTQ

rcorr(REMEDI$Total, REMEDI$GMVACC)

#Pearson's R= 0.55, P = 0.001 This is a "moderate correlation"

REMEDI %>%
  ggplot(aes( x = Total, y = GMVACC)) +
  geom_point() +
  geom_smooth(method = "lm")




#Correlation 2- Emotional Neglect

rcorr(REMEDI$EmotNEG, REMEDI$GMVACC)

#Pearson's R = 0.47, P = 0.0069 This is a "moderate correlation"

REMEDI %>%
  ggplot(aes( x = EmotNEG, y = GMVACC)) +
  geom_point() +
  geom_smooth(method = "lm")




#Correlation 3- Emotional Abuse

rcorr(REMEDI$EmotABU, REMEDI$GMVACC)

#Pearson's R = 0.49, P = 0.0049 This is a "moderate correlation"

REMEDI %>%
  ggplot(aes( x = EmotABU, y = GMVACC)) +
  geom_point() +
  geom_smooth(method = "lm")



#Correlation 4- Physical Neglect

rcorr(REMEDI$PhysNEG, REMEDI$GMVACC)

#Pearson's R = 0.48, P = 0.0053 This is a "moderate correlation"

REMEDI %>%
  ggplot(aes( x = PhysNEG, y = GMVACC)) +
  geom_point() +
  geom_smooth(method = "lm")



#Correlation 5- Physical Abuse

rcorr(REMEDI$PhysABU, REMEDI$GMVACC)

#Pearson's R = 0.4, P = 0.025 This is a "weak/ moderate correlation"

REMEDI %>%
  ggplot(aes( x = PhysNEG, y = GMVACC)) +
  geom_point() +
  geom_smooth(method = "lm")


#Correlation 6- Sexual Abuse

rcorr(REMEDI$SexABU, REMEDI$GMVACC)

#Pearson's R = 0.33, P = 0.069 This is a "weak correlation"

REMEDI %>%
  ggplot(aes( x = PhysNEG, y = GMVACC)) +
  geom_point() +
  geom_smooth(method = "lm")





#Do a model with all of the variables and then drop then one by one to see which is the best fit

model0 <- lm (GMVACC ~ 1, data=REMEDI)
modelall <- lm(GMVACC ~ EmotNEG + EmotABU + PhysNEG + PhysABU + SexABU, data = REMEDI)
anova(model0, modelall)

summary(modelall)
vif(modelall)
#We do not have a collinearity problem as our VIF values are low

#Model with all variables is not significantly different from the model with just the mean

#T-tests suggest that none of the variables seem to predict GMV?

AIC(modelall)

# -163.0944- smaller numbers are better

qqnorm(residuals(modelall))
qqline(residuals(modelall))

plot(modelall)

durbinWatsonTest(modelall)
#D-W value of 1.83 is pretty close to 2 so we conclude that our errors are independent of each other.


#Stepwise regression

steplimitsboth <- step(model0, scope = list (upper = modelall), direction = "both")

summary(steplimitsboth)
#Suggests the best model is with EmotABU and PhysNEG- how to interpret this properly?

AIC(steplimitsboth)

confint(steplimitsboth, level = 95)
#This did not work?

pmodel <- ols_step_forward_p(modelall)
pmodel

#This method suggested the best model is with EmotABU, PhysNEG and SexABU??

leapsmodels <- regsubsets (GMVACC ~ EmotNEG + EmotABU + PhysNEG + PhysABU + SexABU, data = REMEDI)

plot(leapsmodels, scale = "adjr2", main = "Models")
#How to interpret this??

vif(steplimitsboth)
#EmotABu and PhysNEG do not have a collinearity problem












model2 <- lm(GMVACC ~ EmotNEG + EmotABU + PhysNEG + PhysABU, data = REMEDI)
anova(model0, model2)


#Model with sexual abuse dropped is now significant

AIC(model2)

#-163.706



model3 <- lm(GMVACC ~ EmotNEG + EmotABU + PhysNEG, data = REMEDI)
anova(model0, model3)


#Model with sexual abuse and physical abuse dropped is significant

AIC(model3)

#-165.1492



model4 <- lm(GMVACC ~ EmotNEG + EmotABU, data = REMEDI)
anova(model0, model4)

#Model with just emotional neglect and emotional abuse is significant

AIC(model4)

#-166.0371


model5 <- lm(GMVACC ~ EmotNEG, data = REMEDI)
anova(model0, model5)

#Model with just emotional neglect is the most significant??

AIC(model5)

#-166.2829

modeltotal <- lm(GMVACC ~ Total, data = REMEDI)
anova(model0, modeltotal)

#Model with all CTQ subscales added seperately is not significantly different from the mean, but model with total CTQ is??

