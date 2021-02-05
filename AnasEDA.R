
library(ggplot2)
library(viridis)
data(Fatalities)
head(Fatalities)
str(Fatalities)
dim(Fatalities)

numNA = function(label){
  l1 =  length(which(is.na(label)))
  return(l1)
}
sapply(Fatalities, FUN = numNA)
which(is.na(Fatalities$service))#28


Fatalities = Fatalities[-c(28),]

library(ggplot2)
ggplot(Fatalities)+
  geom_point(aes(x= spirits ,y = afatal), position = "dodge")+
  theme_bw()

#### raw fit with AIC and BIC
full.model = lm(fatal~.,data = Fatalities)
empty.model = lm(fatal ~ 1, data = Fatalities)
n=nrow(Fatalities)

forward.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                            k = log(n),trace=FALSE,direction = "forward")
backward.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                             k = log(n),trace=FALSE,direction = "backward")

FB.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = log(n),trace=FALSE,direction = "both")

BF.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = log(n),trace=FALSE,direction = "both")


forward.model.BIC$coefficients #income, baptist, jail, spirits, gsp, miles, mormon
backward.model.BIC$coefficients # spirits income baptist, mormon, miles jail gsp
FB.model.BIC$coefficients
BF.model.BIC$coefficients


# model with afatal_pop = afatal/pop, rate per 100k
Fatalities$afatalPop = (Fatalities$afatal/Fatalities$pop)*100000

#w/o any fatalities or population
full.model = lm(afatalPop ~ spirits + unemp + income + emppop + beertax +baptist+
                  mormon + drinkage + dry +  youngdrivers+ miles + breath + jail+service
                +milestot+unempus+emppopus+gsp, data = Fatalities)
empty.model = lm(afatalPop ~ 1, data = Fatalities)
n=nrow(Fatalities)
#BICs
library(MASS)
forward.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                            k = log(n),trace=FALSE,direction = "forward")
backward.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                             k = log(n),trace=FALSE,direction = "backward")

FB.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = log(n),trace=FALSE,direction = "both")

BF.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = log(n),trace=FALSE,direction = "both")



forward.model.BIC$coefficients #income, baptist, jail, spirits, gsp, miles, mormon
backward.model.BIC$coefficients # spirits income baptist, mormon, miles jail gsp
FB.model.BIC$coefficients
BF.model.BIC$coefficients
#using BIC criterion all have the same variables: income, baptist, jail, spirits, gsp, miles, mormon

#AIC 
forward.model.AIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                            k = 2,trace=FALSE,direction = "forward")
backward.model.AIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                             k = 2,trace=FALSE,direction = "backward")

FB.model.AIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = 2,trace=FALSE,direction = "both")

BF.model.AIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = 2,trace=FALSE,direction = "both")

forward.model.AIC$coefficients 
backward.model.AIC$coefficients
FB.model.AIC$coefficients
BF.model.AIC$coefficients
# using AIC all models have the following variables:
# income, baptist, jail, spirits, gsp, miles, mormon, breath, unemp, youngdrivers, beertax

##################################################################################
# model with night fatalities pop = afatal/pop, rate per 100k
Fatalities$nfatalPop = (Fatalities$nfatal/Fatalities$pop)*100000

#w/o any fatalities or population
full.model = lm(nfatalPop ~ spirits + unemp + income + emppop + beertax +baptist+
                  mormon + drinkage + dry +  youngdrivers+ miles + breath + jail+service
                +milestot+unempus+emppopus+gsp, data = Fatalities)
empty.model = lm(nfatalPop ~ 1, data = Fatalities)

forward.model.BIC_night = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                            k = log(n),trace=FALSE,direction = "forward")
backward.model.BIC_night = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                             k = log(n),trace=FALSE,direction = "backward")

FB.model.BIC_night = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = log(n),trace=FALSE,direction = "both")

BF.model.BIC_night = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = log(n),trace=FALSE,direction = "both")

forward.model.BIC_night$coefficients #income, baptist, jail, spirits, gsp, miles, mormon
backward.model.BIC_night$coefficients # spirits income baptist, mormon, miles jail gsp
FB.model.BIC_night$coefficients
BF.model.BIC_night$coefficients
#using BIC criterion all have the same variables: income, baptist, jail, spirits, gsp, miles, mormon

#AIC 
forward.model.AIC_night = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                            k = 2,trace=FALSE,direction = "forward")
backward.model.AIC_night = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                             k = 2,trace=FALSE,direction = "backward")

FB.model.AIC_night = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = 2,trace=FALSE,direction = "both")

BF.model.AIC_night = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                       k = 2,trace=FALSE,direction = "both")

forward.model.AIC_night$coefficients 
backward.model.AIC_night$coefficients
FB.model.AIC_night$coefficients
BF.model.AIC_night$coefficients
# using AIC all models have the following variables:
# income, baptist, jail, spirits, gsp, miles, mormon, breath, unemp, youngdrivers, beertax



