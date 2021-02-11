library(ggplot2)
library(viridis)
data(Fatalities)
head(Fatalities)
str(Fatalities)
dim(Fatalities)

Fatalities$afatal_rate <- Fatalities$afatal / Fatalities$pop * 10000
## different us regions
south = c("la","ms","al","ga","sc","ar","tx","fl","nc","tn","ar","ok","ky","wv","va")
n_east =  c("me","vt","nh","ma","ny","ri","ct","nj","de","md","pa")

Fatalities$location[Fatalities$state %in% south]  =  "south"
Fatalities$location[Fatalities$state %in% n_east] =  "north east"
Fatalities$location[!(Fatalities$state %in% n_east | Fatalities$state %in% south)]   =  "other"
str(Fatalities$location)
Fatalities$location = as.factor(Fatalities$location)
levels(Fatalities$location)




# get.data = function(y,the.data){
#   the.data <<- subset(Fatalities, year == y)
#   
#   droplevels(the.data$year)
#   numNAs = sapply(the.data, function(x){length(which(is.na(x)))})
#   
#   the.data$afatal_rate <- the.data$afatal / d$pop * 10000
#   the.data$afatal = NULL
# }
# get.data("1982",)

fatality.1982 = subset(Fatalities, year == "1982")
fatality.1983 = subset(Fatalities, year == "1983")
fatality.1984 = subset(Fatalities, year == "1984")
fatality.1985 = subset(Fatalities, year == "1985")
fatality.1986 = subset(Fatalities, year == "1986")
fatality.1987 = subset(Fatalities, year == "1987")
fatality.1988 = subset(Fatalities, year == "1988")

#year 1988 has some na values
which(is.na(fatality.1988$jail)) #observation 4 n in subset data, obs 28 in original data
fatality.1988[4,] #since it is categorical we cannot impute the data so we remove. 
#after running below line, comment out the line to avoid deleting more pts. 
#fatality.1988 = fatality.1988[-c(which(is.na(fatality.1988$jail))),]
View(fatality.1988)
which(is.na(fatality.1988$jail))

get.best.model = function(data){
  
  droplevels(data$year)
  numNAs = sapply(data, function(x){length(which(is.na(x)))})
  
  #data$afatal_rate <- data$afatal / data$pop * 10000
  full.model = lm(afatal_rate ~ spirits + unemp + income + emppop + beertax +baptist+
                  mormon + drinkage + dry +  youngdrivers+ miles + breath + jail+service
                +milestot+unempus+emppopus+gsp, data = data)

  empty.model = lm(afatal_rate ~ 1, data = data)

  n=nrow(data)

  forward.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                              k = log(n),trace=FALSE,direction = "forward")
  backward.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                               k = log(n),trace=FALSE,direction = "backward")
  
  FB.model.BIC = stepAIC(empty.model,  scope = list(lower = empty.model, upper= full.model), 
                         k = log(n),trace=FALSE,direction = "both")
  
  BF.model.BIC = stepAIC(full.model,  scope = list(lower = empty.model, upper= full.model), 
                         k = log(n),trace=FALSE,direction = "both")
  
  forwards = forward.model.BIC$call
  b = backward.model.BIC$call
  fb = FB.model.BIC$call
  bf = BF.model.BIC$call
  results=  list("na" = numNAs, "F" = forwards, "B" = b,"FB" = fb, "BF" = bf)
  return(results)
}

get.best.model(fatality.1982)$FB
model.1982 = lm(formula = afatal_rate ~ miles + dry + jail, data =fatality.1982)
round(model.1982$coefficients, 4)

get.best.model(fatality.1983)$FB
model.1983 = lm(formula = afatal_rate ~ miles + income + drinkage + breath + 
                  gsp, data = fatality.1983)
round(model.1983$coefficients, 5)

get.best.model(fatality.1984)$FB
model.1984 = lm(formula = afatal_rate ~ income + gsp + baptist, data = fatality.1984)
round(model.1984$coefficients, 5)


get.best.model(fatality.1985)$FB
model.1985 = lm(formula = afatal_rate ~ income + miles + jail + mormon, data = fatality.1985)
round(model.1985$coefficients, 4)

get.best.model(fatality.1986)$FB
model.1986 = lm(formula = afatal_rate ~ income + baptist + jail, data = fatality.1986)
round(model.1986$coefficients,5)

get.best.model(fatality.1987)$FB
model.1987 = lm(formula = afatal_rate ~ income + baptist, data = fatality.1987)
round(model.1987$coefficients,4)

get.best.model(fatality.1988)$FB
model.1988 = lm(formula = afatal_rate ~ income + baptist + spirits, data = fatality.1988)
round(model.1988$coefficients, 4)



