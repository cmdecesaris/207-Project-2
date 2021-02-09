
check.normality.assumptions = function(M){
qqnorm(M$residuals)
  #qqline(M$residuals)
  the.SWtest = shapiro.test(M$residuals)
  pValue = the.SWtest$p.value
  if (pValue < (0.01|0.05|0.1)){
    bc = boxcox(M,plotit = FALSE)
    lambda = bc$x[which.max(bc$y)]
    lambda #To see the value of lambda, transform to ln(y)
 
  results = list("p-value" = pValue, "lambda" = lambda)
  return(results)
  }
}

check.normality.assumptions(model.1982)
check.normality.assumptions(model.1983)
check.normality.assumptions(model.1984)
check.normality.assumptions(model.1985)

transformData = function(data){
  data$afatal_rate = log(data$afatal_rate)
  return(data)
}
transf.fatality.1982 = transformData(fatality.1982)
View(transf.fatality.1982)
transfd.model.1982 = lm(formula = afatal_rate ~ miles + dry + jail, data =transf.fatality.1982)

#after running the above models again,
shapiro.test(transfd.model.1982$residuals)

library(ggplot2)
library(lmtest)
#test for constant variance
check.variance = function(M, data){
 # plot(M$fitted.values, model1982$residuals, main="errors vs fitted values", ylab = "errors",
       #xlab = "fitted values")
  #abline(h = 0, col = "red") #seems to have const variance
  
  q= qplot(M$fitted.values,M$residuals , data = data) +  
    ggtitle("Errors vs. Fitted Values") + xlab("Fitted Values") + 
    ylab("Errors") + geom_hline(yintercept = 0,col = "purple")
  
  test = bptest(model1982) #constant variance assumption met
  stuff = list("plot" = q, "pVal" = test)
  return(stuff)
}
check.variance(transfd.model.1982, transf.fatality.1982)


outliers = function(Model, data){
  ri = rstandard(Model)
  alpha = 0.1
  n = nrow(data) 
  p = length(Model$coefficients)
  cutoff = qt(1-alpha/(2*n), n -p )
  outliers = which(abs(ri) > cutoff)
  return(outliers)
}
outliers(transfd.model.1982, transf.fatality.1982)


# #check for interaction terms, look for the model that lowers BIC the most
# model1 = lm(afatal_rate ~ miles + dry + jail + miles*dry, data = fatality1982)
# BIC(model1)
# BIC(model1982)
# 
# model.2 = lm(afatal_rate ~ miles + dry + jail + miles*jail, data = fatality1982)
# BIC(model.2)
# BIC(model1982)
# 
# model.3 = lm(afatal_rate ~ miles + dry + jail + dry*jail, data = fatality1982)
# BIC(model.3)
# BIC(model1982)



