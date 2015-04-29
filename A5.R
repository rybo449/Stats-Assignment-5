getwd()
setwd("/Users/MSAUSI2014/Desktop/Assignment 5")
getwd()
art<-long$V1
fem<-long$V2
mar<-long$V3
kid5<-long$V4
phd<-long$V5
ment<-long$V6
##Including all the covariates in the glm command and reporting the summary
fit<-glm(art~fem+mar+kid5+phd+ment, family = poisson(link = "log"))
summary(fit)

##Interpreting the results of the model in terms of the Risk Ratio
rr_fem<-exp(coef(fit)["fem"])
rr_mar<-exp(coef(fit)["mar"])
rr_kid5<-exp(coef(fit)["kid5"])
rr_phd<-exp(coef(fit)["phd"])
rr_ment<-exp(coef(fit)["ment"])
rr_fem
##the fem has only 79% of the influence on the # of articles produced by a PhD in the last 3 years
rr_mar
##Risk ratio between the number of articles in the last 3 years and if the candidate is married is 1.167
rr_kid5
##the number of articles produced by a PhD with kids is 83% if there are no kids
rr_phd
##there is no change in the number of articles produced and the prestige of the school
rr_ment
##there is no change in influence between the number of articles produced by the mentor and the PhD

##Determining the confidence interval
exp(confint.default(fit)["fem",])
exp(confint.default(fit)["mar",])
exp(confint.default(fit)["kid5",])
exp(confint.default(fit)["phd",])
exp(confint.default(fit)["ment",])


##Fit the model using the Fisher scoring algorithm
ff<-art~fem+mar+kid5+phd+ment
ols<-lm(ff)
ols
model.matrix(ols)


##Fisher Function

Fisher.it = function(Y,X, pi0, niter=1, print=F) { 
  pi = pi0
   for (i in 1:niter) {
     W = pi*(1-pi)
     Z = log(pi/(1-pi)) + (Y - pi)/(pi*(1-pi)) 
     lmobj = lm(Z ~ X - 1, weights=W)
     beta = lmobj$coef
     eta = X %*% beta
     pi = exp(eta)/(1 + exp(eta)) 
     if (print) {
       print(paste("Iteration ", as.character(i), ": Betahat"))
       print(beta)}
   }
   XWX = t(lmobj$R) %*% lmobj$R
return(beta, XWX, pi, W) }
ff<-~fem+mar+kid5+phd+ment
out = Fisher.it(ols, ff, 0.75, 1)
