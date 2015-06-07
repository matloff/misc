
library(minpack.lm)
library(sandwich)

# uses output of nlsLM() of the minpack.lm package to get an asymptotic 
# covariance matrix without assuming homoscedasticity

# arguments:
# 
#    nlslmout: return value from nlsLM()
# 
# value: approximate covariance matrix for the estimated
#        parameter vector

nlsvcovhc <- function(nlslmout) {
   # notation: g(t,b) is the regression model, where x is the vector of
   # variables for a given observation; b is the estimated parameter
   # vector; x is the matrix of predictor values
   b <- coef(nlslmout)
   m <- nlslmout$m
   # y - g:
   resid <- m$resid()
   # row i of hmat will be deriv of g(x[i,],b) with respect to b
   hmat <- m$gradient()
   # calculate the artificial "x" and "y" of the algorithm
   fakex <- hmat
   fakey <- resid + hmat %*% b
   # -1 means no constant term in the model
   lmout <- lm(fakey ~ fakex - 1)
   vcovHC(lmout)
}

