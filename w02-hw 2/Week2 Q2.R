## Exercise 2 (Writing Functions)

# This exercise is a continuation of Exercise 1.
# 
# **(a)** Write a function called `get_sd_est` that calculates an estimate of $\sigma$ in one of two ways depending on input to the function. The function should take three arguments as input:
#   
#   - `fitted_vals` - A vector of fitted values from a model
# - `actual_vals` - A vector of the true values of the response
# - `mle` - A logical (`TRUE` / `FALSE`) variable which defaults to `FALSE`
# 
# The function should return a single value:
#   
#   - $s_e$ if `mle` is set to `FALSE`.
# - $\hat{\sigma}$ if `mle` is set to `TRUE`.
# 
# **(b)** Run the function `get_sd_est` on the residuals from the model in Exercise 1, with `mle` set to `FALSE`. Explain the resulting estimate in the context of the model.
# 
# **(c)** Run the function `get_sd_est` on the residuals from the model in Exercise 1, with `mle` set to `TRUE`. Explain the resulting estimate in the context of the model. Note that we are trying to estimate the same parameter as in part **(b)**.
# 
# **(d)** To check your work, output `summary(cat_model)$sigma`. It should match at least one of **(b)** or **(c)**.


y <- cats$Hwt
yhat <- summary(cat_model)$coefficients[1,1] + (summary(cat_model)$coefficients[2,1]*cats$Bwt)
e <- y- yhat
len <- length(e)
  
get_sd_est <- function(fitted_vals=yhat, actual_vals=y, mle = FALSE)
{
  
  se <-  sqrt (sum(e^2)/(len-2))
  sigmahat <- sqrt (sum(e^2)/(len))
  ifelse (mle  == FALSE, se ,sigmahat)

}


get_sd_est(yhat,y,FALSE)

get_sd_est(yhat,y,TRUE)

summary(cat_model)$sigma

