## Exercise 1 (Using `lm`)

# For this exercise we will use the `cats` dataset from the `MASS` package. You should use `?cats` to learn about the background of this dataset.
# 
# **(a)** Suppose we would like to understand the size of a cat's heart based on the body weight of a cat. Fit a simple linear model in `R` that accomplishes this task. Store the results in a variable called `cat_model`. Output the result of calling `summary()` on `cat_model`.
# 
# **(b)** Output only the estimated regression coefficients. Interpret $\hat{\beta_0}$ and $\beta_1$ in the *context of the problem*. Be aware that only one of those is an estimate.
# 
# **(c)** Use your model to predict the heart weight of a cat that weights **2.7** kg. Do you feel confident in this prediction? Briefly explain.
# 
# **(d)** Use your model to predict the heart weight of a cat that weights **4.4** kg. Do you feel confident in this prediction? Briefly explain.
# 
# **(e)** Create a scatterplot of the data and add the fitted regression line. Make sure your plot is well labeled and is somewhat visually appealing.
# 
# **(f)** Report the value of $R^2$ for the model. Do so directly. Do not simply copy and paste the value from the full output in the console after running `summary()` in part **(a)**.#


cats <- MASS :: cats
cat_model <- lm(Hwt ~ Bwt, data= cats)
summary(cat_model)


cat_model$coefficients
beta_0_hat <- cat_model$coefficients[1]
beta_1_hat <- cat_model$coefficients[2]

# OR 

# beta_0_hat <- summary(cat_model)$coefficients[1,1]
# beta_1_hat <- summary(cat_model)$coefficients[2,1]


pred1 <- summary(cat_model)$coefficients[1,1] +(summary(cat_model)$coefficients[2,1]*2.7)
pred1

**This prediction is more reliable as the prediction is an intrapolation of the data values already available. 

pred2 <- summary(cat_model)$coefficients[1,1] +(summary(cat_model)$coefficients[2,1]*4.4)
pred2

**This prediction is less reliable as we are prdicting the value of heart rate as a result of extraploation from the 
data we already have.Thus we cannot be very sure of the prediction.


plot(Hwt ~ Bwt, data = cats,
     xlab = "Body weight(in kgs)",
     ylab = "Heart weight(in kgs)",
     main = "Body weight Vs Heart weight of cats",
     pch  = 20,
     cex  = 1,
     col  = "blue")
abline(cat_model, lwd = 3, col = "darkorange")

rsquared <- summary(cat_model)$r.squared
rsquared
