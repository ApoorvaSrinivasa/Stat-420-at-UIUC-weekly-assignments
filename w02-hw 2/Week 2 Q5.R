For this exercise we will use the Ozone dataset from the mlbench package. You should use ?Ozone to learn about the background of this dataset. You may need to install the mlbench package. If you do so, do not include code to install the package in your R Markdown document.

For simplicity, we will perform some data cleaning before proceeding.

data(Ozone, package = "mlbench")
Ozone = Ozone[, c(4, 6, 7, 8)]
colnames(Ozone) = c("ozone", "wind", "humidity", "temp")
Ozone = Ozone[complete.cases(Ozone), ]
We have:
  
  Loaded the data from the package
Subset the data to relevant variables
This is not really necessary (or perhaps a good idea) but it makes the next step easier
Given variables useful names
Removed any observation with missing values
This should be given much more thought in practice
For this exercise we will define the “Root Mean Square Error” of a model as

RMSE=1n∑i=1n(yi−ŷ i)2‾‾‾‾‾‾‾‾‾‾‾‾‾‾⎷.

(a) Fit three SLR models, each with “ozone” as the response. For the predictor, use “wind speed,” “humidity percentage,” and “temperature” respectively. For each, calculate RMSE and R2. Arrange the results in a markdown table, with a row for each model. Suggestion: Create a data frame that stores the results, then investigate the kable() function from the knitr package.

(b) Based on the results, which of the three predictors used is most helpful for predicting wins? Briefly explain.


install.packages("mlbench")

data(Ozone, package = "mlbench")
Ozone = Ozone[, c(4, 6, 7, 8)]
colnames(Ozone) = c("ozone", "wind", "humidity", "temp")
Ozone = Ozone[complete.cases(Ozone), ]

slr1 <-  lm(ozone~wind, data = Ozone)
slr_2  <-  lm(ozone~humidity, data = Ozone)
slr3 <-  lm(ozone~temp, data = Ozone)

pred1 <- predict(slr1)
e1 <- Ozone$ozone - pred1
rmse1 <- sqrt(mean(e1^2))
rsquared1 <- summary(slr1)$r.squared

pred2 <- predict(slr2)
e2 <- Ozone$ozone - pred2
rmse2 <- sqrt(mean(e2^2))
rsquared2 <- summary(slr2)$r.squared
rsquared2

pred3 <- predict(slr3)
e3 <- Ozone$ozone - pred3
rmse3 <- sqrt(mean(e3^2))
rsquared3 <- summary(slr3)$r.squared
rsquared3

RMSE <- c(rmse1,rmse2,rmse3)
rsquared <- c(rsquared1,rsquared2,rsquared3)

final<- data.frame(RMSE,rsquared)
rownames(final) <- c("wind", "humidity", "temp")



predictors <-  data.frame()
colnames(predictors) <- c("Wind","humidity","temp")
for (i in predictors)
{
  assign(paste("blah",i, sep="_"), predict(i))
  }



slr3 <-  lm(ozone~temp, data = Ozone)

pred1 <- predict(slr1)
e1 <- Ozone$ozone - pred1
rmse1 <- sqrt(mean(e1^2))
rsquared1 <- summary(slr1)$r.squared
wind <- c(rmse1,rsquared1)


df <- data.frame()
for ( i in c("wind","temp"))
{
  df[i] <-  lm(ozone~temp, data = Ozone)
}
















d<-3
for (i in 1:10){ assign(paste("A",i, sep=""), (d + rnorm(3))) }


