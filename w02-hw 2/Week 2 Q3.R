

birthday = 19920720
set.seed(birthday)

num_obs = 25
beta_0  = 5
beta_1  = -3
sigma   = sqrt(10.24)
x = runif(n = 25, 0, 10)

sim_slr = function(x, beta_0 = 5, beta_1 = -3, sigma = 3.2) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}


sim_data = sim_slr(x = x, beta_0 = 5, beta_1 = -3, sigma = 3.2)

ft_data = lm(response ~ predictor, data = sim_data)
coef(ft_data)

# The output of our simulation is almost very accurate with just a small error. 
# True values(known):
#   Beta 1 = -3
#   Beta 0 = -5
# Predicted values:
#   Beta 1 hat = -3.227879
#   Beta 0 hat = 5.007382
# As we can see, the prdiction is very close to the expected values.


plot(response ~ predictor, data = sim_data,
     xlab = "Simulated Predictor Variable",
     ylab = "Simulated Response Variable",
     main = "Simulated Regression Data",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(ft_data, lwd = 3, lty = 1, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")
legend("topright", c("Estimate", "Truth"), lty = c(1, 2), lwd = 2,
       col = c("darkorange", "dodgerblue"))

# sim_slr = function(x, beta_0 = 5, beta_1 = -3, sigma = 3.2) {
#   n = length(x)
#   epsilon = rnorm(n, mean = 0, sd = sigma)
#   y = beta_0 + beta_1 * x + epsilon
#   data.frame(predictor = x, response = y)
# }


beta_hat_1 <- as.vector(rep(0,1500))
for (i in 1:1500)
{
  sim_temp <- sim_slr(x = x, beta_0 = 5, beta_1 = -3, sigma = 3.2)
  slr_temp <-  lm(response ~ predictor,data = sim_temp)
  coef <-  coef(slr_temp)[2]
  beta_hat_1[i] <- coef
  i <- i+1
}

mean_beta_hat1 <- mean(beta_hat_1)
sd_beta_hat1 <- sd(beta_hat_1)

# The mean value of the slope obtained from running multiple combinations(simulations) of the response values is very close to the actual mean value
# considered for the simluation
# Actual model slope considered = -3
# Average predicted slope of the 1500 simulations = -3.002218

histogram_beta_hat1 = hist(beta_hat_1, breaks = 50,
                           main = "Histogram of slope estimate",
                           xlab = " Estimate of slope", col = "grey")
box(col="grey")

# The histogram follows a normal distribution with few odd spikes at some points of the slope.

