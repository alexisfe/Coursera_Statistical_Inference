# In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. 
# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. 
# The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. 
# Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. 
# Note that you will need to do a thousand simulations.
# 
# Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials. You should
# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# 3. Show that the distribution is approximately normal.
# 
# In point 3, focus on the difference between the distribution of a large collection of random exponentials and the distribution of a large collection of averages of 40 exponentials. 

# As a motivating example, compare the distribution of 1000 random uniforms
# hist(runif(1000))
# and the distribution of 1000 averages of 40 random uniforms
# 
# mns = NULL
# for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
# hist(mns)
# This distribution looks far more Gaussian than the original uniform distribution!

set.seed(1234)
lambda = .2
num_sim = 1e3
num_var = 40

hist(rexp(num_sim*num_var, lambda))
mns = NULL
for (i in 1 : num_sim) mns = c(mns, mean(rexp(num_var, lambda)))
hist(mns)

# 1. Show the sample mean and compare it to the theoretical mean of the distribution.

#Theoretical mean:
tmean <- 1/lambda
#Sample mean: 
smean <- mean(mns)

# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

#Theoretical var:
tvar <- 1/(num_var*lambda^2)
#Sample var:
svar <- var(mns)

# 3. Show that the distribution is approximately normal.
library(ggplot2)
mns <- data.frame(mns)

ggplot(mns, aes(x = mns)) + 
  geom_histogram(alpha = .10, binwidth=0.1, colour = "black", aes(y = ..density..)) +
  stat_function(geom = "line", fun = dnorm, arg = list(mean = tmean, sd = sqrt(tvar)),
                size = 2, colour = "red")

norm_vars <- rnorm(num_var, mean = smean, sd = sqrt(svar))

qqnorm(mns);qqline(norm_vars, col=2)

