library(fitdistrplus)
set.seed(515)    # Set the seed for the random number generator
B <- 100000         # Number of bootstrap replications
alpha <- 0.05    # Nominal level of the test
# load or simulate your data and store as X
X <- read.csv('/Users/Tjett/OneDrive/Documents/Maastricht/Computational research skill/ScanRecords.csv')
X_1<-X[X$PatientType == 'Type 1',]
X_2<-X[X$PatientType == 'Type 2',]
X_1_duration <- X_1[,'Duration']
X_1_duration
X_2_duration <- X_2[,'Duration']
X_2_duration

#Using bootstrap mainly to find the confidence interval of the mean and to make a way bigger dataset so that the distribution can be fitted.
n <- length(X_1_duration)                                          # Sample size
X.bar <- mean(X_1_duration)                                        # Sample mean of X
St.Dev <- sd(X_1_duration)                                         # Standard deviation of X
Q.n <- sqrt(n)*X.bar/St.Dev 
# Test statistic
X_1_duration.bar.star=0
# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)                            # Initialise vector for bootstrap statistics
St.Dev.stars <- rep(NA, times = B)
bootdur1 <- rep(NA, times = B*n)
for (b in 1:B) {
  J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
  X_1_duration.star <- X_1_duration[J]                                      # Draw the bootstrap sample
  for (i in 1:n) {
    bootdur1[(b-1)*n+i] <- X_1_duration.star[i]
  }
  X_1_duration.bar.star[b] <- mean(X_1_duration.star)                          # Bootstrap sample mean
  St.Dev.star <- sd(X_1_duration.star)                           # Bootstrap standard deviation
  St.Dev.stars[b] <- sd(X_1_duration.star)
  Q.star[b] <- sqrt(n)*(X_1_duration.bar.star[b]-X.bar)/St.Dev.star # Bootstrap statistic
}
hist(X_1_duration.bar.star, freq = FALSE, main = paste0("n = ", n), breaks = 100)
cv <- quantile(Q.star, probs = 1-alpha)                 # Calculate the bootstrap critical value
p.val <- mean(Q.star > Q.n)                             # Calculate the bootstrap p-value
quantile(X_1_duration.bar.star,c(0.025,0.975))*60  #confidence interval for the mean amount of minutes for the duration of a type 1 patient



hist(bootdur1, freq = FALSE, main = paste0("n = ", n), breaks = 10)
#cullen and frey graph to see which distribution to look at
print(descdist(X_1_duration, discrete = FALSE,boot = 10000))
print(descdist(bootdur1, discrete = FALSE))


# Fit a normal distribution to the bootstrapped data duration 1
fit_result1 <- fitdistr(bootdur1, densfun = "normal")

# Plot histogram of the bootstrapped data
hist(bootdur1 , freq = FALSE, col = "lightblue", main = "Fitted Normal Distribution", xlab = "Bootstrap Observed Data", breaks = 10)

# Add the fitted density curve to the plot
curve(dnorm(x, mean = fit_result1$estimate["mean"], sd = fit_result1$estimate["sd"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Bootstrap Observed Data", "Fitted Normal Distribution"), col = c("lightblue", "red"),lty=1, cex = 0.75)

#mean and standard deviation of the normal distribution for the duration of type 1 patients
fit_result1$estimate["mean"]
fit_result1$estimate["sd"]


# Fit a normal distribution to the data duration 1
fit_result2 <- fitdistr(X_1_duration, densfun = "normal")

# Plot histogram of the observed data
hist(X_1_duration , freq = FALSE, col = "lightblue", main = "Fitted Normal Distribution", xlab = "Bootstrap Observed Data", breaks = 10)

# Add the fitted density curve to the plot
curve(dnorm(x, mean = fit_result2$estimate["mean"], sd = fit_result2$estimate["sd"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Observed Data", "Fitted Normal Distribution"), col = c("lightblue", "red"),lty=1, cex = 0.75)


#Find the difference between call times


X_1_diff <- diff(X_1$Time)
X_1_diff[X_1_diff<0]<-X_1_diff[X_1_diff<0]+9 #to account for the time between days
X_1_diff <- X_1_diff[!is.nan(X_1_diff)]
X_1_diff
#print(descdist(X_1_diff, discrete = FALSE,boot = 50000))
#hist(X_1_diff, freq = FALSE, main = paste0("n = ", m), breaks = 20)

#Using bootstrap mainly to find the confidence interval of the mean and to make a way bigger dataset so that the distribution can be fitted.
m <- length(X_1_diff)                                          # Sample size
X.bar <- mean(X_1_diff)                                        # Sample mean of X
St.Dev <- sd(X_1_diff)                                         # Standard deviation of X
Q.n <- sqrt(m)*X.bar/St.Dev 
# Test statistic
X_1_diff.bar.star=0
# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)                            # Initialise vector for bootstrap statistics
St.Dev.stars <- rep(NA, times = B)
bootdiff1 <- rep(NA, times = B*m)
for (b in 1:B) {
  J <- sample.int(m, size = m, replace = TRUE)        # Draw the indices J
  X_1_diff.star <- X_1_diff[J]                                      # Draw the bootstrap sample
  for (i in 1:m) {
    bootdiff1[(b-1)*m+i] <- X_1_diff.star[i]
  }
  X_1_diff.bar.star[b] <- mean(X_1_diff.star)                          # Bootstrap sample mean
  St.Dev.star <- sd(X_1_diff.star)                           # Bootstrap standard deviation
  St.Dev.stars[b] <- sd(X_1_diff.star)
  Q.star[b] <- sqrt(m)*(X_1_diff.bar.star[b]-X.bar)/St.Dev.star # Bootstrap statistic
}
hist(X_1_diff.bar.star, freq = FALSE, main = paste0("n = ", m), breaks = 100)
cv <- quantile(Q.star, probs = 1-alpha)                 # Calculate the bootstrap critical value
p.val <- mean(Q.star > Q.n)    
1/quantile(X_1_diff.bar.star,c(0.025,0.975))  #confidence interval for the rate of the exponential distribution
quantile(X_1_diff.bar.star,c(0.025,0.975))*60  #confidence interval for the mean amount of minutes between two calls

hist(bootdiff1, freq = FALSE, main = paste0("n = ", n), breaks = 10)
#cullen and frey graph to see which distribution to look at
print(descdist(X_1_diff, discrete = FALSE,boot = 10000))
print(descdist(bootdiff1, discrete = FALSE))




# Fit an exponential distribution to the bootstrapped data difference between call times 1
fit_result3 <- fitdistr(bootdiff1, densfun = "exponential")

# Plot histogram of the bootstrapped data
hist(bootdiff1 , freq = FALSE, col = "lightblue", main = "Fitted Exponential Distribution", xlab = "Bootstrap Observed Data")

# Add the fitted density curve to the plot
curve(dexp(x, rate = fit_result3$estimate["rate"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Bootstrap Observed Data", "Fitted Exponential Distribution"), col = c("lightblue", "red"),lty=1, cex = 0.75)


#rate of the exponential distribution for the difference in call times of type 1 patients
fit_result3$estimate["rate"]




# Fit an exponential distribution to the data difference between call times 1
fit_result4 <- fitdistr(X_1_diff, densfun = "exponential")

# Plot histogram of the observed data
hist(X_1_diff , freq = FALSE, col = "lightblue", main = "Fitted Exponential Distribution", xlab = "Observed Data")

# Add the fitted density curve to the plot
curve(dexp(x, rate = fit_result4$estimate["rate"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Observed Data", "Fitted Exponential Distribution"), col = c("lightblue", "red"),lty=1, cex = 0.75)

fit_result4$estimate["rate"]


#Using bootstrap mainly to find the confidence interval of the mean and to make a way bigger dataset so that the distribution can be fitted.
n2 <- length(X_2_duration)                                          # Sample size
X.bar <- mean(X_2_duration)                                        # Sample mean of X
St.Dev <- sd(X_2_duration)                                         # Standard deviation of X
Q.n <- sqrt(n2)*X.bar/St.Dev
# Test statistic
X_2_duration.bar.star=0
# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)                            # Initialise vector for bootstrap statistics
St.Dev.stars <- rep(NA, times = B)
bootdur2 <- rep(NA, times = B*n2)
for (b in 1:B) {
  J <- sample.int(n2, size = n2, replace = TRUE)        # Draw the indices J
  X_2_duration.star <- X_2_duration[J]                                      # Draw the bootstrap sample
  for (i in 1:n2) {
    bootdur2[(b-1)*n2+i] <- X_2_duration.star[i]
  }
  X_2_duration.bar.star[b] <- mean(X_2_duration.star)                          # Bootstrap sample mean
  St.Dev.star <- sd(X_2_duration.star)                           # Bootstrap standard deviation
  St.Dev.stars[b] <- sd(X_2_duration.star)
  Q.star[b] <- sqrt(n2)*(X_2_duration.bar.star[b]-X.bar)/St.Dev.star # Bootstrap statistic
}
hist(X_2_duration.bar.star, freq = FALSE, main = paste0("n = ", n2), breaks = 100)
cv <- quantile(Q.star, probs = 1-alpha)                 # Calculate the bootstrap critical value
p.val <- mean(Q.star > Q.n)                             # Calculate the bootstrap p-value
quantile(X_2_duration.bar.star,c(0.025,0.975))*60  #confidence interval for the mean amount of minutes for the duration of a type 2 patient



hist(bootdur2, freq = FALSE, main = paste0("n = ", n2), breaks = 10)
#cullen and frey graph to see which distribution to look at
print(descdist(X_2_duration, discrete = FALSE,boot = 10000))
print(descdist(bootdur2, discrete = FALSE))


# Fit a gamma distribution to the data
fit_result5 <- fitdistr(X_2_duration, densfun = "gamma")

# Plot histogram of the observed data
hist(X_2_duration , freq = FALSE, col = "lightblue", main = "Fitted Gamma Distribution", xlab = "Observed Data")

# Add the fitted density curve to the plot
curve(dgamma(x, shape = fit_result5$estimate["shape"], rate = fit_result5$estimate["rate"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Observed Data", "Fitted Gamma Distribution"), col = c("lightblue", "red"),lty=1)




# Fit a normal distribution to the data
fit_result6 <- fitdistr(X_2_duration, densfun = "normal")

# Plot histogram of the observed data
hist(X_2_duration , freq = FALSE, col = "lightblue", main = "Fitted Normal Distribution", xlab = "Observed Data")

# Add the fitted density curve to the plot
curve(dnorm(x, mean = fit_result6$estimate["mean"], sd = fit_result6$estimate["sd"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Observed Data", "Fitted Normal Distribution"), col = c("lightblue", "red"),lty=1)





# Fit a gamma distribution to the bootstrap data
fit_result7 <- fitdistr(bootdur2, densfun = "gamma")

# Plot histogram of the bootstrapped data
hist(bootdur2 , freq = FALSE, col = "lightblue", main = "Fitted Gamma Distribution", xlab = "Bootstrap Observed Data", breaks = 10)

# Add the fitted density curve to the plot
curve(dgamma(x, shape = fit_result7$estimate["shape"], rate = fit_result7$estimate["rate"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Bootstrap Observed Data", "Fitted Gamma Distribution"), col = c("lightblue", "red"),lty=1)

#shape and rate of the gamma distribution for the difference in call times of type 1 patients
fit_result7$estimate["shape"]
fit_result7$estimate["rate"]



# Fit a normal distribution to the bootstrap data
fit_result8 <- fitdistr(bootdur2, densfun = "normal")

# Plot histogram of the bootstrapped data
hist(bootdur2 , freq = FALSE, col = "lightblue", main = "Fitted Normal Distribution", xlab = "Bootstrap Observed Data", breaks = 10)

# Add the fitted density curve to the plot
curve(dnorm(x, mean = fit_result8$estimate["mean"], sd = fit_result8$estimate["sd"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Bootstrap Observed Data", "Fitted Normal Distribution"), col = c("lightblue", "red"),lty=1)





#Find the difference between call times
X_2_diff <- diff(X_2$Time)
X_2_diff[X_2_diff<0]<-X_2_diff[X_2_diff<0]+9 #to account for time between days
X_2_diff <- X_2_diff[!is.nan(X_2_diff)]
X_2_diff
# print(descdist(X_2_expo, discrete = FALSE,boot = 50000))
# hist(X_2_expo, freq = FALSE, main = paste0("n = ", m), breaks = 20)
# x<-rexp(5000)
# print(descdist(x, discrete = FALSE,boot = 1000))



#Using bootstrap mainly to find the confidence interval of the mean and to make a way bigger dataset so that the distribution can be fitted.
m2 <- length(X_2_diff)                                          # Sample size
X.bar <- mean(X_2_diff)                                        # Sample mean of X
St.Dev <- sd(X_2_diff)                                         # Standard deviation of X
Q.n <- sqrt(m2)*X.bar/St.Dev 
# Test statistic
X_2_diff.bar.star=0
# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)                            # Initialise vector for bootstrap statistics
St.Dev.stars <- rep(NA, times = B)
bootdiff2 <- rep(NA, times = B*m2)
for (b in 1:B) {
  J <- sample.int(m2, size = m2, replace = TRUE)        # Draw the indices J
  X_2_diff.star <- X_2_diff[J]                                      # Draw the bootstrap sample
  for (i in 1:m2) {
    bootdiff2[(b-1)*m2+i] <- X_2_diff.star[i]
  }
  X_2_diff.bar.star[b] <- mean(X_2_diff.star)                          # Bootstrap sample mean
  St.Dev.star <- sd(X_2_diff.star)                           # Bootstrap standard deviation
  St.Dev.stars[b] <- sd(X_2_diff.star)
  Q.star[b] <- sqrt(m2)*(X_2_diff.bar.star[b]-X.bar)/St.Dev.star # Bootstrap statistic
}
hist(X_2_diff.bar.star, freq = FALSE, main = paste0("n = ", n), breaks = 100)
cv <- quantile(Q.star, probs = 1-alpha)                 # Calculate the bootstrap critical value
p.val <- mean(Q.star > Q.n)    
quantile(X_2_diff.bar.star,c(0.025,0.975))*60  #confidence interval for the mean amount of minutes between two calls


hist(bootdiff2, freq = FALSE, main = paste0("n = ", m2), breaks = 10)
#cullen and frey graph to see which distribution to look at
print(descdist(X_2_diff, discrete = FALSE,boot = 10000))
print(descdist(bootdiff2, discrete = FALSE))


# Fit a normal distribution to the data difference between call time 2
fit_result9 <- fitdistr(X_2_diff, densfun = "normal")

# Plot histogram of the observed data
hist(X_2_diff , freq = FALSE, col = "lightblue", main = "Fitted Normal Distribution", xlab = "Observed Data")

# Add the fitted density curve to the plot
curve(dnorm(x, mean = fit_result9$estimate["mean"], sd = fit_result9$estimate["sd"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Observed Data", "Fitted Normal Distribution"), col = c("lightblue", "red"),lty=1)


# Fit a normal distribution to the bootstrapped data
fit_result10 <- fitdistr(bootdiff2, densfun = "normal")

# Plot histogram of the observed data
hist(bootdiff2 , freq = FALSE, col = "lightblue", main = "Fitted Normal Distribution", xlab = "Bootstrap Observed Data", breaks = 10)

# Add the fitted density curve to the plot
curve(dnorm(x, mean = fit_result10$estimate["mean"], sd = fit_result10$estimate["sd"]), col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Bootstrap Observed Data", "Fitted Normal Distribution"), col = c("lightblue", "red"),lty=1)


#mean and standard deviation of the normal distribution for the duration of type 1 patients
fit_result10$estimate["mean"]
fit_result10$estimate["sd"]



