# The normal distribution: R code for Chapter 10 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap10.r">here.
# ------------------------------------------------------------

# Example 10.4. One small step for man?
# Calculate probabilities under the normal curve. The command pnorm(Y) gives the probability of obtaining a value less than Y under the normal distribution. The arguments mean and sd give the mean and standard deviate of the desired normal distribution.
# Pr[Height < 157.5]

pnorm(157.5, mean = 177.6, sd = 9.7)

# Pr[Height > 190.54]

1 - pnorm(190.54, mean = 177.6, sd = 9.7)

# Pr[Height < 157.5 or Height > 190.54]

pnorm(157.5, mean = 177.6, sd = 9.7) + 1 - pnorm(190.54, mean = 177.6, sd = 9.7)

# ------------------------------------------------------------

# Figure 10.6. <a href="../wp-content/data/chapter10/chap10e6AgesAtDeathSpanishFlu1918.csv">Ages at death during the Spanish flu
# Demonstration of the central limit theorem, using the distribution of sample mean age at death in samples from a highly non-normal distribution: the frequency distribution of age at death in Switzerland in 1918 during the Spanish flu epidemic.
# Read and inspect the data.

flu <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter10/chap10e6AgesAtDeathSpanishFlu1918.csv"))
head(flu)

# Histogram showing the frequency distribution of ages at death in Switzerland in 1918 during the Spanish flu epidemic.

hist(flu$age, right = FALSE)

# Commands for a histogram with more options are here.

hist(flu$age, right = FALSE, breaks = seq(0,102,2), col = "firebrick", las = 1, 
	xlab = "Age at death (yrs)", ylab = "Frequency", main = "")

# Demonstrate the central limit theorem. Treat the age at death measurements from Switzerland in 1918 as the population. Take a large number
# of random samples, each of size n, from the population of age at death measurements and plot the sample means. Note: your results won't be
# the identical to the one in Figure 10.6-2, because 10,000 random samples is not large enough for extreme accuracy. Change n below to another
# number and rerun to see the effects of sample size on the shape of the distribution of sample means.

n <- 40
results <- vector()
for(i in 1:10000){
	AgeSample <- sample(flu$age, size = n, replace = FALSE)
	results[i] <- mean(AgeSample)
	}

# Histogram of the sample means, with options.

hist(results, right = FALSE, breaks = 50, col = "firebrick", las = 1, 
	xlab = "Mean age at death (yrs)", ylab = "Frequency", main = "")

# ------------------------------------------------------------

# Example 10.7. The only good bug is a dead bug
# Normal approximation to the binomial distribution applied to the brown recluse spider example.
# The P-value from the binomial test is P = 2 Pr[X &ge; 31], which is the same as 2 (1 - Pr[X < 30]), since Pr[X &ge; 31] = 1 - Pr[X < 30]. We can use the normal approximation as follows. Remember that n = 41 and p = 0.5.

spiderProb <- 1 - pnorm( (30 + 1/2 - 41 * 0.5) / sqrt(41 * 0.50 * 0.5))
Pvalue <- 2 * spiderProb
Pvalue

# Compare with the result obtained when using the binomial distribution, dbinom, which we encountered in the Chapter 7 R page.

2 * sum( dbinom(31:41, size = 41, prob = 0.5) )

# Or use pbinom.

2 * (1 - pbinom(30, size = 41, prob = 0.5))

