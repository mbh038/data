# Inference for a normal population: R code for Chapter 11 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap11.r">here.
# ------------------------------------------------------------

# Example 11.2. <a href="../wp-content/data/chapter11/chap11e2Stalkies.csv">Stalk-eyed flies
# Confidence intervals for the population mean, variance, and standard deviation using eye span measurements from a sample of stalk-eyed flies.
# Read and inspect the data.

stalkie <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter11/chap11e2Stalkies.csv"))
stalkie

# Histogram with options

hist(stalkie$eyespan, right = FALSE, col = "firebrick", las = 1, 
	xlab = "Eye span (mm)", ylab = "Frequency", main = "")

# 95% confidence interval for the mean. Adding $conf.int after the function t.test causes R to give the 95% confidence interval for the mean.

t.test(stalkie$eyespan)$conf.int

# 99% confidence interval for the mean. Adding the argument conf.level=0.99 changes the confidence level of the confidence interval.

t.test(stalkie$eyespan, conf.level = 0.99)$conf.int

# 95% confidence interval for variance. R has no built-in function for the confidence interval of a variance, so must we compute it using the formula in the book:

df <- length(stalkie$eyespan) - 1
varStalkie <- var(stalkie$eyespan)
lower = varStalkie * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper = varStalkie * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(lower = lower, variance = varStalkie, upper = upper)

# 95% confidence interval for standard deviation. Calculated from the confidence interval of the variance, which we just calculated above.

c(lower = sqrt(lower), std.dev = sqrt(varStalkie), upper = sqrt(upper))

# ------------------------------------------------------------

# Example 11.3. <a href="../wp-content/data/chapter11/chap11e3Temperature.csv">Human body temperature
# Uses a one-sample t-test to compare body temperature in a random sample of people with the "expected" temperature 98.6 &deg;F.
# Read and inspect the data.

heat <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter11/chap11e3Temperature.csv"))
head(heat)

# Histogram with options.

hist(heat$temperature, right = FALSE, breaks = seq(97, 100.5, by = 0.5),
	col = "firebrick", las = 1, xlab = "Body temperature (degrees F)", 
	ylab = "Frequency", main = "")

# One-sample t-test can be calculate using t.test. The mu arguemtn gives the value stated in the null hypothesis.

t.test(heat$temperature, mu = 98.6)

