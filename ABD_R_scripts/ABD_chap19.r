# Computer-intensive methods: R code for Chapter 19 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap19.r">here
# ------------------------------------------------------------

# Example 19.1 <a href="../wp-content/data/chapter19/chap19e1TwoDigitNumbers.csv">Two-digit “psychic”
# Hypothesis testing using simulation to determine whether all two-digit numbers occur with equal probability when people choose two-digit numbers haphazardly.
# Read and examine data. Each row is the two-digit number chosen by a different individual.

haphazard <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter19/chap19e1TwoDigitNumbers.csv"))
head(haphazard)

# Plot the frequency distribution of two-digit numbers chosen by volunteers. 

hist(haphazard$numberSelected, right = FALSE, breaks = seq(10,100, by = 1), 
	las = 1, col = "firebrick", xlab = "Number thought of by volunteer", 
	ylim=c(0,50))

# Frequency table of the two-digit numbers chosen by volunteers. Notice that many numbers between 0 and 99 are not represented in the data. To make sure that numbers not chosen are nevertheless included in the calculations, we must convert the variable to a factor and specify all possible categories.

table(haphazard$numberSelected)
factorData <- factor(haphazard$numberSelected, levels = 10:99)
observedFreq <- table(factorData)
observedFreq

# 	
# Calculate the observed value of the test statistic. Here, we are using the &chi;2 goodness-of-fit statistic. We are lazy and use the chisq.test function to do the calculations. R will give you a warning because of the low expected frequencies. This is exactly why we need to use simulation instead of the &chi;2 goodness-of-fit-test. 

chiData <- chisq.test(observedFreq)$statistic
chiData

# We'll need to know the sample size (number of rows, in this case) to run the simulation.

n <- nrow(haphazard)
n

# Simulate a single random sample of n numbers from 10 to 99 (with replacement), where n is the number of individuals in the sample.

randomTwoDigit <- sample(10:99, size = n, replace = TRUE)

# Calculate the test statistic for this single simulated sample (&chi;2). Remember to convert to a factor to make sure that all categories are included in the calculations. Under the null hypothesis, the expected frequency is the same for all numbers.

randomTwoDigit <- factor(randomTwoDigit, levels = 10:99)
observedFreq <- table(randomTwoDigit)
expectedFreq <- n/length(10:99)
chiSim <- sum( (observedFreq - expectedFreq)^2 / expectedFreq )
chiSim

# Repeat this process many times. The following is a loop repeated nSim times. In each iteration i, a random sample of numbers is simulated, a frequency table is calculated, and the &chi;2 statistic is computed. The statistic is saved in the ith element of the vector results.

nSim <- 10000
results <- vector()
for(i in 1:nSim){
	randomTwoDigit <- sample(10:99, size = n, replace = TRUE)
	randomTwoDigit <- factor(randomTwoDigit, levels = 10:99)
	simFreq <- table(randomTwoDigit)
	expectedFreq <- n/length(10:99)
	results[i] <- sum( (simFreq - expectedFreq)^2 / expectedFreq )
	}

# Plot the frequency distribution of &chi;2 values from the simulation. This is the simulated null distribution for the test statistic.

hist(results, right = FALSE, breaks = 50)

# The P-value is the fraction of similated chisquare values equalling or exceededing the observed value of the test statistic in the data.

P <- sum(results >= chiData)
P

# <hr class = "short">
# R also has a built-in method to simulate the null distribution and calculate the P-value. Using it simply involves providing a couple of arguments to the chisq.test function. B is the number of iterations desired. If you don't provide the probabilities of each outcome under the null hypothesis as an argument, R assumes that all the categories are equiprobable under the nuill hypothesis.

factorData <- factor(haphazard$numberSelected, levels = 10:99)
observedFreq <- table(factorData)
chisq.test(observedFreq, simulate.p.value = TRUE, B = 10000)

# ------------------------------------------------------------

# Example 19.2 <a href="../wp-content/data/chapter19/chap19e2ChimpBrains.csv">Chimp language centers
# Bootstrap estimation of the median asymmetry score for Brodmann’s area 44 in chimpanzees.
# Read and examine the data.

chimp <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter19/chap19e2ChimpBrains.csv"))
head(chimp)

# Histogram of asymmetry scores of the 20 chimps.

hist(chimp$asymmetryScore, breaks = seq(-.5, 1.25, by = 0.25), 
	right = FALSE, col = "firebrick", las = 1)

# Estimate the median asymmetry score from the data.

median(chimp$asymmetryScore)

# 	
# Obtain a single bootstrap replicate. This involves sampling (with replacement) from the actual data. Calculate the median of this bootstrap replicate.

bootSample <- sample(chimp$asymmetryScore, size = nrow(chimp), replace = TRUE)
median(bootSample)

# Repeat this process many times. The following is a loop repeated B times. In each iteration, the bootstrap replicate estimate (median) is recalculated and saved in the results vector bootMedian.

B <- 10000
bootMedian <- vector()
for(i in 1:B){
	bootSample <- sample(chimp$asymmetryScore, size = nrow(chimp), replace = TRUE)
	bootMedian[i] <- median(bootSample)
	}

# Draw a histogram of the bootstrap replicate estimates for median asymmetry. Note: your results won't be the identical to the one in Figure 19.2-2, because 10,000 random samples is not large enough to obtain the sampling distribution with extreme accuracy. Increase B in the above loop for greater accuracy.

hist(bootMedian, breaks = 100, right = FALSE, col = "firebrick", las = 1)

# Calculate the mean of the bootstrap replicate estimates.

mean(bootMedian)

# The bootstrap standard error is the standard deviation of the bootstrap replicate estimates.

sd(bootMedian)

# Use the percentiles of the bootstrap replicate estimates to obtain a bootstrap 95% confidence interval for the population median.

quantile(bootMedian, probs = c(0.025, 0.975))

# <hr class = "short">
# Use the boot package for bootstrap estimation. 

library(boot)

# To begin, we need to define a new R function to tell the boot package what statistic we want to estimate, which is the median in this example. Let's call the new function boot.median. The syntax required by the package is not too complex, but our function must include a counter as an argument (as usual, we call it i).

boot.median <- function(x, i){ median(x[i]) }

# Use the boot function of the boot package to obtain many bootstrap replicate estimates and calculate the bootstrap standard error.

bootResults <- boot(chimp$asymmetryScore, boot.median, R = 10000)
bootResults

# Draw a histogram of the bootstrap replicate estimates for median asymmetry.

hist(bootResults$t, breaks = 100, right = FALSE, col = "firebrick", las = 1)

# Get the bootstrap 95% confidence interval for the population median. We show two methods. The percentile method is already familiar, but boot also provides an improved method, called "bias corrected and accelerated".

boot.ci(bootResults, type = "perc")
boot.ci(bootResults, type = "bca")

# ------------------------------------------------------------

# Figure 19.2-3 <a href="../wp-content/data/chapter13/chap13e5SagebrushCrickets.csv">Sexual cannibalism in crickets revisited
# Bootstrap estimation of the difference between medians of two groups, using times to mating (in hours) of female sagebrush crickets that were either starved or fed (data from Example 13.5). We show the method using R's boot package.
# Read and inspect the data.

cannibalism <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter13/chap13e5SagebrushCrickets.csv"))
head(cannibalism)

# Calculate the observed difference between the sample medians of the two groups.

twoMedians <- tapply(cannibalism$timeToMating, cannibalism$feedingStatus, median)
diffMedian <- twoMedians[2] -  twoMedians[1]
diffMedian

# Define a new function to calculate the difference between the medians of the bootstrap replicate estimates. Let's call it boot.diffMedian.

boot.diffMedian <- function(x, i){
	twoMedians <- tapply(x$timeToMating[i], x$feedingStatus[i], median)
	diffMedian <- twoMedians[2] -  twoMedians[1]
	}

# Load the boot package, obtain the many bootstrap replicate estimates and get the bootstrap standard error.

library(boot)
bootResults <- boot(cannibalism, boot.diffMedian, R = 10000)
bootResults

# Histogram of the bootstrap replicate estimates of the difference between two medians (Figure 19.2-3). 

hist(bootResults$t, breaks = 20, right = FALSE, col = "firebrick", las = 1)

# Obtain the bootstrap 95% confidence interval for the difference between the population medians, using the percentile and the "bca" method.

boot.ci(bootResults, type = "perc")
boot.ci(bootResults, type = "bca")

