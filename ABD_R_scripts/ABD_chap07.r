# Analyzing proportions: R code for Chapter 7 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap07.r">here.
# ------------------------------------------------------------

# Table 7.1-1 and Figure 7.1-1. Binomial distribution with n = 27 and p = 0.25
# Table and histogram of binomial probabilities. Uses the data from Chapter 6 on the genetics of mirror-image flowers.
# Calculate a binomial probability, the probability of obtaining X successes in n trials when trials are independent and probability of success p is the same for every trial. The probability of getting exactly 6 left-handed flowers when n = 27 and p = 0.25 is

dbinom(6, size = 27, prob = 0.25)

# Table of probabilities for all possible values for the number of left-handed flowers out of 27.

xsuccesses <- 0:27
probx <- dbinom(xsuccesses, size = 27, prob = 0.25) 
probTable <- data.frame(xsuccesses, probx)
probTable

# Histogram of binomial probabilities for the number of left-handed flowers out of 27. This illustrates the full binomial distribution when n = 27 and p = 0.25.

barplot(height = probx, names.arg = xsuccesses, space = 0, las = 1, 
	ylab = "Probability", xlab = "Number of left-handed flowers")

# ------------------------------------------------------------

# Figure 7.1-2. Sampling distribution of a binomial proportion
# Compare sampling distributions for the proportion based on n = 10 and n = 100.
# Take a large number of random samples of n = 10 from a population having probability of success p = 0.25. Convert to proportions by dividing by the sample size. Do the same for the larger sample size n = 100. The following commands use 10,000 random samples.

successes10 <- rbinom(10000, size = 10, prob = 0.25)
proportion10 <- successes10 / 10
successes100 <- rbinom(10000, size = 100, prob = 0.25)
proportion100 <- successes100 / 100

# Plot and visually compare the sampling distributions of the proportions based on n = 10 and n = 100. The par(mfrow = c(2,1)) command sets up a graph window that will plot both graphs arranges in 2 rows and 1 column.

par(mfrow = c(2,1))
hist(proportion10, breaks = 10, right = FALSE, xlim = c(0,1),
	xlab = "Sample proportion")
hist(proportion100, breaks = 20, right = FALSE, xlim = c(0,1),
	xlab = "Sample proportion")
par(mfrow = c(1,1))

# Commands for a fancier plot are here.

oldpar <- par(no.readonly = TRUE) # make backup of default graph settings
par(mfrow = c(2,1), oma = c(4, 0, 0, 0), mar = c(1, 6, 4, 1)) # adjust margins
saveHist10 <- hist(proportion10, breaks = 10, right = FALSE, plot = FALSE)
saveHist10$counts <- saveHist10$counts/sum(saveHist10$counts)
plot(saveHist10, col = "firebrick", las = 1, cex.lab = 1.2,
	ylim = c(0,0.3), xlim = c(0,1), ylab = "Relative frequency",
	xlab = "", main = "")
text(x = 1, y = 0.25, labels = "n = 10", adj = 1, cex = 1.1)
saveHist100 <- hist(proportion100, breaks = 40, right = FALSE, plot = FALSE)
saveHist100$counts <- saveHist100$counts/sum(saveHist100$counts)
plot(saveHist100, col = "firebrick", las = 1, cex.lab = 1.2, 
	ylim = c(0,0.1), xlim = c(0,1), ylab = "Relative frequency", 
	xlab = "", main = "")
text(x = 1, y = 0.08, labels = "n = 100", adj = 1, cex = 1.1)
mtext("Proportion of successes", side = 1, outer = TRUE, padj = 2)
par(oldpar) # Revert to backup graph settings
# ------------------------------------------------------------

# Example 7.3. <a href="../wp-content/data/chapter07/chap07e2SexAndX.csv">Sex and the X chromosome
# The binomial test, used to test whether spermatogenesis genes in the mouse genome occur with unusual frequency on the X chromosome.
# Read and inspect the data. Each row in the data file represents a different spermatogenesis gene.

mouseGenes <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07e2SexAndX.csv"))
head(mouseGenes)

# Tabulate the number of spermatogenesis genes on the X-chromosome and the number not on the X-chromosome.

table(mouseGenes$onX)

# Calculate the binomial probabilities of all possible outcomes under the null hypothesis (Table 7.2-1). Under the binomial distribution with n = 25 and p = 0.061, the number of successes can be any integer between 0 and 25. 

xsuccesses <- 0:25
probx <- dbinom(xsuccesses, size = 25, prob = 0.061)
data.frame(xsuccesses, probx)

# Use these probabilities to calculate the P-value corresponding to an observed 10 spermatogenesis genes on the X chromosome. Remember to multiply the probability of 10 or more successes by 2 for the two-tailed test result.

2 * sum(probx[xsuccesses >= 10])

# For a faster result, try R's built-in binomial test. The resulting P-value is slightly different from our calculation. In the book, we get the two-tailed probability by multiplying the one-tailed probability by 2. As we say on page 188, computer programs may calculate the probability of extreme results at the "other" tail with a different method. The output of binom.test includes a confidence interval for the proportion using the Clopper-Pearson method, which is more conservative than the Agresti-Coull method.

binom.test(10, n = 25, p = 0.061)

# ------------------------------------------------------------

# Example 7.2. <a href="../wp-content/data/chapter07/chap07e2SexAndX.csv">Radiologists' missing sons
# Standard error and 95% confidence interval for a proportion using the Agresti-Coull method for the confidence interval.
# Read and inspect the data.

radiologistKids <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter07/chap07e3RadiologistOffspringSex.csv"))
head(radiologistKids)

# Frequency table of female and male offspring number.

table(radiologistKids$offspringSex)

#     
# Calculate the estimated proportion of offspring that are male, and the total number of radiologists.

n <- sum(table(radiologistKids$offspringSex))
n
pHat <- 30 / n
pHat

# Standard error of the sample proportion.

sqrt( (pHat * (1 - pHat))/n )

# Agresti-Coull 95% confidence interval for the population proportion.

pPrime <- (30 + 2)/(n + 4)
pPrime
lower <- pPrime - 1.96 * sqrt( (pPrime * (1 - pPrime))/(n + 4) )
upper <- pPrime + 1.96 * sqrt( (pPrime * (1 - pPrime))/(n + 4) )
c(lower = lower, upper = upper)

# Agresti-Coull 95% confidence interval for the population proportion using the binom package. To use this package you will need to install it (this needs to be done only once per computer) and load it using the library command (this needs to be done once per R session). The confidence interval from the binom package will be very slightly different from the one you calculated above because the formula we use takes a slight shortcut.

# install.packages("binom", dependencies = TRUE)
library(binom)
binom.confint(30, n = 87, method = "ac")

