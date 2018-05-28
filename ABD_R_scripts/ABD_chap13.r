# Handling violations of assumptions: R code for Chapter 13 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap13.r">here.
# ------------------------------------------------------------

# Example 13.1. <a href="../wp-content/data/chapter13/chap13e1MarineReserve.csv">Biomass in marine reserves
# The normal quantile plot, Shapiro-Wilk test of normality, and the log transformation, investigating the ratio of biomass between marine reserves and non-reserve control areas.
# Read and inspect the data.

marine <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter13/chap13e1MarineReserve.csv"))
head(marine)

# Histogram of biomass ratio (Figure 13.1-4).

hist(marine$biomassRatio, right = FALSE, col = "firebrick", las = 1)

# Normal quantile plot of biomass ratio data.

qqnorm(marine$biomassRatio, datax = TRUE)

# Commands for a normal quantile plot with more options (Figure 13.1-4) are here.

qqnorm(marine$biomassRatio, datax = TRUE, pch = 16, col = "firebrick", 
	las = 1, ylab = "Biomass ratio", xlab = "Normal quantile", main = "")

# Shapiro-Wilk test of normality.

shapiro.test(marine$biomassRatio)

# <hr class = "short">
# Natural log-transformation and resulting confidence interval of the a mean of marine biomass ratio.
# Log-transformation. The function log() takes the natural logarithm of all the elements of a vector or variable. The following command puts the results into a new variable in the same data frame, marine.

marine$lnBiomassRatio <- log(marine$biomassRatio)

# Histogram of the log-transformed marine biomass ratio (Figure 13.3-2) .

hist(marine$lnBiomassRatio, right = FALSE, col = "firebrick", las = 1)

# 95% confidence interval of the mean using the log-transformed data.

t.test(marine$lnBiomassRatio)$conf.int

# Back-transform lower and upper limits of confidence interval (exp is the inverse of the log function).

exp( t.test(marine$lnBiomassRatio)$conf.int )

# ------------------------------------------------------------

# Example 13.4. <a href="../wp-content/data/chapter13/chap13e4SexualConflict.csv">Sexual conflict and speciation
# The sign test, comparing the numbers of species in 25 pairs of closely related insect taxa.
# Read and inspect the data.

conflict <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter13/chap13e4SexualConflict.csv"))
head(conflict)

# Histogram of the difference in numbers of species (Figure 13.4-1).

hist(conflict$difference, right = FALSE, col = "firebrick",
	breaks = 50, xlab = "Difference in species number", las = 1)

# Count up the frequency of differences that are below, equal to, and above zero. The first command below creates a new variable called zero in the data frame and sets all elements to "equal". This is just to get started. The next two commands overwrite those elements corresponding to taxon pairs with a difference greater than zero, or less than zero, respectively. 

conflict$zero <- "equal"
conflict$zero[conflict$difference > 0] <- "above"
conflict$zero[conflict$difference < 0] <- "below"
conflict$zero <- factor(conflict$zero, levels = c("below", "equal", "above"))
table(conflict$zero)

#     
# Sign test. The sign test is just a binomial test. The result includes a confidence interval for the proportion using the Clopper-Pearson method, which isn't covered in the book.

binom.test(7, n = 25, p = 0.5)

# ------------------------------------------------------------

# Example 13.5. <a href="../wp-content/data/chapter13/chap13e5SagebrushCrickets.csv">Cricket sexual cannibalism
# The Wilcoxon rank-sum test (equivalent to the Mann-Whitney U-test) comparing times to mating (in hours) of starved and fed female sagebrush crickets. We also apply the permutation test to the same data.
# Read and inspect data.

cannibalism <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter13/chap13e5SagebrushCrickets.csv"))
head(cannibalism)

# Multiple histograms (Figure 13.5-1) using the lattice package

library(lattice)
histogram( ~ timeToMating | feedingStatus, data = cannibalism,
	layout = c(1,2), col = "firebrick", breaks = seq(0, 100, by = 20),
	type = "count", xlab = "Time to mating (hours)", ylab = "Frequency")

# Commands for multiple histograms in basic R are here.

oldpar = par(no.readonly = TRUE) # make backup of default graph settings
par(mfrow = c(2,1), oma = c(4, 6, 2, 6), mar = c(3, 4, 3, 2))
hist(cannibalism$timeToMating[cannibalism$feedingStatus == "starved"],
	col = "firebrick", las = 1, breaks = seq(0, 100, by = 20), 
	main = "starved", ylab = "Frequency")
hist(cannibalism$timeToMating[cannibalism$feedingStatus == "fed"],
	col = "firebrick", las = 1, breaks = seq(0, 100, by = 20), 
	main = "fed", ylab = "Frequency")
mtext("Time to mating (hours)", side = 1, outer = TRUE, padj = 0)
par(oldpar) # revert to default graph settings

# Wilcoxon rank-sum test (equivalent to Mann-Whitney U-test)

wilcox.test(timeToMating ~ feedingStatus, data = cannibalism)

# <hr class = "short">
# Permutation test of the difference between mean time to mating of starved and fed crickets.
# Begin by calculating the observed difference between means (starved minus fed). The difference is -18.25734 in this data set.

cricketMeans <- tapply(cannibalism$timeToMating, cannibalism$feedingStatus, mean)
cricketMeans
diffMeans <- cricketMeans[2] - cricketMeans[1]
diffMeans

# Decide on the number of permutations.

nPerm <- 10000

# Create a loop to permute the data many times (determined by nperm). In the loop, i is just a counter that goes from 1 to nPerm by 1; each permuted difference is saved in the permResult.

permResult <- vector() # initializes
for(i in 1:nPerm){
	# step 1: permute the times to mating
	permSample <- sample(cannibalism$timeToMating, replace = FALSE)
	# step 2: calculate difference betweeen means
	permMeans <- tapply(permSample, cannibalism$feedingStatus, mean)
	permResult[i] <- permMeans[2] - permMeans[1]
	}

# Plot null distribution based on the permuted differences (Figure 13.8-1).

hist(permResult, right = FALSE, breaks = 100)

# Use the null distribution to calculate an approximate P-value. This is the twice the proportion of the permuted means that fall below the observed difference in means, diffMeans (-18.25734 in this example). The following code calculates the number of permuted means falling below diffMeans.

sum(as.numeric(permResult <= diffMeans))

# These commands obtain the fraction of permuted means falling below diffMeans.

sum(as.numeric(permResult <= diffMeans)) / nPerm

# Finally, multiply by 2 to get the P-value for a two-sided test.

2 * ( sum(as.numeric(permResult <= diffMeans)) / nPerm )

