# Correlation between numerical variables: R code for Chapter 16 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap16.r">here
# ------------------------------------------------------------

# Example 16.1. <a href="../wp-content/data/chapter16/chap16e1FlippingBird.csv">Flipping Booby
# Estimate a linear correlation between the number of non-parent adult visits experienced by boobies as chicks and the number of similar behaviors performed by the same birds when adult. 
# Read and inspect the data

booby <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter16/chap16e1FlippingBird.csv"))
head(booby)

# Scatter plot.

plot(futureBehavior ~ nVisitsNestling, data = booby)

# For a fancier scatter plot using more options (Figure 16.1-4), see the commands here.

plot(futureBehavior ~ nVisitsNestling, data = booby, pch = 16, col = "firebrick",
	las = 1, bty = "l", cex = 1.2, xlab = "Events experienced as a nestling", 
	ylab = "Future behavior")

# Correlation coefficient. The cor.test function computes a number of useful quantities, which we save in the object boobyCor. The quantities can be extracted one at a time or shown all at once.

boobyCor <- cor.test(booby$futureBehavior, booby$nVisitsNestling)
boobyCor

# If only the estimated correlation and standard error are of interest, they can be obtained as follows. The calculation of standard error uses nrow(booby) to get the sample size for the correlation, but this will only be true if there are no missing values.

r <- boobyCor$estimate
r
SE <- sqrt( (1 - r^2)/(nrow(booby) - 3) )
unname(SE)

# Confidence limit for a correlation coefficient. The 95% confidence interval for the correlation is included in the output of cor.test. If all you want is the confidence interval, it can be extracted from the boobyCor calculated in an earlier step.

boobyCor$conf.int

# ------------------------------------------------------------

# Example 16.2. <a href="../wp-content/data/chapter16/chap16e2InbreedingWolves.csv">Inbreeding wolves
# Test a linear correlation between inbreeding coefficients of litters of mated wolf pairs and the number of pups surviving their first winter.
# Read and inspect data.

wolf <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter16/chap16e2InbreedingWolves.csv"))
head(wolf)

# Scatter plot.

plot(nPups ~ inbreedCoef, data = wolf)

# A fancier scatter plot with more options can be made with the commands here.

plot(nPups ~ inbreedCoef, data = wolf, pch = 16, col = "firebrick",
	las = 1, bty = "l", cex = 1.2, xlab = "Inbreeding coefficient", 
	ylab = "Number of pups")

# Test of zero correlation. The results of the test are included in the output of cor.test.

cor.test(wolf$nPups, wolf$inbreedCoef)

# ------------------------------------------------------------

# Figure 16.4-1. <a href="../wp-content/data/chapter16/chap16f4_1StreamInvertebrates.csv">Stream invertebrates
# Effect of the range of the data on the correlation coefficient between population density of (log base 10 of number of individuals per square meter) and body mass (g) of different species of stream invertebrates.
# Read and inspect the data.

streamInvert <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter16/chap16f4_1StreamInvertebrates.csv"))
head(streamInvert)

# Scatter plot.

plot(log10Density ~ log10Mass, data = streamInvert)

# See here for commands to make a scatter plot of these data with more options.

plot(log10Density ~ log10Mass, data = streamInvert, pch = 16, col = "firebrick",
	las = 1, bty = "l", cex = 1.2, xlab = "Log population density", 
	ylab = "Log body mass")

# Effect of the range of the data on the correlation coefficient. Here is the correlation coefficient for the full range of the data. The command uses cor.test but we extract just the correlation coefficient for this exercise.

cor.test(streamInvert$log10Density, streamInvert$log10Mass)$estimate

# Here is the correlation coefficient for the subset of the data corresponding to a log10Mass between 0 and 2. 

streamInvertReduced <- subset(streamInvert, log10Mass > 0 & log10Mass < 2)
cor.test(streamInvertReduced$log10Density, streamInvertReduced$log10Mass)$estimate

# ------------------------------------------------------------

# Example 16.5. <a href="../wp-content/data/chapter16/chap16e5IndianRopeTrick.csv">Indian rope trick
# Spearman rank correlation between impressiveness score of the Indian rope trick and the number of years elapsed bewteen the witnessing of the trick and the telling of it in writing.
# Read and inspect the data.

trick <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter16/chap16e5IndianRopeTrick.csv"))
head(trick)

# Scatter plot.

plot(impressivenessScore ~ years, data = trick)

# See here for commands to make a scatter plot of these data with more options.

plot(impressivenessScore ~ years, data = trick, pch = 16, col = "firebrick",
	las = 1, bty = "l", cex = 1.2, xlab = "Years elapsed", 
	ylab = "Impressiveness score")

# Test of zero Spearman rank correlation. In this example, the variable "impressivenessScore" is a number score with lots of tied observations. Because of the ties, R will warn you that the P-value in the output is not exact.

cor.test(trick$years, trick$impressivenessScore, method = "spearman")

