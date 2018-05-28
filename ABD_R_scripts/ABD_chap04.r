# Estimating with uncertainty: R code for Chapter 4 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap04.r">here.
# ------------------------------------------------------------

# Example 4.1. <a href="../wp-content/data/chapter04/chap04e1HumanGeneLengths.csv">The length of human genes
# Describe the parameters of a known population of human gene lengths. Then, take a random sample from the known population to estimate the population mean.
# Read the human gene length data, which we will use as our known population of measurements.

humanGeneLengths <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter04/chap04e1HumanGeneLengths.csv"))
head(humanGeneLengths)

# Draw a histogram of the length of genes in the human genome (Figure 4.1-1). We first use subset to grab the large majority of genes that are 15,000 nucleotides or less in length and put into a second data frame. 

geneLengthsUpTo15K <- subset(humanGeneLengths, geneLength <= 15000)
hist(geneLengthsUpTo15K$geneLength, right = FALSE)

# A fancier histogram that makes use of additional options can be produced with the commands here.
hist(geneLengthsUpTo15K$geneLength, breaks = seq(0,15000,500), 
	xlab = "Gene length (Number of nucleotides)", ylab = "Frequency", 
	col = "firebrick", las = 1, main = "", right = FALSE)
# Calculate the population mean and standard deviation (Table 4.1-1). Just this once we must use the total number of genes N instead of N - 1 in the denominator when calculating the variance and standard deviation, because we treat all the genes of the human genome as a population for this exercise, not as a sample. For this reason we can't use the built-in commands to calculate variance and standard deviation, because they divide by N - 1.

meanGeneLength <- mean(humanGeneLengths$geneLength)
meanGeneLength
N <- nrow(humanGeneLengths)
varGeneLength <- sum( (humanGeneLengths$geneLength - meanGeneLength)^2 ) / N
sdGeneLength <- sqrt(varGeneLength)
sdGeneLength

# Commands to put the mean and standard deviation into a table are shown here.

data.frame(Parameter = c("Mean", "Standard deviation"), 
	Value = c(meanGeneLength, sdGeneLength))
# <hr class = "short">
# Take a single random sample of 100 genes from the population of genes. The argument replace = FALSE ensures that the same gene is not sampled twice. Save your random sample to a vector. Note: your sample won't be the identical to the one in the book, because each random sample is subject to sampling error.

geneSample100 <- sample(humanGeneLengths$geneLength, size = 100, replace = FALSE)

# Draw a histogram of the unique random sample.

hist(geneSample100, right = FALSE)

# Commands for a fancier version of the histogram are provided here.

# These commands adjust the axes so that this sample's histogram can be 
# compared with that of the population histogram.
hist(geneSample100[geneSample100 <= 15000], breaks = seq(0,15000,500), 
	right = FALSE, col = "firebrick", las = 1,  
	xlab = "Gene length (no. nucleotides)",
	ylab = "Frequency", main = "")
# Calculate the sample mean and standard deviation of the unique random sample. Note: because of sampling error, your values won't be the identical to the ones in Table 4.1-2.

mean(geneSample100)
sd(geneSample100)

# Calculate the standard error of the mean gene length for the unique sample of 100 genes. The length command indicates the number of elements in a vector variable, which is the sample size if there are no missing (NA) elements in the vector. You won't get the same value for the standard error as we obtained (146.3, p 102) because your unique random sample will not be the same as ours.

n <- length(geneSample100)
sd(geneSample100) / sqrt(n)

# <hr class = "short">
# Create a loop to take repeated random samples from the population and calculate the mean on each sample. This generates the sampling distribution of the mean. Take a large number (10,000) of random samples, each of size 100. On each iteration, the sample mean is calculated and saved in a vector named results100 (the samples themselves are not saved). The results vector is initialized before the loop. The term results100[i] refers to the ith element of results100, where i is a counter. This many iterations might take a few minutes to run on your computer. 

results100 <- vector() 
for(i in 1:10000){
	temporarySample <- sample(humanGeneLengths$geneLength, size = 100, 
		replace = FALSE)
	results100[i] <- mean(temporarySample)
	}

# Plot the sample means in a histogram. The histogram shows the sampling distribution of the sample mean. Note: your results won't be completely identical to Figure 4.1-3, because 10,000 random samples is not a large enough number of iterations to obtain the true sampling distribution with extreme accuracy.

hist(results100, breaks = 50, right = FALSE)

# A histogram showing the sampling distribution as relative frequencies (Figure 4.1-3) can be obtained with the commands here. 

# These commands use "hist" to count the frequencies in each bin (without generating a plot).
# The frequencies are converted to proportions, which are finally plotted.
saveHist <- hist(results100, breaks = 50, right = FALSE, plot = FALSE)
saveHist$counts <- saveHist$counts / sum(saveHist$counts) 
plot(saveHist, , col = "firebrick", las = 1, main = "",
	xlab = "Sample mean length (nucleotides)", ylab = "Relative frequency")

# Commands to compare the sampling distribution of the mean for different sample sizes (Figure 4.1-4) are shown here.

results20 <- vector()
for(i in 1:10000){
	tmpSample <- sample(humanGeneLengths$geneLength, size = 20, replace = FALSE)
	results20[i] <- mean(tmpSample)
	}
results100 <- vector()
for(i in 1:10000){
	tmpSample <- sample(humanGeneLengths$geneLength, size = 100, replace = FALSE)
	results100[i] <- mean(tmpSample)
	}
results500 <- vector()
for(i in 1:10000){
	tmpSample <- sample(humanGeneLengths$geneLength, size = 500, replace = FALSE)
	results500[i] <- mean(tmpSample)
	}
par(mfrow = c(3,1))  # put 3 plots on the same page in 3 rows, 1 column
hist(results20, breaks = 50, right = FALSE, xlim = range(results20),
	col = "firebrick", main = "n = 20", xlab = "")
hist(results100, breaks = 50, right = FALSE, xlim = range(results20),
	col = "firebrick", main = "n = 100", xlab = "")
hist(results500, breaks = 50, right = FALSE, xlim = range(results20),
	col = "firebrick", main = "n = 500", xlab = "")
par(mfrow = c(1,1))  # change layout back to 1 plot per page

# Commands to display approximate confidence intervals of the population mean for 20 random samples (Figure 4.3-1) are available here.

geneLengthsUpTo15K <- subset(humanGeneLengths, geneLength <= 15000)
results100 <- data.frame(mean = rep(0,20), lower = rep(0,20), upper = rep(0,20))
index <- 1:20
for(i in index){
	tmpSample <- sample(geneLengthsUpTo15K$geneLength, size = 100, replace = FALSE)
	t <- t.test(tmpSample)
	results100$mean[i] <- t$estimate
	results100$lower[i] <- t$conf.int[1]
	results100$upper[i] <- t$conf.int[2]
	}
plot(index ~ mean, data = results100, pch = 16, col = "red", yaxt = "n",
	xlim = c(min(results100$lower), max(results100$upper)), bty = "l", 
	xlab = "Gene length (number of nucleotides)", ylab = "")
lines(c(2622.0, 2622.0), c(0,20), lty = 2)
segments(results100$lower, index, results100$upper, index)
segments(results100$lower, index - 0.25, results100$lower, index + 0.25)
segments(results100$upper, index - 0.25, results100$upper, index + 0.25)

# ------------------------------------------------------------

# Figure 4.4-1. <a href="../wp-content/data/chapter04/chap02f1_2locustSerotonin.csv">Locust serotonin
# Draw a strip chart with standard error bars for the locust serotonin data. The data are from Chapter 2 (Figure 2.1-2).
# Read and inspect the data.

locustData <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter02/chap02f1_2locustSerotonin.csv"))
head(locustData)

# Begin by drawing the strip chart. Keep the graphic window open because we'll be adding the error bars to this plot.

stripchart(serotoninLevel ~ treatmentTime, data = locustData, 
	method = "jitter", vertical = TRUE)

# Now calculate the statistics by group needed for the error bars: the mean and standard error. Here, tapply is used to obtain each quantity by treatment group. 

meanSerotonin <- tapply(locustData$serotoninLevel, locustData$treatmentTime, mean)
sdSerotonin <- tapply(locustData$serotoninLevel, locustData$treatmentTime, sd)
nSerotonin <- tapply(locustData$serotoninLevel, locustData$treatmentTime, length)
seSerotonin <- sdSerotonin / sqrt(nSerotonin)

# Finally, add the error bars to the strip chart. Offset their position (a constant offsetAmount is used below) so that they are drawn to the right of the data points. Somewhat confusingly, the treatments are "0", "1", and "2", but the positions of points for the three treatments along the x-axis are 1, 2, and 3. This is because the treatment variable is a factor, whose first level is always at position 1, the second is at 2, and so on.

offsetAmount <- 0.2
segments( c(c(1,2,3) + offsetAmount), meanSerotonin - seSerotonin, 
	  c(c(1,2,3) + offsetAmount), meanSerotonin + seSerotonin)
points(meanSerotonin ~ c(c(1,2,3) + offsetAmount), pch = 16, cex = 1.2)

# Commands for a fancier stripchart with additional options are provided here.

offsetAmount <- 0.2
par(bty = "l") 
stripchart(serotoninLevel ~ treatmentTime, data = locustData, vertical = TRUE, 
	method = "jitter", jitter = 0.1, pch = 16, xlab = "Treatment time (hours)", 
	ylab = "Serotonin (pmoles)", col = "firebrick", cex = 1.5, las = 1, 
	ylim = c(0, max(locustData$serotoninLevel)))
points(meanSerotonin ~ c(c(1,2,3) + offsetAmount), pch = 16, cex = 1.2)
arrows( c(c(1,2,3) + offsetAmount), meanSerotonin - seSerotonin, 
	c(c(1,2,3) + offsetAmount), meanSerotonin + seSerotonin, 
	angle = 90, code = 3, length = 0.1)

