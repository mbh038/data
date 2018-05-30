# Describing data: R code for Chapter 3 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap03.r">here.
# ------------------------------------------------------------

# Example 3.1. <a href="../wp-content/data/chapter03/chap03e1GlidingSnakes.csv">Gliding snakes
# Sample mean, standard deviation, variance and coefficient of variation of undulation rate of 8 gliding paradise tree snakes, in Hz.
# Read and inspect the data.

snakeData <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03e1GlidingSnakes.csv"))
head(snakeData)

# Draw a histogram of the data.

hist(snakeData$undulationRateHz, right = FALSE)

# Commands for a fancier histogram are shown here.

hist(snakeData$undulationRateHz, right = FALSE, las = 1, col = "firebrick", 	
	breaks = seq(0.8,2.2,by=0.2), xlab = "Undulation rate (Hz)", 
	ylab = "Frequency", main = "")

# Calculate the mean, standard deviation and variance of the undulation rates.

mean(snakeData$undulationRate)
sd(snakeData$undulationRate)
var(snakeData$undulationRate)

# Calculate the coefficient of variation.

100 * sd(snakeData$undulationRate)/mean(snakeData$undulationRate)

# ------------------------------------------------------------

# Table 3.1-2. <a href="../wp-content/data/chapter03/chap03t1_2ConvictionsFreq.csv">Numbers of convictions
# Mean and standard deviation from a frequency table. The data are from Chapter 2.
# Read and inspect the data. It is a frequency table of the number of convictions.

convictionsFreq <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03t1_2ConvictionsFreq.csv"))
head(convictionsFreq)

# Calculate the mean and standard deviation from the frequency table. First, use the rep command to "repeat" each value in the table,
# the number of times according to its frequency. Here, we store the result in convictions.

convictions <- rep(convictionsFreq$convictions, convictionsFreq$frequency)

# Then, calculate mean and standard deviation on the result.

mean(convictions)
sd(convictions)

# ------------------------------------------------------------

# Example 3.2. <a href="../wp-content/data/chapter03/chap03e2SpiderAmputation.csv">Spider running speed
# Median, interquartile range, and box plot of running speed (cm/s) of male Tidarren spiders. We also include below the cumulative 
# frequency distribution of running speed before amputation.
# Read and inspect the data. The data are in "long" format. One variable indicates running speed, and a second variable gives treatment
# (before vs after amputation). Therefore, every individual spider is on two rows, once for its before-amputation measurement and one for
# its after-amputation measurement.

spiderData <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03e2SpiderAmputation.csv"))
head(spiderData)

# Box plot of the data. Begin by ordering the treatment levels so that the "before" amputation measurements come before the "after" 
# measurements in the plot.

spiderData$treatment <- factor(spiderData$treatment, levels = c("before", "after"))
boxplot(speed ~ treatment, data = spiderData)

# Instructions for a fancier box plot, made with additional options, is shown here.

par(bty = "l")
boxplot(speed ~ treatment, data = spiderData, ylim = c(0,max(spiderData$speed)),
	col = "goldenrod1", boxwex = 0.5, whisklty = 1, las = 1,
	xlab = "Amputation treatment", ylab = "Running speed (cm/s)")

# Extract the before-amputation data using subset. Note the double equal sign "==" needed in the logical statement, which indicates 
# "is equal to" in R. Save the result in a new data frame named speedBefore.

speedBefore <- subset(spiderData, treatment == "before") 
speedBefore

# Calculate the sample median of before-amputation running speed.

median(speedBefore$speed)

# Calculate the first and third quartiles of before-amputation running speed (0.25 and 0.75 quantiles). 
# Type 5 reproduces the method we use in the book to calculate quartiles.

quantile(speedBefore$speed, probs = c(0.25, 0.75), type = 5)

# Determine the interquartile range of before-amputation running speed. Type 5 reproduces the method we use in the book to calculate 
# quartiles.

IQR(speedBefore$speed, type = 5)

# Draw a cumulative frequency distribution of running speed before amputation (Figure 3.4-1).

plot( ecdf(speedBefore$speed), verticals = TRUE,
	ylab = "Cumulative relative frequency", 
	xlab = "Running speed before amputation (cm/s)")

# Instructions for a slightly more polished cumulative frequency distribution are here.

par(bty = "l")
plot( ecdf(speedBefore$speed), verticals = TRUE,  
	las = 1, main = "", do.points = FALSE,
	ylab = "Cumulative relative frequency", 
	xlab = "Running speed before amputation (cm/s)" )

# ------------------------------------------------------------

# Example 3.3. <a href="../wp-content/data/chapter03/chap03e3SticklebackPlates.csv">Stickleback lateral plates
# Draw multiple histograms and produce a table of descriptive statistics by group for plate numbers of three stickleback genotypes. We also include a table of frequencies and proportions of stickleback genotypes. 
# Download and inspect the data.

sticklebackData <- read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter03/chap03e3SticklebackPlates.csv"))
head(sticklebackData)

# Draw multiple histograms of number of plates, one histogram per genotype. Begin by setting the preferred order of the three genotypes in the figure. Here, we use the lattice package to draw the histograms, so it must be loaded first.

sticklebackData$genotype <- factor(sticklebackData$genotype, 
	levels = c("MM","Mm","mm"))
library(lattice)
histogram(~ plates | genotype, data = sticklebackData, breaks = seq(0,70,by=2), 
	  layout = c(1, 3), col = "firebrick")

# Commands to draw multiple histograms in base R, without using the lattice package, are here. This method is more tedious, 
# but allows for easier addition of options.

oldpar = par(no.readonly = TRUE) # make backup of default graph settings
par(mfrow = c(3,1), las = 1, oma = c(4, 6, 2, 6), mar = c(2, 5, 4, 2)) # adjust margins
hist(sticklebackData$plates[sticklebackData$genotype == "MM"], right = FALSE, 
	  breaks = seq(0,70,by=2), main = "MM", col = "firebrick",
	  las = 1, ylab = "Frequency")
hist(sticklebackData$plates[sticklebackData$genotype == "Mm"], right = FALSE, 
	  breaks = seq(0,70,by=2), main = "Mm", col = "firebrick",
	  las = 1, ylab = "Frequency")
hist(sticklebackData$plates[sticklebackData$genotype == "mm"], right = FALSE, 
	  breaks = seq(0,70,by=2), main = "mm", col = "firebrick",
	  las = 1, ylab = "Frequency")
mtext("Number of lateral plates", side = 1, outer = TRUE, padj = 1.5)
par(oldpar) # revert to default graph settings

# Make a table of descriptive statistics by group using tapply. The commands below assume that the variables contain no missing (NA) elements. 
# We need to calculate all the statistics, one at a time. Save them so that we can put them all together in a table afterward. 
# To begin, get the sample sizes by group (genotype):

n <- tapply(sticklebackData$plates, INDEX = sticklebackData$genotype, FUN = length)
n

# Next, calculate the mean number of plates by group. Let's round the results to 1 decimal place to make it easier to read them when we
# place into a table. To accomplish this, set digits = 1 in the round function. 

meanPlates <- tapply(sticklebackData$plates, INDEX = sticklebackData$genotype, 
	FUN = mean)
meanPlates <- round(meanPlates, digits = 1)
meanPlates

# Repeat for medians.

medianPlates <- tapply(sticklebackData$plates, INDEX = sticklebackData$genotype, 
	FUN = median)
medianPlates

# Repeat for standard deviations, including rounding.

sdPlates <- tapply(sticklebackData$plates, INDEX = sticklebackData$genotype, FUN = sd)
sdPlates <- round(sdPlates, 1) 
sdPlates

# Finally, the interquartile range by group.

iqrPlates <- tapply(sticklebackData$plates, INDEX = sticklebackData$genotype, 
	FUN = IQR, type = 5)
iqrPlates

# Make the table by assembling all the results in a new data frame (data frames can be used as tables for display purposes).

sticklebackTable <- data.frame(genotype = names(n), n = n, 
	mean = meanPlates, median = medianPlates, 
	sd = sdPlates, iqrange = iqrPlates)
sticklebackTable

# We can make a table of frequencies and proportions of the stickleback genotypes (Table 3.5-1). Below, we first generate a frequency
# table of genotypes (the dnn argument names the variable in the table). 

sticklebackFreq <- table(sticklebackData$genotype, dnn = "genotype")
sticklebackFreq

# We then convert the table to a data frame so that we can include the proportions in the table too. 

sticklebackFreq <- data.frame(sticklebackFreq)
sticklebackFreq

# Finally, we calculate the proportions and put them into the data frame. To convert frequencies to proportions, divide the 
# frequencies by the sum of the frequencies.

sticklebackFreq$proportion <- sticklebackFreq$Freq / sum(sticklebackFreq$Freq)
# sticklebackFreq$proportion <- round(sticklebackFreq$proportion,3)
sticklebackFreq

# The table would look even nicer if you round the proportions before including them in the table (give this a try).
