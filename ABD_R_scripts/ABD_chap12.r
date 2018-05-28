# Comparing two means: R code for Chapter 12 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap12.r">here.
# ------------------------------------------------------------

# Example 12.2. <a href="../wp-content/data/chapter12/chap12e2BlackbirdTestosterone.csv">Red-winged blackbirds
# Confidence interval for mean difference and the paired t-test, comparing immunocompetence of red-winged blackbirds before and after testosterone implants.
# Read the data into a data frame. The data are in "wide" format (one row per individual).

blackbird <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter12/chap12e2BlackbirdTestosterone.csv"))
blackbird

#  Calculate and plot differences (Figure 12.2-2). We add a variable called d to the data frame with the After minus Before difference.

blackbird$d <- blackbird$logAfterImplant - blackbird$logBeforeImplant
head(blackbird)
hist(blackbird$d, right = FALSE, col = "firebrick")

# To see how to produce the strip chart with lines (Figure 12.2-1) click here.

# It helps to obtain a version of the data in "long" format.
blackbird2 = reshape(blackbird, varying = 4:5, direction = "long", 
	 idvar = "blackbird", v.names = "logAntibody", 
	 times = factor(c("before","after"), levels = c("before","after")))
head(blackbird2)

par(bty="l")
stripchart(logAntibody ~ time, data = blackbird2, vertical = TRUE, 
	xlab = "Implant treatment", ylab="Antibody production rate (ln[mOD/min])",
	xlim = c(0.6,2.4), las = 1, pch = 16, col = "firebrick")
segments(1, blackbird$logBeforeImplant, 2, blackbird$logAfterImplant)

# 95% confidence interval for the mean difference. 95% confidence intervals are part of the output of the t.test function, viewed in isolation by adding $conf.int to the command.

t.test(blackbird$d)$conf.int

# or using the paired = TRUE argument of t.test and specifying the paired variables.

t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired = TRUE)$conf.int

# Paired t-test. A paired t-test can be done either on differences you have already calculated (d here) or by using the paired=TRUE argument with the measurements from the two groups.

t.test(blackbird$d)

# or

t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired = TRUE)

# ------------------------------------------------------------

# Example 12.3. <a href="../wp-content/data/chapter12/chap12e3HornedLizards.csv">Horned lizards
# Confidence interval for the difference between two means, and the two-sample t-test, comparing horn length of live and dead (spiked) horned lizards.

lizard <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter12/chap12e3HornedLizards.csv"))
lizard

# Note that there is one missing value for the variable "squamosalHornLength". Everything is easier if we eliminate the row with missing data.

lizard2 <- na.omit(lizard)
lizard2

# Multiple histograms using the lattice package.

library(lattice)
histogram( ~ squamosalHornLength | Survival, data = lizard2,
	layout = c(1,2), col = "firebrick", breaks = seq(12, 32, by = 2),
	xlab = "Horn length (mm)")

# Click here for code to make multiple histograms using hist in base R instead.

oldpar = par(no.readonly = TRUE) # make backup of default graph settings
par(mfrow = c(2,1), oma = c(4, 6, 2, 6), mar = c(3, 4, 3, 2))
hist(lizard2$squamosalHornLength[lizard2$Survival == "living"],
	breaks = seq(12,32,by=2), col = "firebrick", las = 1,  
	main = "living", ylab = "Frequency")
hist(lizard2$squamosalHornLength[lizard2$Survival == "killed"],
	breaks = seq(12,32,by=2), col = "firebrick", las = 1,  
	main = "killed", ylab = "Frequency")
mtext("Horn length (mm)", side = 1, outer = TRUE, padj = 0)
par(oldpar) # revert to default graph settings

# 95% confidence interval for the difference between means
# The output of t.test includes the 95% confidence interval for the difference between means. Add $confint after calling the function to get R to report only the confidence interval. The formula in the following command tells R to compare squamosalHornLength between the two groups indicated by Survival.

t.test(squamosalHornLength ~ Survival, data = lizard, var.equal = TRUE)$conf.int

# A two-sample t-test of the difference between two means can be carried out with t.test by using a formula, asking if squamosalHornLength is predicted by Survival, and specifying that the variables are in the data frame lizard.

t.test(squamosalHornLength ~ Survival, data = lizard, var.equal = TRUE)

# ------------------------------------------------------------

# Example 12.4. <a href="../wp-content/data/chapter12/chap12e4BrookTrout.csv">Salmon survival with brook trout
# Welch's two-sample t-test for unequal variances, comparing chinook salmon survival in the presents and absence of brook trout. Below, we use this same example to demonstrate the 95% confidence interval for the ratio of two variances, and the F-test of equal variances.
# Read the data.

chinook <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter12/chap12e4ChinookWithBrookTrout.csv"))

# Set the preferred order of categories in tables and graphs

chinook$troutTreatment <- factor(chinook$troutTreatment, 
	levels = c("present", "absent"))

# Strip chart of the data (bare bones version of Figure 12.4-1)

stripchart(proportionSurvived ~ troutTreatment, data = chinook, 
	method = "jitter", vertical = TRUE)

# Adding the means and confidence intervals to the plot is trickier. Code for a fancier plot is given here.

# Calculate means and confidence intervals for the means.
meanProportion = tapply(chinook$proportionSurvived, chinook$troutTreatment, mean)
ciPresence = t.test(chinook$proportionSurvived[chinook$troutTreatment == "present"])$conf.int
ciAbsence = t.test(chinook$proportionSurvived[chinook$troutTreatment == "absent"])$conf.int
lower = c(ciPresence[1], ciAbsence[1])
upper = c(ciPresence[2], ciAbsence[2])

# Stripchart with options
adjustAmount = 0.2
par(bty = "l") 
stripchart(proportionSurvived ~ troutTreatment, data = chinook, vertical = TRUE, 
	method = "jitter", jitter = 0.1, pch = 1, col = "firebrick", cex = 1.5,
	las = 1, ylim = c(0, max(chinook$proportionSurvived)), lwd = 1.5,
	xlab = "Trout treatment", ylab = "Proportion surviving")
segments( c(1,2) + adjustAmount, lower, c(1,2) + adjustAmount, upper)
points(meanProportion ~ c( c(1,2) + adjustAmount ), pch = 16, cex = 1.2)

# Calculating summary statistics by group (as found in Table 12.4-3)
# Use tapply to calculate statistics by group. It has three required arguments. The first is the numeric variable of interest (a vector). The second argument is a categorical variable (a vector of the same length) identifying the groups that individuals belong to. The third argument is the name of the R function that you want to apply to the variable by group.

meanProportion <- tapply(chinook$proportionSurvived, chinook$troutTreatment, mean)
sdProportion <-   tapply(chinook$proportionSurvived, chinook$troutTreatment, sd)
nProportion <-    tapply(chinook$proportionSurvived, chinook$troutTreatment, length)
data.frame(mean = meanProportion, std.dev = sdProportion, n = nProportion)

# Welch's two-sample t-test of means for unequal variances can also be done with t.test, when the var.equal argument is set to FALSE (as it is by default):

t.test(proportionSurvived ~ troutTreatment, data = chinook, var.equal = FALSE)

# <hr class = "short">
# Here, we demonstrate the 95% confidence interval for the ratio of two variances, and F-test of equal variances, using the chinook salmon experiment.
# 95% confidence interval for variance ratio. (Warning: remember that the method is not robust to departures from assumption of normality.)

var.test(proportionSurvived ~ troutTreatment, data = chinook)$conf.int

# F-test of equal variances (Warning: Remember that the F-test is not robust to departures from assumption of normality.)

var.test(proportionSurvived ~ troutTreatment, data = chinook)

# Levene's test of equal variances. This function is in the car package, which must first be installed (see here for instructions) and loaded with the library function before use.

# Install the car package. This only need to be done once per computer. The following command
# downloads the package from the internet and configures it in your R folder. 
# install.packages("car", dependencies = TRUE)


library(car)
leveneTest(chinook$proportionSurvived, group = chinook$troutTreatment, 
	center = mean)

