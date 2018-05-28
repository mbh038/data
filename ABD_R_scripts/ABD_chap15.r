# Comparing means of more than two groups: R code for Chapter 15 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap15.r">here.
# ------------------------------------------------------------

# Example 15.1. <a href="../wp-content/data/chapter15/chap15e1KneesWhoSayNight.csv">Knees who say night
# Analysis of variance, comparing phase shift in the circadian rhythm of melatonin production in participants given alternative light treatments. Also, the nonparametric Kruskal-Wallis test. Finally, we use the same data to demonstrate planned comparisons and unplanned comparisons (Tukey-Kramer tests).
# Read and inspect the data.

circadian <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter15/chap15e1KneesWhoSayNight.csv"))

# Set the preferred ordering of groups in tables and graphs.

circadian$treatment <- factor(circadian$treatment, 
	levels = c("control", "knee", "eyes")) 

# 	
# Table of descriptive statistics by treatment group (Table 15.1-1).

meanShift <- tapply(circadian$shift, circadian$treatment, mean)
sdevShift <- tapply(circadian$shift, circadian$treatment, sd)
n         <- tapply(circadian$shift, circadian$treatment, length)
data.frame(mean = meanShift, std.dev = sdevShift, n = n)

# Strip chart of circadian rhythm shift by treatment.

stripchart(shift ~ treatment, data = circadian, method = "jitter", vertical = TRUE)

# Add standard error bars to previous strip chart. This makes use of the descriptive statistics calculated in an earlier step. The error bars are added as line segments, offset along the x-axis by the amount adjustAmount. The means are added using points.

seShift <- sdevShift / sqrt(n)
adjustAmount <- 0.15
segments( c(1,2,3) + adjustAmount, meanShift - seShift, 
	  c(1,2,3) + adjustAmount, meanShift + seShift )
points(meanShift ~ c( c(1,2,3) + adjustAmount ))

# Commands for a stripchart with more options are shown here.

par(bty="l")
adjustAmount <- 0.15
stripchart(shift ~ treatment, data = circadian, method = "jitter",
	vertical = TRUE, las = 1, pch = 1, xlab = "Light treatment",
	ylab = "Shift in circadian rhythm (h)", col = "firebrick", 
	cex = 1.2, ylim = c(-3, 1))
segments( c(1,2,3) + adjustAmount, meanShift - seShift, 
	  c(1,2,3) + adjustAmount, meanShift + seShift )
points(meanShift ~ c( c(1,2,3) + adjustAmount ), pch = 16, col = "firebrick")
# Fixed effects ANOVA table (Table 15.1-2). This is done in two steps. The first step involves fitting the ANOVA model to the data using lm ("lm" stands for "linear model", of which ANOVA is one type). Then we use the command anova to assemble the ANOVA table.

circadianAnova <- lm(shift ~ treatment, data = circadian)
anova(circadianAnova)

# R2 indicating the fraction of variation in the response variable "explained" by treatment. This is again done in two steps. The first step calculates a bunch of useful quantities from the ANOVA model object previously created with a lm command. The second step shows the R2 value.

circadianAnovaSummary <- summary(circadianAnova)
circadianAnovaSummary$r.squared

# Kruskal-Wallis test, a nonparametric method to compare more than two groups. The method is not needed for the circadian rhythm data, because assumptions of ANOVA are met, but we include it here to demonstrate the method. The formula is the same as that used with lm.

kruskal.test(shift ~ treatment, data = circadian)

# <hr class = "short">
# Planned and unplanned comparisons between means. A planned comparison is one planned during the design of the study, before the data were collected. To use the method you need a good justification to focus on a specific comparison of two treatments. Only a small number of planned comparisons are allowed. If you don't have a good prior justification for a specific comparison, use unplanned comparison instead. Unplanned comparisons typically involve testing differences between all pairs of means, and methods provide needed protection against rising Type I errors that would otherwise result from multiple testing. 
# If you haven't already done so, you'll need to install the multicomp package (this needs to be done just once per computer).

# install.packages("multcomp", dependencies = TRUE)

# Planned comparison between "control" and "knee" treatments. Begin by loading the multicomp package. The commands shown will give the 95% confidence interval and the planned t-test of a difference between the treatment means.

library(multcomp)
circadianPlanned <- glht(circadianAnova, linfct = 
			mcp(treatment = c("knee - control = 0")))
confint(circadianPlanned)
summary(circadianPlanned)

# Unplanned comparisons (Tukey-Kramer tests) between all pairs of means. The raw data for the "Wood-wide web" example (Example 15.4) are not available, so we have used the circadian rhythm data to demonstrate the R method here instead. The output table will say "t" but it is actually "q" as we describe in the book.

library(multcomp)
circadianTukey <- glht(circadianAnova, linfct = mcp(treatment = "Tukey"))
summary(circadianTukey)

# ------------------------------------------------------------

# Example 15.6. <a href="../wp-content/data/chapter15/chap15e6WalkingStickFemurs.csv">Walking stick limbs
# Random effects ANOVA to estimate variance components and calculate repeatability of measurements of femur length in walking stick insects.
# Read and inspect data. Data are in "long" format. Femur length is one column, with another variable indicating specimen identity. Each specimen was measured twice and therefore takes up two rows.

walkingstick <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter15/chap15e6WalkingStickFemurs.csv"))
head(walkingstick)

# Descriptive statistics for each specimen, which we need to produce the strip chart. tapply is used to gets the mean, smallest measurement, largest measurement, and specimen id of each specimen.

meanFemur <- tapply(walkingstick$femurLength, walkingstick$specimen, mean)
minFemur <- tapply(walkingstick$femurLength, walkingstick$specimen, min)
maxFemur <- tapply(walkingstick$femurLength, walkingstick$specimen, max)
specimen <- tapply(walkingstick$specimen, walkingstick$specimen, unique)

# Stripchart, with line segments connecting the two measurements of 

stripchart(femurLength ~ specimen, data = walkingstick, 
	vertical = TRUE, xlab = "Specimen")
segments(specimen, minFemur, specimen, maxFemur)

# Commands for a prettier figure in which specimens are ordered along the x-axis by mean femur length (Figure 15.6-1) are here.

par(bty="l") 
stripchart(minFemur[order(meanFemur, specimen)] ~ c(1:length(specimen)),
	vertical = TRUE, xaxt = "n", pch = 16, col = "firebrick", las = 1,
	cex = 1.2, ylim = range(c(minFemur, maxFemur)), ylab = "Femur length (mm)",
	xlab = "Individual walking sticks")
stripchart(maxFemur[order(meanFemur, specimen)] ~ c(1:length(specimen)),
	vertical = TRUE, add = TRUE, pch = 16, col = "firebrick", las = 1, cex = 1.2)
segments(c(1:length(specimen)), minFemur[order(meanFemur, specimen)], 
	 c(1:length(specimen)), maxFemur[order(meanFemur, specimen)])

# Fit the random effects ANOVA using lme. The random effects ANOVA function requires two formulas, rather than just one. The first formula (beginning with "fixed =") is for the fixed effect. The walking stick insect example doesn't include a fixed-effect variable, so we just provide a symbol for a constant in the formula ("~ 1"), representing the grand mean. The second formula (beginning with "random =") is for the random effect. In this example, the individual specimens are the random groups, and the second formula indicates this (the "~ 1" in the formula below indicates that each specimen has its own mean). You will need to load the nlme library to begin.

library(nlme)
walkingstickAnova <- lme(fixed = femurLength ~ 1, 
 	 	 	 random = ~ 1|specimen, data = walkingstick)

# Obtain the variance components for the random effects using VarCorr. The output includes the standard deviation and variance for both components of random variation in the random effects model for this example. The first is the variance among the specimen means. This is the variance among groups, and is confusingly labeled "Intercept" in the output. The second component is the variance among measurements made on the same individuals. This is the within group variance, also known as the error mean square, and is labeled "Residual" in the output.

walkingstickVarcomp <- VarCorr(walkingstickAnova)
walkingstickVarcomp

# Calculate the repeatability of the walking stick femur measurements using the estimates of the variance components.

varAmong  <- as.numeric( walkingstickVarcomp[1,1] )
varWithin <- as.numeric( walkingstickVarcomp[2,1] )
repeatability <- varAmong / (varAmong + varWithin)
repeatability

# Note that lme doesn't test the significance of the random effects (whether the specimen means are significantly different from one another), since the method basically assumes the presence of variance among random means. As a result, there is no ANOVA table for random effects. 
