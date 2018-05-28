# Regression: R code for Chapter 17 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap17.r">here
# ------------------------------------------------------------

# Example 17.1. <a href="../wp-content/data/chapter17/chap17e1LionNoses.csv">Lion noses
# Estimate a linear regression and calculate predicted values and their uncertainty for the relationship between lion age and proportion of black on the lion nose. 
# Read and inspect the data.

lion <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17e1LionNoses.csv"))
head(lion)

# Basic scatter plot.

plot(ageInYears ~ proportionBlack, data = lion)

# Commands for a fancier scatter plot using more options (Figure 17.1-1) are here.

plot(ageInYears ~ proportionBlack, data = lion, pch = 16, col="firebrick", 
	las = 1, cex = 1.5, bty = "l", xlim = c(0,0.8), ylim = c(0,14), 
	ylab = "Age (years)", xlab = "Proportion black")

# Fit the linear regression to the data using least squares. Use lm, which was also used for ANOVA. Save the results into a model object, and then use other commands to extract the quantities we want.

lionRegression <- lm(ageInYears ~ proportionBlack, data = lion)

# Show the estimated linear regression coefficients: slope and intercept. The output of summary includes other valuable quantities, including standard errors and a t-test of zero regression slope. If you want only the regression coeffients without the rest of the data, use coef(lionRegression).

summary(lionRegression)

# Add the least squares regression line to the plot. The quick method is to use abline. If needed, redraw the scatter plot first. 

abline(lionRegression)

# Another way to draw the regression line is to generate predicted values and then connect them with a line (Figure 17.1-4). This has the advantage of more control - for example, we can make sure that the line doesn't fall outside the range of the data, avoiding extrapolation. Commands to accomplish this are here.

xpts <- range(lion$proportionBlack)
ypts <- predict(lionRegression, data.frame(proportionBlack = xpts))
lines(ypts ~ xpts, lwd = 1.5)

# Use the regression line for prediction. For example, here is how to predict mean lion age corresponding to a value of 0.50 of proportion black in the nose. Standard error of the predicted mean is included.

predict(lionRegression, data.frame(proportionBlack = 0.50), se.fit = TRUE)

# Calculate the residual and regression mean squares. All the mean squares are included in the anova table, which is generate using the anova command.

anova(lionRegression)

# Standard error of slope. The standard errors are provided in the output of the summary function.

summary(lionRegression)

# 95% confidence interval for slope.

confint(lionRegression)

# 95% confidence bands (Figure 17.2-1). If needed, redraw the scatter plot, including the regression line, before executing the commands below. The seq function below generates a sequence of 100 new x-values evenly spaced between the smallest and largest values of proportionBlack. The predict function calculates the corresponding predicted mean y-values on the regression line, as well as the lower and upper limits of the confidence intervals for the predicted mean y-values at each x-value (these are put into the data frame named ypt. Finally, the lines functions connect the consecutive lower limits of the confidence intervals with a line, and do the same for the consecutive upper limits, forming the bands.

xpt <- seq(min(lion$proportionBlack), max(lion$proportionBlack), 
	length.out = 100)
ypt <- data.frame( predict(lionRegression, 
	newdata = data.frame(proportionBlack = xpt), 
	interval = "confidence") )
lines(ypt$lwr ~ xpt, lwd = 1.5, lty = 2)
lines(ypt$upr ~ xpt, lwd = 1.5, lty = 2)

# 95% prediction intervals (Figure 17.2-1). If needed, redraw the scatter plot, including the regression line, before executing the commands below. The commands below are the same as those used to generate the confidence bands (see above), but here the lower and upper limits are confidence intervals for the prediction of a single individual y-value, rather than the mean value, corresponding to each x-value.

xpt <- seq(min(lion$proportionBlack), max(lion$proportionBlack), 
	length.out = 100)
ypt <- data.frame(predict(lionRegression, newdata = data.frame(proportionBlack = xpt), 
	interval = "prediction", level = 0.95))
lines(ypt$lwr ~ xpt, lwd = 1.5, lty = 3)
lines(ypt$upr ~ xpt, lwd = 1.5, lty = 3)

# ------------------------------------------------------------

# Example 17.3. <a href="../wp-content/data/chapter17/chap17e3PlantDiversityAndStability.csv">Prairie home campion
# Test the null hypothesis of zero regression slope. The data are from an experiment investigating the effect of plant species diversity on the stability of plant biomass production.
# Read and inspect the data.

prairie <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17e3PlantDiversityAndStability.csv"))
head(prairie)

# Log-transform stability and include the new variable in the data frame. Inspect the data frame once again to make sure the function worked as intended.

prairie$logStability <- log(prairie$biomassStability)
head(prairie)

# Scatter plot with regression line.

plot(logStability ~ nSpecies, data = prairie) 
prairieRegression <- lm(logStability ~ nSpecies, data = prairie)
abline(prairieRegression)

# Commands for a fancier scatter plot with more options (Figure 17.3-1) are here.

plot(logStability ~ nSpecies, data = prairie, bty = "l", col = "firebrick", 
	pch = 1, las = 1, cex = 1.5, xlim = c(0,16), xaxp = c(0,16,8), 
	xlab = "Species number treatment", ylab = "Log-transformed ecosystem stability")
prairieRegression <- lm(logStability ~ nSpecies, data = prairie)
xpts <- c(1,16)
ypts <- predict(prairieRegression, newdata = data.frame(nSpecies = xpts))
lines(ypts ~ xpts, lwd=1.5)
# t-test and 95% confidence interval of regression slope.

prairieRegression <- lm(logStability ~ nSpecies, data = prairie)
summary(prairieRegression)
confint(prairieRegression)

# ANOVA test of zero regression slope (Table 17.3-2).

anova(prairieRegression)

# R2. The R2 associated with the regression is part of the output of the summary command.

summary(prairieRegression)$r.squared

# ------------------------------------------------------------

# Figure 17.5-2. <a href="../wp-content/data/chapter17/chap17f5_2JuncoOutlier.csv">Tail feather outlier
# Examine the effect on an outlier on linear regression. The data are percentage of white in the tail feathers of the dark-eyed junco at sites at different latitudes.
# Read and inspect data.

junco <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f5_2JuncoOutlier.csv"))
head(junco)

# Scatter plot of all the data.

plot(percentWhiteTail ~ latitude, data = junco)

# Commands for a more elaborate scatter plot using more options (Figure 17.5-2) are here.

plot(percentWhiteTail ~ latitude, data = junco, pch = 16, col="firebrick", 
 	las = 1, cex = 1.5, bty = "l", xlab = "Latitude (degrees N)", 
 	ylab = "Percent white in tail")

# Add regression lines to the scatter plot, calculated both with and without the outlier. You may need to redraw the scatter plot using the above commands before executing the following functions.

juncoRegression <- lm(percentWhiteTail ~ latitude, data = junco)
abline(juncoRegression, col = "red", lwd = 2)
juncoRegressionNoOut <- lm(percentWhiteTail ~ latitude, 
	data = junco, subset = percentWhiteTail > 38)
abline(juncoRegressionNoOut, lwd = 2)

# ------------------------------------------------------------

# Figure 17.5-3. <a href="../wp-content/data/chapter17/chap17f5_3DesertPoolFish.csv">Desert pool fish
# Detect a nonlinear relationship graphically, using data on surface area of desert pools and the number of fish species present.
# Read and inspect data.

desertFish <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f5_3DesertPoolFish.csv"))
head(desertFish)

# Scatter plot.

plot(nFishSpecies ~ poolArea, data = desertFish)

# Commands for a fancier scatter plot are here.

plot(nFishSpecies ~ poolArea, data = desertFish, pch = 16, col = "firebrick", 
  	las = 1, cex = 1.5, bty = "l", xlab = expression(Area~(m^2)), 
  	ylab = "Number of species", ylim = c(0,6))

# Add regression line to previous scatter plot.

desertRegression <- lm(nFishSpecies ~ poolArea, data = desertFish)
abline(desertRegression)

# Add curve based on smoothing to previous scatter plot.

desertSmooth <-loess(nFishSpecies ~ poolArea, data = desertFish)
xpts <- seq(min(desertFish$poolArea), max(desertFish$poolArea), length.out = 100)
ypts <- predict(desertSmooth, new = data.frame(poolArea = xpts))
lines(xpts, ypts)

# Here is another method to produce a smooth curve in a scatter plot.

desertSmooth <-smooth.spline(desertFish$poolArea, desertFish$nFishSpecies, df = 4)
xpts <- seq(min(desertFish$poolArea), max(desertFish$poolArea), length.out = 100)
ypts <- predict(desertSmooth, xpts)$y
lines(xpts, ypts)

# ------------------------------------------------------------

# Figure 17.5-4 (left). <a href="../wp-content/data/chapter17/chap17f5_4BlueTitCapColor">Blue tit cap color
# Create a residual plot to check assumptions. The data are cap color of offspring and parents in a sample of the blue tit.
# Read and inspect the data.

capColor <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f5_4BlueTitCapColor.csv"))
head(capColor)

# Basic scatter plot with regression line.

plot(offspringColorScore ~ midparentColorScore, data = capColor)
capColorRegression <- lm(offspringColorScore ~ midparentColorScore, data = capColor)
abline(capColorRegression)

# Residual plot. Residuals are calculated from the model object created from the previous lm command, using the residuals command.

plot(residuals(capColorRegression) ~ midparentColorScore, data = capColor)
abline(c(0,0))

# Commands for a fancier residual plot using more options are here.

plot(residuals(capColorRegression) ~ midparentColorScore, data = capColor, 
	pch = 16, col = "firebrick", las = 1, cex = 1.5, bty = "l", 
	xlab = "Midparent color score", ylab = "Residuals")
abline(c(0,0))

# <hr class = "short">
# Figure 17.5-4 (right). <a href="../wp-content/data/chapter17/chap17f5_4CockroachNeurons.csv">Cockroach neurons
# Make a residual plot to check assumptions. The data are temperature and the firing rate of neurons in cockroach.
# Read and inspect the data.

roach <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f5_4CockroachNeurons.csv"))
head(roach)

# Scatter plot with regression line.

plot(rate ~ temperature, data = roach)
roachRegression <- lm(rate ~ temperature, data = roach)
abline(roachRegression)

# Residual plot.

plot(residuals(roachRegression) ~ temperature, data = roach)
abline(c(0,0))

# Commands for a more elaborate residual plot are here.

plot(residuals(roachRegression) ~ temperature, data = roach, 
	pch = 16, col = "firebrick", las = 1, cex = 1.5, bty = "l", 
	xlab = "Temperature", ylab = "Residuals")
abline(c(0,0))

# ------------------------------------------------------------

# Figure 17.6-3. <a href="../wp-content/data/chapter17/chap17f6_3IrisPollination.csv">Iris pollination
# Use a square root transformation to improve the fit to assumptions of linear regression. The data are number of pollen grains received and floral tube length of an iris species.
# Read and inspect data.

iris <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f6_3IrisPollination.csv"))
head(iris)

# Scatter plot with regression line.

plot(grainsDeposited ~ tubeLengthMm, data = iris)
irisRegression <- lm(grainsDeposited ~ tubeLengthMm, data = iris)
abline(irisRegression)

# Residual plot.

plot(residuals(irisRegression) ~ tubeLengthMm, data = iris)
abline(c(0,0))

# Commands for a residual plot with more options are here.

plot(residuals(irisRegression) ~ tubeLengthMm, data = iris, pch = 16,  
	col = "firebrick", las = 1, cex = 1.5, bty = "l",
	xlab = "Floral tube length (mm)", ylab = "Residuals")
abline(c(0,0))

# Square root transformation.

iris$sqRootGrains <- sqrt(iris$grainsDeposited + 1/2)

# Scatter plot using transformed data, with new regression line added.

plot(sqRootGrains ~ tubeLengthMm, data = iris)
irisRegressionSqrt <- lm(sqRootGrains ~ tubeLengthMm, data = iris)
abline(irisRegressionSqrt)

# Residual plot based on the transformed data.

plot(residuals(irisRegressionSqrt) ~ tubeLengthMm, data = iris)
abline(c(0,0))

# Instructions for a residual plot with more options are here.

plot(residuals(irisRegressionSqrt) ~ tubeLengthMm, data = iris,  
	pch= 16, col = "firebrick", las = 1, cex=1.5, bty = "l", 
	xlab = "Floral tube length (mm)", ylab = "Residuals")
abline(c(0,0))

# ------------------------------------------------------------

# Figure 17.8-1. <a href="../wp-content/data/chapter17/chap17f8_1IronAndPhytoplanktonGrowth.csv">Iron and phytoplankton growth
# Fit a nonlinear regression curve having an asymptote (Michaelis-Menten curve). The data are the relationship between population growth rate of a phytoplankton in culture and the concentration of iron in the medium.
# Read and examine data.

phytoplankton <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f8_1IronAndPhytoplanktonGrowth.csv"))
head(phytoplankton)

# Scatter plot.

plot(phytoGrowthRate ~ ironConcentration, data = phytoplankton)

# Instructions for a scatter plot with more options are here.

plot( phytoGrowthRate ~ ironConcentration, data = phytoplankton, pch = 16, 
	col = "firebrick", las = 1, cex = 1.5, bty = "l", 
	ylab = "Growth rate (no./day)",
	xlab = expression(paste("Iron concentration (", mu, "mol)")) )

#   	
# Fit a Michaelis-Menten curve to the phytoplankton data using the nls (nonlinear least squares). To fit the curve, provide a formula that also includes symbols (letters) for the parameters to be estimated. In the following function, we use "a" and "b" to indicate the parameters of the Michaelis-Menten curve we want to estimate. The function includes an argument where we must provide an initial guess of parameter values. The value of the initial guess is not so important -- here we choose a=1 and b=1 as initial guesses. The first function below carried out the model fit and save the results in an R object named phytoCurve.

phytoCurve <- nls(phytoGrowthRate ~ a*ironConcentration / ( b+ironConcentration), 
	data = phytoplankton, list(a = 1, b = 1))

# Obtain the parameter estimates using the summary command to , including standard errors and t-tests of null hypotheses that parameter values are zero.

summary(phytoCurve)

# Add the nonlinear regression curve to scatter plot. If necessary, redraw the scatter plot before issuing the following commands.

xpts <- seq(min(phytoplankton$ironConcentration), 
	max(phytoplankton$ironConcentration), length.out = 100)
ypts <- predict(phytoCurve, new = data.frame(ironConcentration = xpts))
lines(xpts, ypts)

# Many of the functions that can be used to extract results from a saved lm object work in the same way when applied to an nls object, such as predict, residuals, and coef.
# ------------------------------------------------------------

# Figure 17.8-2. <a href="../wp-content/data/chapter17/chap17f8_2PondPlantsAndProductivity.csv">Pond plants and productivity
# Fit a quadratic curve to the relationship between the number of plant species present in ponds and pond productivity.
# Read and examine data.

pondProductivity <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f8_2PondPlantsAndProductivity.csv"))
head(pondProductivity)

# Scatter plot.

plot(species ~ productivity, data = pondProductivity)

# Commands for a fancier scatter plot are here.

plot(species ~ productivity, data = pondProductivity, pch = 16, 
	col = "firebrick", las = 1, cex = 1.5, bty = "l", 
	ylab = "Number of species", xlab = "Productivity (g/15 days)" )

#   	
# Fit a quadratic curve to the data. Here, the single variable productivity in the data frame is included in the formula both as itself and as the squared term, productivity2. To make the squared term work, we need to wrap the term with I(). The results of the model fit are saved in an R object productivityCurve.

productivityCurve <- lm(species ~ productivity + I(productivity^2), 
	data = pondProductivity)

# Show estimates of the parameters of the quadratic curve (regression coefficients) are obtained as follows, along with standard errors and t-tests.

summary(productivityCurve)

# Add quadratic regression curve to scatter plot. If necessary, redraw the previous scatter plot before issuing the following commands.

xpts <- seq(min(pondProductivity$productivity), max(pondProductivity$productivity), 
	length.out = 100)
ypts <- predict(productivityCurve, new = data.frame(productivity = xpts))
lines(xpts, ypts)

# ------------------------------------------------------------

# Example 17.8. <a href="../wp-content/data/chapter17/chap17e8ShrinkingSeals.csv">Incredible shrinking seal
# Fit a formula-free curve (cubic spline) to the relationship between body length and age for female fur seals.
# Read and inspect data

shrink <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17e8ShrinkingSeals.csv"))
head(shrink)

# Scatter plot.

plot(length ~ ageInDays, data = shrink, pch = ".")

# Commands for a fancier scatter plot with more options are here.

plot(jitter(length, factor = 2) ~ ageInDays, data = shrink, pch = ".", 
	col = "firebrick", las = 1, bty = "l", ylab = "Body length (cm)",
	xlab = "Female age (days)")

#   	
# Fit a cubic spline. The argument df stands for effective degrees of freedom, which allows you to control how complicated the curve should be. The simplest possible curve is a straight line, which has df=2 (one for slope and another for intercept). More complex curves require more df. Here we fit a very complicated curve.

shrinkCurve <- smooth.spline(shrink$ageInDays, shrink$length, df = 40)

# Add curve to scatter plot. If needed, replot the scatter plot before issuing the commands below.

xpts <- seq(min(shrink$ageInDays), max(shrink$ageInDays), length.out = 1000)
ypts <- predict(shrinkCurve, xpts)$y
lines(xpts, ypts)

# ------------------------------------------------------------

# Figure 17.9-1. <a href="../wp-content/data/chapter17/chap17f9_1GuppyColdDeath.csv">Guppy cold death
# Fit a logistic regression to the relationship between guppy mortality and duration of exposure to a temperature of 5 degrees C.
# Read and inspect the data.

guppy <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17f9_1GuppyColdDeath.csv"))
head(guppy)

# Draw a frequency table of mortality at different exposure times.

table(guppy$mortality, guppy$exposureDurationMin, 
	dnn = c("Mortality","Exposure (min)"))

# Scatter plot of the data.

plot(mortality ~ jitter(exposureDurationMin), data = guppy)

# Commands for a fancier scatter plot, with more options, are here.

plot(jitter(mortality, factor = 0.1) ~ jitter(exposureDurationMin, factor = 1),  
	data = guppy, col = "firebrick", las = 1, bty = "l", ylab = "Mortality",
	xlab = "Duration of exposure (min)")

#   	
# Fit a logistic regression.

guppyGlm <- glm(mortality ~ exposureDurationMin, data = guppy,
	family = binomial(link = logit))

# Add logistic regression curve to scatter plot.

xpts <- seq(min(guppy$exposureDurationMin), max(guppy$exposureDurationMin), 
	length.out = 100)
ypts <- predict(guppyGlm, newdata = data.frame(exposureDurationMin = xpts), 
	type = "response")
lines(xpts, ypts)

# Table of regression coefficients, with parameter estimates (Table 17.9-2).

summary(guppyGlm)

# 95% confidence intervals for parameter estimates. It is necessary to load the MASS package first. 

library(MASS)
confint(guppyGlm)

# Predict probability of mortality (mean mortality) for a given x-value, 10 min duration, including standard error of prediction.

predict(guppyGlm, newdata = data.frame(exposureDurationMin = 10),
	type = "response", se.fit = TRUE)

# Estimate the LD50, the dose at which half the individuals are predicted to die from exposure.

library(MASS)
dose.p(guppyGlm, p = 0.50)

# Analysis of deviance table, with a of the null hypothesis of zero slope (Table 17.9-3).

anova(guppyGlm, test = "Chi")

