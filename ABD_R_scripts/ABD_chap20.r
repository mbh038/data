# Likelihood: R code for Chapter 20 examples
# Download the R code on this page as a single file <a href="../wp-content/rcode/chap20.r">here
# ------------------------------------------------------------

# Example 20.3 <a href="../wp-content/data/chapter20/chap20e1UnrulyPassengers.csv">Unruly passengers
# Maximum likelihood estimation of the proportion of parasitic wasp individuals that choose the mated butterflies in a choice test. Below, we also apply the log-likelihood ratio test to these data.
# Read and inspect the data.

wasps <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter20/chap20e3UnrulyPassengers.csv"))
head(wasps)

# Frequency table of the data.

table(wasps)

# The likelihood and log-likelihood of p = 0.5.

dbinom(23, size = 32, p = 0.5)
dbinom(23, size = 32, p = 0.5, log = TRUE)

# The log-likelihood of a range of values of p (Table 20.3-1). 

proportion <- seq(0.1, 0.9, by = 0.1)
logLike <- dbinom(23, size = 32, p = proportion, log = TRUE)
data.frame(Proportion = proportion, Loglikelihood = logLike)

# The log-likelihood calculated using a narrower range of values for p (Table 20.3-2). The additional quantity dlogLike is the difference between each likelihood and the maximum.

proportion <- seq(0.4, 0.9, by = 0.01)
logLike <- dbinom(23, size = 32, p = proportion, log = TRUE)
dlogLike <- logLike - max(logLike)
data.frame(Proportion = proportion, Loglikelihood = logLike, diffLogLike = dlogLike)

# The maximum likelihood estimate.

pHat <- proportion[logLike == max(logLike)]
pHat

# The log-likelihood curve (Figure 20.3-1).

plot(logLike ~ proportion, lwd = 2, las = 1, type = "l", bty = "l", 
	xlab = "Hypothesized proportion, p", ylab = "Log-likelihood")
abline(logLike[proportion == pHat], 0, lty = 2)

# The likelihood-based 95% confidence interval.

lower <- max( proportion[proportion <= pHat & logLike <= max(logLike) - 1.92] )
upper <- min( proportion[proportion >= pHat & logLike <= max(logLike) - 1.92] )
c(lower = lower, upper = upper)

# <hr class = "short">
# The log-likelihood ratio test to test the null hypothesis that the population proportion of wasps choosing mated female butterflies is 0.5. G is the test statistic and P is the P-value.

G <- 2 * ( dbinom(23, size = 32, p = pHat, log = TRUE)
	 - dbinom(23, size = 32, p = 0.5, log = TRUE) )
G
P <- 1 - pchisq(G, 1)
P

# ------------------------------------------------------------

# Example 20.4 <a href="../wp-content/data/chapter20/chap20e4ConservationScoop.csv">Conservation Scoop
# Maximum likelihood estimation of elephant population size using mark-recapture data.
# Read and inspect the data on the 74 individuals collected in the second sample.

elephant <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter20/chap20e4ConservationScoop.csv"))
head(elephant)

# Frequency table of the 74 individuals in the second sample.

table(elephant)

# Quantities needed for the likelihood calculations. 

m <-  27     # size of first sample (total marked individuals)
n2 <- 74     # size of second sample
Y <-  15     # number of recaptures in second sample

# Choose a range of values of N to try. N is the (unknown) total number of individuals in the population. N must be at least n2 + m - Y (86, in the current example).

N <-  90:250 # The values of N to try

# Likelihood and log-likelihood.

like <-     choose(m, Y) *  choose(N - m, n2 - Y) /  choose(N, n2)
logLike <- lchoose(m, Y) + lchoose(N - m, n2 - Y) - lchoose(N, n2)

# Likelihood and log-likelihood can also be obtained using R's built-in function for the hypergeometric distribution.

like <-    dhyper(Y, m, N - m, n2)
logLike <- dhyper(Y, m, N - m, n2, log = TRUE)

# The maximum likelihood estimate of elephant population size.

Nhat <- N[logLike == max(logLike)]
Nhat

# The log-likelihood curve (Figure 20.4-1).

plot(logLike ~ N, lwd = 2, las = 1, type = "l", bty = "l", 
	xlab = "Population size estimate, N", ylab = "Log-likelihood")
abline(logLike[N == Nhat], 0, lty = 2)

# The likelihood-based 95% confidence interval for elephant population size.

lower <- max(N[N <= Nhat & logLike <= max(logLike) - 1.92])
upper <- min(N[N >= Nhat & logLike <= max(logLike) - 1.92])
c(lower = lower, upper = upper)

