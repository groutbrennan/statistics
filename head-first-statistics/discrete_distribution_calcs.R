## The following are geometric distribution, binomial distribution, and Poisson distribution definitions and calculations.

###############################################
########### Geometric Distribution ############
###############################################
# Definition:
## The geometric distribution applies when you run a series of independent trials, there can be either a success or failure for each trial.
## The main thing you are interested in is how many trials are needed in order to get your first success.

## If the conditions are met for the geometric distribution.
## X is the number of trials needed to get the first successful outcome.
## p is the probability of success in a trial.

# X ~ Geo(p)

## The following probabilities apply if X ~ Geo(p):
# P(X = r) = pq^r-1
gd_equals <- function(p, q, r) {
  r_1_diff <- r - 1
  gd <- p * q^r_1_diff
  print(gd)
}
## 3% of sketched foil cards in each mtg collector booster pack.
## WHat is the probability of opening a sketched foil mtg card on the 4th attempt.
gd_equals(0.03, 0.97, 4)
## 0.02738 probability you'll open a sketched foil mtg card on your fourth attempt from a collector booster pack.

# P(X > r) = qr
gd_greater_than <- function(q, r) {
  gd <- q^r
  print(gd)
}
gd_greater_than(0.97, 4)
## 0.8852 probability you'll need to open more than 2 collector booster packs in order to open a sketched foil metg card.

# P(X <= r) = 1- q^r
gd_less_than_equal <- function(q, r) {
  gd <- 1 - q^r
  print(gd)
}
gd_less_than_equal(0.97, 4)
## 0.114 probability you'll open a sketched mtg card on the 4th collector booster pack.

## Expectation.
# E(X) = 1/p
gd_expectation <- function(p) {
  gd_e <- 1/p
  print(gd_e)
}
gd_expectation(0.03)
## 33.33 expectation to open collector booster packs before finding a sketched foil mtg card.

## Variance.
# Var(X) = q/p^2
gd_variance <- function(p, q) {
  gd_var <- q / p^2
  print(gd_var)
}
gd_variance(0.03, 0.97)
## 1077.778 variance dispersion.

##############################################
########### Binomial Distribution ############
##############################################
# Definition: The binomial distribution applies when you run a series of finite independent trials.
## There can be either a success or failure for each trial.
## The main thing you are interested in is the number of successes in the n independent trials.
## If the conditions are met for the binomial distribution.
## X is the number of successful outcomes out of n trials.
## p is the probability of success in a trial, then:
## X ~ B(n, p)
## P(X = r) = nCr * p^r * q^n-r
binomial_distribution_calc <- function(n, r, p, q) {
  c <- factorial(n) / (factorial(r) * factorial(n-r))
  pwr <- n-r
  p_q <- p^r * q^pwr
  bd <- c * p_q
  print(bd)
}
binomial_distribution_calc(3, 1, 0.25, 0.75)
## 0.421875 chance of getting the correct answer.

## Expectation.
## E(X) = np
binomial_expectation <- function(n,p) {
  print(n * p)
}
binomial_expectation(3, 0.25)
## 0.75

## Binomial Variance.
## Var(X) = npq
binomial_variance <- function(n,p,q) {
  print(n * p * q)
}
binomial_variance(3,0.25, 0.75)
## 0.5625

#############################################
########### Poisson Distribution ############
#############################################
# Definition: The Poisson distribution applies when individual events occur at random and independently in a given interval.
## You know the mean number of occurrences in the interval or the rate of occurrences and this is finite.
## You want to know the number of occurrences in a given interval.

## If the conditions are met for the Poisson distribution:
## X is the number of occurrences in a particular interval.
## Lambda is the rate of occurrences, then:
## X ~ Po(lambda)

## P(X = r) = e-lambda lambda * r/r!
## NOTE: exp() is the built in exponential value for e in the following equation.
poisson_dsitribution <- function(lambda, r) {
  numerator <- exp(-lambda) * lambda^r
  pd <- numerator / factorial(r)
  print(pd)
}
poisson_dsitribution(2, 3)
## 0.180

## If X ~ Po(lambdax), Y ~ Po(lambday) and X and Y are independent:
## Example: What is the probability distribution of the popcorn machine and the drink machine not breaking down?
## Popcorn machine: lambda = 3.4
## Drink machine: lambda = 2.3
popcorn_machine <- 3.4
drink_machine <- 2.3
p_d_lambda <- popcorn_machine + drink_machine
poisson_dsitribution(p_d_lambda, 0)
## 0.0033

## If X ! B(n, p) where n is the large and p is the small, you can approximate it with X ~ Po(np).
## Poisson in disguise
## Sometimes its simpler to use the Poisson distribution than the binomial.
## An example if you had to calculate a binomial probability where n is 50, which would be difficult to calculate the factorial value.

## Example:
## A student needs to take an exam and needs to guess the answer to each question, and the probability of gett a question right is 0.05.
## There are 50 questions, and the probability of getting an answer right is 0.05.
## lambda = np
question_lambda <- 50 * 0.05
## Whats the probability he'll get 5 questions right?
poisson_dsitribution(question_lambda, 5)
#0.067
