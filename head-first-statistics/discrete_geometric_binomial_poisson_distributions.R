library(ggplot2)
library(tidyverse)
library(dplyr)
## Geometric distribution calculation can help us figure out the probability of something succeeding.
## p stands for probability to succeed. 
## q stands for probability to failure.
## r stands for the number of trials we need to get the first success.
## P(X=r) = q^r-1 p

geometric_distribution <- function(q, p, r) {
  r_1_diff <- r - 1
  gd <- q^r_1_diff * p 
  print(gd)
}

geometric_distribution(0.8, 0.2, 100)
## 0.000000000005092

geometric_distribution(0.8, 0.2, 20)
geometric_distribution(0.8, 0.2, 3)

## Geometric distribution with inequalities.
## P(X > r) = q^r

## Expectation of a probability distribution.
## nE(X).
expectation_sum <- function(x,y) {
  print(sum(x * y))
}
expectation_sum(0.8^2, 0.2)

## xP(X=x) function - page 280 in headfirst book.
xP_x_equals_X <- function(p,pw,q,x) {
  pq <- p^pw * q
  px <- pq * x
  print(px)
}
xP_x_equals_X(0.8,2,0.2,3)

## Pattern of expectations for the geometric distribution.
x_col <- c(1, 2, 3, 4, 5, 6, 7, 8)
equal_x <- c(0.2, 0.32, 0.384, 0.4096, 0.4096, 0.393216, 0.3670016, 0.33554432)
less_equal_x <- c(0.2, 0.52, 0.904, 1.3136, 1.7232, 2.116416, 2.4834176, 2.81894608)
gd_pattern <- data.frame(x_col, equal_x, less_equal_x)

head(gd_pattern)

## Plot out pattern of expectations data frame to find the peak.
ggplot(data = gd_pattern, aes(x=less_equal_x, y=equal_x)) + 
  geom_line() +
  geom_point()

##################### Expectation Variance ###################################
## Find an expression for the variance of the geometric distribution.
x_ten_col <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
p_equal_x <- c(0.2, 0.16, 0.128, 0.1024, 0.08192, 0.065536, 0.0524288, 0.04194304, 0.033554432, 0.0268435456)

## Find expectation variance.
expectation_variance <- function(r,x) {
  total <- r * x^2
  print(total)
}
## Turn 
e_v_df <- data.frame(expectation_variance(p_equal_x, x_ten_col))

head(e_v_df)

## Rename the first column then find the running sum using cumsum calculation.
e_rt <- e_v_df %>% 
  rename(variance_expectation = expectation_variance.p_equal_x..x_ten_col.) %>% 
  mutate(running_sum_total = cumsum(variance_expectation))

head(e_rt)

ggplot(data = e_rt, aes(x=running_sum_total, y=variance_expectation)) +
  geom_line()

############################ Geometric Distribution Calculations ##################
## Definition: Use Geometric Distribution if you're interested in how many trials you'll need before you have your first success.

## Question 1
## The probability that another snowboarder will make it down the slope without falling over is 0.4.
## Your job is to play like you're the snowboarder and work out the following probabilities for your slope success.

## Probability that you will be successful on your second attempt while failing on your first.
## P(X = 2) = p * q
successful_second_attempt <- function(p, q) {
  probability <- p * q
  print(probability)
}
successful_second_attempt(0.4, 0.6)
## 0.24

## The probability that you will be successful in 4 attempts or fewer.
## P(X <= 4) = 1 - q^4
p_successful_four_or_fewer <- function(q) {
  probability <- 1 - q^4
  print(probability)
}
p_successful_four_or_fewer(0.6)
## 0.8704

## The probability that you will need more than 4 attempts to be successful.
## P(X > 4) = q^4
p_successful_more_than_4_attemtps <- function(q) {
  print(q^4)
}
p_successful_more_than_4_attemtps(0.6)
## 0.1296

## The number of attempts you expect you'll need to make before being successful.
## E(X) = 1/p
expectation_e_x <- function(p) {
  print(1 / p)
}
expectation_e_x(0.4)
## 2.5

## The variance of the number of attempts.
## Var(X) = q/p^2
variance_q_p <- function(q,p) {
  print(q/p^2)
}
variance_q_p(0.6, 0.4)
## 3.75

########################## Binomial Distribution #####################
## Definition: Use Binomial Distribution if you have a fixed number of trials and you want to know the probability of getting a certain number of successes.
## P(X = r) = nCr * p^r * q^n-r
binomial_distribution <- function(n, r, p, q) {
  c <- factorial(n) / (factorial(r) * factorial(n-r))
  pwr <- n-r
  p_q <- p^r * q^pwr
  bd <- c * p_q
  print(bd)
}
binomial_distribution(3, 1, 0.25, 0.75)
## 0.422
## 42% chance I get one question correct out of 3 questions.

## Binomial expectation.
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

##################### Scenario Exercise for Binomial Distribution ###############################
## In the latest round of a game, there are 5 questions.
## The probability of getting a successful outcome in a single trial is 0.25.

## Question 1: What's the probability of getting exactly 2 questions right?
binomial_distribution(5, 2, 0.25, 0.75)
## 0.264

## Question 2: What's the probability of getting exactly 3 questions right?
binomial_distribution(5, 3, 0.25, 0.75)
## 0.0879

## Question 3: What's the probability of getting two or three questions right?
question_3 <- binomial_distribution(5, 2, 0.25, 0.75) + binomial_distribution(5, 3, 0.25, 0.75)
print(question_3)
## 0.3516

## Question 4: What's the probability of getting no questions right?
binomial_distribution(5, 0, 0.25, 0.75)
## 0.2373

## Question 5: What are the expectation and variance?
binomial_expectation(5, 0.25)
## 1.25

binomial_variance(5, 0.25, 0.75)
## 0.9375

############################ Poisson Distribution #######################
## Definition: Individual events occur at random and independently in a given interval.
## You know the mean number of occurrences in the interval or the rate of occurrences, and it's finite.
## The mean number of occurrences is represented by the Greek letter Lambda.

## P(X = r) = e-lambda lambda * r/r!
## NOTE: exp() is the built in exponential value for e in the following equation.
poisson_dsitribution <- function(lambda, r) {
  numerator <- exp(-lambda) * lambda^r
  pd <- numerator / factorial(r)
  print(pd)
}
poisson_dsitribution(2, 3)
## 0.180

poisson_dsitribution(0.25, 1)

## Scenario: The mean number of times the popcorn machine breaks down per week is 3.4.
## Question 1: Whats the probability of the popcorn machine not malfunctioning next week?
poisson_dsitribution(3.4, 0)
## 0.033

## Question 2: What's the probability of the popcorn machine malfunctioning three times next week?
poisson_dsitribution(3.4, 3)
## 0.218

## Question 3: What's the expectation and variance of the popcorn machine malfunctioning?
## 3.4 - For Poisson Distribution the expectation and variance are equal to Lambda.

## Question 4: What is the probability distribution of the popcorn machine and the drink machine not breaking down?
## Popcorn machine: lambda = 3.4
## Drink machine: lambda = 2.3
popcorn_machine <- 3.4
drink_machine <- 2.3
p_d_lambda <- popcorn_machine + drink_machine
poisson_dsitribution(p_d_lambda, 0)
## 0.0033

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

#############################################################
####################### Quiz ################################
#############################################################

## QUESTION 1: 
## A man is bowling. The probability of him knocking all the pins over is 0.3.
## If he has 10 shots, what's the probability he'll knock all the pins down less than three times?
## Which distribution, expectation, and variance they follow.

## Expectation
## E(X) = np
bowling_expectation <- 10 * 0.3
bowling_expectation
## 3

## Variance
## Var(X) = npq
bowling_variance <- 10 * 0.3 * 0.7
bowling_variance
## 2.1

## Binomial Distribution
## P(X = r) = nCr * p^r * q^n-r.
## solve 0 times he knocks pins down.
zero_times <- binomial_distribution(10, 0, 0.3, 0.7)
## 0.028

## Solve 1 times he knocks all pins down.
one_time <- binomial_distribution(10, 1, 0.3, 0.7)
## 0.121

## Solve 2 times he knocks all pins down.
two_times <- binomial_distribution(10, 2, 0.3, 0.7)
## 0.233

## Solve less than 3 times he knocks the pins down.
less_than_three_times <- zero_times + one_time + two_times
less_than_three_times
## 0.382

## QUESTION 2: 
## On average, 1 bus stops at a certain point every 15 minutes.
## What's the probability that no buses will turn up in a single 15 minute interval?
## Expectation
## E(X) = lambda
bus_expectation <- 1

## Variance.
## Var(X) = lambda
bus_variance <- 1

## General probability using Poisson distribution.
## P(X = 0) = e^-1 * 1^0 / factorial(0)
poisson_dsitribution(1, 0)
## 0.368

## QUESTION 3: 
## 20% of cereal packets contain a free toy.
## What's the probability you'll need to open fewer than 4 cereal packets before finding your first toy?
## Expectation
## E(X) = 1/p
expectation_e_x(0.2)
## 5

## Variance.
## Var(X) = q/p^2
variance_q_p(0.8, 0.2)
## 20

## General probability.
## P(X <= r) = 1 - q^r
probability_q_r <- function(q, r) {
  print(1-q^r)
}
probability_q_r(0.8, 3)
## 0.488

probability_q_r(0.97, 33)
expectation_e_x(0.03)
variance_q_p(0.97, 0.03)
33/4
8*275
