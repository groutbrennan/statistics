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
expectation <- function(x,y) {
  print(sum(x * y))
}
expectation(0.8^2, 0.2)

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
number_of_attempts_to_be_successful <- function(p) {
  print(1 / p)
}
number_of_attempts_to_be_successful(0.4)
## 2.5

## The variance of the number of attempts.
## Var(X) = q/p^2
variance_number_of_attempts <- function(q,p) {
  print(q/p^2)
}
variance_number_of_attempts(0.6, 0.4)
## 3.75

########################## Binomial Distribution #####################
## P(X = r) = nCr * p^r * q^n-r
binomial_distribution <- function(n, r, p, q) {
  c <- factorial(n) / factorial(n-r)
  pwr <- n-r
  p_q <- p^r * q^pwr
  bd <- c * p_q
  print(bd)
}
binomial_distribution(3,1, 0.25, 0.75)
## 0.422