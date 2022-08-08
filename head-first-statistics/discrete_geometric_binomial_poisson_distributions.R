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

##################### Epectation Variance ###################################
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
e_v_df %>% 
  rename(variance_expectation = expectation_variance.p_equal_x..x_ten_col.) %>% 
  mutate(running_sum_total = cumsum(variance_expectation))



