## Probability independent observations.

## Probability distribution of X as x and y vectors.
x <- c(-0.5, 1.5, 4.5, 9.5)
y <- c(0.8, 0.1, 0.07, 0.03)

## Expectation of a probability distribution.
## nE(X).
expectation <- function(x,y) {
  print(sum(x * y))
  }
expectation(x, y)

## Variance using the expectation (mean) value.
## Var(X) = E(X-mean)2P(X=x)
## e = expectation
## x and y are probability distribution vectors.
variance <- function(x,y) {
  e <- sum(x * y)
  diff_sq <- (x-e) ^ 2
  print(sum(diff_sq * y))
} 
variance(x, y)

## Standard Deviation is square root of the variance.
standard_deviation <- function(x) {
  print(sqrt(x))
}
standard_deviation(variance(x,y))
