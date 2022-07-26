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

## Restaurant offers two menus, one for the weekdays and the other for the weekends.
## Each menu offers four set prices, and the probability distributions for the amount someone pays is:
## Weekday menu:
weekday_x <- c(10, 15, 20, 25)
weekday_y <- c(0.2, 0.5, 0.2, 0.1)

## Weekend menu:
weekend_x <- c(15, 20, 25, 30)
weekend_y <- c(0.15, 0.6, 0.2, 0.05)

print(25 * expectation(weekday_x, weekday_y)) ## 400
print(20 * expectation(weekend_x, weekend_y)) ## 415
## Expect 20 people who will eat on the weekend will pay more than 25 people who eat on the weekday.

## Find how much Sam tends to spend at each restaurant.
restaurant_a_x <- c(20, 30, 40, 45)
restaurant_a_y <- c(0.3, 0.4, 0.2, 0.1)

restaurant_b_x <- c(10, 15, 18)
restaurant_b_y <- c(0.2, 0.6, 0.2)

## Restaurant expectations.
res_a <- expectation(restaurant_a_x, restaurant_a_y) ## 30.5
res_b <- expectation(restaurant_b_x, restaurant_b_y) ## 14.6
## Difference between in restaurant prices.
print(res_a - res_b) ## 15.9

## Restaurant variance.
res_var_a <- variance(restaurant_a_x, restaurant_a_y)
res_var_b <- variance(restaurant_b_x, restaurant_b_y)
## Variance between restaurants.
print(res_var_a + res_var_b) ## 78.89
