## Spread function calculates the spread given the possibilities, bet, and odds. 
## If there are 6 different ways to win, then the spread is ratio is 7:1.
## The bet is $500.
spread <- function(n, bet, odds) {
  x <- c((bet-(bet*2)), (bet*odds))
  probability <- c((n-1)/n, (1/n))
  winnings <- sum(x * round(probability, digits = 3))
  print(winnings)
}

## Factorial takes all of the numbers from n down to 1 called arrangements.
## Factorial of 3 = 3*2*1 = 6.
spread(factorial(3), 500, factorial(3)+1)
## Expect to win $168.

## Paula has just been reminded that the first three numbers in some arrangement of the numbers: 1,2,3.
## The last four numbers is some arrangement of the numbers: 4,5,6,7.
## Whats the probability of getting the right telephone number?
## First set has 3 numbers.
## Second set has 4 numbers.
telephone_number_p <- function(x, y) {
  n1 <- factorial(x)
  n2 <- factorial(y)
  p <- 1 / (n1 * n2)
  print(p)
}

telephone_number_p(3,4)
## 0.006944444

## Social Security Numbers have 3 set of number arrangements: 3, 2, 4.
## Find the probability of guessing the correct Social Security Number.
ssn_p <- function(x, y, z) {
  n1 <- factorial(x)
  n2 <- factorial(y)
  n3 <- factorial(z)
  p <- 1 / (n1 * n2 * n3)
  print(p)
}

ssn_p(3, 2, 4)
## 0.003472222

