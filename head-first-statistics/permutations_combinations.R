## Spread function calculates the spread given the possibilities, bet, and odds. 
## If there are 6 different ways to win, then the spread is ratio is 7:1.
## The bet is $500.
spread <- function(r1, r2, bet, odds) {
  x <- c((bet-(bet*2)), (bet*odds))
  probability <- c((r1-1)/r1, (r2/r1))
  print(sum(x * round(probability, digits = 3)))
}

## Factorial takes all of the numbers from n down to 1.
## Factorial of 3 = 3*2*1 = 6.
spread(factorial(3), 1, 500, factorial(3)+1)
## Expect to win $168.

