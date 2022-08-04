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

## Finding the probability if the arrangements are in a circle.
## Fix the position of one number (n-1)! then calculate the number of ways for the rest of the numbers.
circle_arrangement_p <- function (x) {
  fix_n <- x - 1
  n <- factorial(fix_n)
  p <- 1 / n
  print(p)
}

circle_arrangement_p(10)

## Arranging by type.
## So if there are 3 horses and 3 zebras in a race.
## Calculate how many different orderings there are of horses and zebras.
type_arrangement <- function(n, x) {
  p <- factorial(n) / prod(factorial(x))
  print(p)
}

horse_zebra <- c(3, 3)
type_arrangement(6, horse_zebra)
## 20 possible ways of winning.
## 1/20 chance of winning.

## A race between 3 horses, 2 zebras, 5 camels.
## How many ways are there of finishing the race if we're just interested in species of animal in each position?
horse_zebra_camel <- c(3, 2, 5)

type_arrangement(10, horse_zebra_camel)
## 2520 possible ways of winning.

## What the probability that all 5 camels finish the race consecutively if each animal has an equal chance of winning?
## Find the number of ways in which the 5 camels can finish the race together.
## To do this, we class the 5 camels as one single object (1).
horse_zebra_camelWins <- c(3,2,1)
type_arrangement(6, horse_zebra_camelWins)
## 60 possible ways a camel can win.

## Find the probability of all camels finishing together. 
## Divide 60 possible ways the camel can win over the amount of possible ways the horse_zebra_camel can win: 2520.
camel_p <- type_arrangement(6, horse_zebra_camelWins)
horse_zebra_camel_p <- type_arrangement(10, horse_zebra_camel)
camels_finishing_together_p <- camel_p / horse_zebra_camel_p
camels_finishing_together_p
## 0.02380952 probability of all camels finishing together.

#################################################################
################### Permutations ################################
#################################################################

## Permutation: is the number of ways in which you can choose objects from a pool, and hwere the order in which you choose them counts.
## It's a lot more specific than a combination as you want to count the number of ways in which you fill each position.
## Permutation: Order Matters.
## nPr = n!/(n-r)

permutation <- function(n, r) {
  n_r_diff <- n - r
  p <- factorial(n) / factorial(n_r_diff)
  print(p)
}
permutation(20, 3)
## 6,840 permutations for filling the first three places in the race.

#################################################################
################### Combinations ################################
#################################################################

## Combination: is the number of ways in which you can choose objects from a pool, without caring about the exact order in which you choose them.
## It's a lot more general than a permutation as you don't need to know how each position has been filled.
## It's enough to know which objects have been chosen.
## Combination: Order Doesn't Matter.
## nCr = n! / r!(n-r)

combinations <- function(n, r) {
  n_r_diff <- n - r
  fac_nr_diff <- factorial(r) * factorial(n_r_diff)
  c <- factorial(n) / fac_nr_diff
  print(c)
}
## Find the combinations of the top 3 horses winning out of 20 horses.
combinations(20, 3)
## 1140
## There are 6,840 permutations for filling the first three places in the race.
## If you aren't concerned about the order, there are 1,140 combinations.
