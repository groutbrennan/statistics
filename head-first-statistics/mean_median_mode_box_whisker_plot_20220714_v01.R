library(ggplot2)

## Mean
mean(x, trim=0, na.rm=FALSE, ...)

x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

mean_result <- result.mean <- mean(x, trim=0.3)
print(mean_result)

## Median
median(x, na.rm = FALSE)
xm <- c(1,2,3,4,5,6,7,8)

median.result <- median(xm)
print(median.result)

## Mode
## Create function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)

result <- getmode(v)
print(result)

## Variance function.
xvar <- c(1,2,9)
var(xvar)
## Standard Deviation function.
sd(xvar)

## Find the variance and standard deviation of the following vectors.
seven <- c(1,2,3,4,5,6,7)
var(seven)
sd(seven)

six <- c(1,2,3,4,5,6)
var(six)
sd(six)

## Standard score function using x, mean, standard deviation calculations.
standard_score <- function(i, x) {
  m <- result.mean <- mean(i, trim=0.3)
  sd <- sd(i)
  x_mean_diff <- x-m
  score <- x_mean_diff/sd
  print(score)
}

## Find standard scores for the following basketball players.
playerone <- c(7,9,9,10,10,10,10,11,11,13)
standard_score(playerone, 12)

playertwo <- c(7,8,9,9,10,10,11,11,12,13)
standard_score(playertwo, 15)

playerthree <- c(3,3,6,7,7,10,10,10,11,13,30)
standard_score(playerthree, 10)

## Calculate Quartile
test = c(3,3,6,7,7,10,10,10,11,13,30)

## Find the position of the lower quartile. 
## If the answer is not an interger, then round up.
lower_quartile <- length(test)/4
as.integer(ceiling(lower_quartile))

## Find the position of the upper quartile.
## If the answer is not an interger, then round up.
upper_quartile <- 3*length(test)/4
as.integer(ceiling(upper_quartile))

## Calculate percentiles.
## Start by lining all line your values in ascending order.
## Calculate k(n/100)
## Find the 10th percentile where k = 10.
percentile <- 10 * length(test)/100
as.integer(ceiling(percentile))


## Find the median quickly using the summary function.
summary(test)

## Create a data frame from the test vector.
test_df <- data.frame(test)
head(test_df)

## Create a box and whisker plot to visualize the upper bound, lower bound, upper quartile, lower quartile, interquartile range, median.
ggplot(data = test_df, aes(x=test)) +
  geom_boxplot()

## Player 1 and player 2 basketball scores as vectors.
score <- c(0,8,9,9,10,10,10,11,11,12,8,10,10,10,10,10,10,10,10,12)
player <- c("player1", "player1", "player1", "player1", "player1", "player1", "player1", "player1", "player1", "player1", "player2",  "player2",  "player2",  "player2",  "player2",  "player2",  "player2",  "player2",  "player2",  "player2")

## Create data frame of player scores.
player_score_df <- data.frame(score, player)
head(player_score_df)
View(player_score_df)

## Create a box and whisker plot with player scores.
ggplot(data = player_score_df, aes(x=score, y=player)) +
  geom_boxplot()
