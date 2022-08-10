## Continuous data: data that is measured in some way rather than counted.

## Probability density function: Tells you how high probabilities are across ranges.
## Probability density is the line on the graph and the probability is given by the area underneath it for a specific range of values.

## Standardize distribution and range.
standardize <- function(x, m, v) {
  mx_diff <- x - m
  z <- mx_diff / sqrt(v)
  print(z)
}
standardize(64, 71, 20.25)
