# The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
# Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

# Iterate from 1:1000, but at each point record only last 10 digits.

last_digits <- function(prev,x,n){
  # Return last n digits of prev + x^x
  max_digits <- 10^n
  l <- x %% max_digits
  i <- 2
  while( i <= x ){
    l <- (l*x) %% max_digits
    i <- i + 1
  }
  return((l+prev) %% max_digits)
}

max_digit <- 1000
digits <- 1
digits_to_recall <- 10

for(i in 2:max_digit){
  digits <- last_digits(digits,i,digits_to_recall)
}
print(digits)