# If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
# Not all numbers produce palindromes so quickly. For example,
#   349 + 943 = 1292,
#   1292 + 2921 = 4213
#   4213 + 3124 = 7337

# That is, 349 took three iterations to arrive at a palindrome.
#   Although no one has proved it yet, it is thought that some numbers, like 196, never produce a
# palindrome. A number that never forms a palindrome through the reverse and add process is called a
# Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem,
# we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for
# every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations,
# or, (ii) no one, with all the computing power that exists, has managed so far to map it to a
# palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before
# producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

# Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is
#   4994.
# How many Lychrel numbers are there below ten-thousand?
# NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel
#   numbers.

get_digits <- function(x){ (x %/% 10^((ceiling(log10(x+1))-1):0)) %% 10 }

# TAKES A SET OF DIGITS AND ADDS THE REVERSE, IMPLEMENTED SO THAT IT NEVER CALCULATES THE EXACT INTEGER
# VALUE. DOING SO WOULD RUN INTO MODULO ACCURACY PROBLEMS.
add_digits <- function(x){
  new_digits <- rep(0, length(x)+1)
  remainder <- 0
  for(i in 1:length(x)){
    summed <- x[i] + x[length(x)+1-i] + remainder
    new_digits[i] <- summed %% 10
    remainder <- (summed - new_digits[i]) %/% 10
  }
  if(remainder > 0){
    new_digits[length(x)+1] <- remainder
  }else{
    new_digits <- new_digits[1:length(x)]
  }
  return(rev(new_digits))
}

max_num <- 9999
max_iters <- 50
lychrel <- rep(TRUE,max_num)

for(i in 1:max_num){
  curr <- get_digits(i)
  for(iter in 1:max_iters){
    curr <- add_digits(curr)
    if(all(curr == rev(curr))){
      lychrel[i] <- FALSE
      break
    }
  }
}
print(sum(lychrel))

