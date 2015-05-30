#An irrational decimal fraction is created by concatenating the positive integers:
#  0.123456789101112131415161718192021...
#It can be seen that the 12th digit of the fractional part is 1.

#If dn represents the nth digit of the fractional part, find the value of the following
#   expression.
#   d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

to_find <- c(1,10,100,1000,10000,100000,1000000)
found <- c()
num <- 0
curr_index <- 0
max_index <- 1000000

digits <- function(x){
  d <- rep(0,ceiling(log10(x)))
  i <- 0
  while(x > 0){
    i <- i + 1
    d[i] <- x %% 10
    x <- x %/% 10
  }
  return(rev(d))
}


total <- 1
while(curr_index <= max_index){
  num <- num + 1
  ndigit <- ceiling(log10(num+1))
  # CHECK IF WE'RE APPROACHING AN IMPORTANT NUMBER
  if(sum(match(curr_index:(curr_index+ndigit),to_find),na.rm=TRUE) > 0){
    d <- digits(num)
    for(digit in d){
      curr_index <- curr_index + 1
      if(any(curr_index==to_find)){
        total <- total * digit
        found <- c(found,digit)
      }
    }    
  }else{ # OTHERWISE JUST INCREMENT BY NDIGIT AND CONTINUE
    curr_index <- curr_index + ndigit
  }
}