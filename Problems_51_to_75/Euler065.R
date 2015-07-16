# PRINT THE SUM OF DIGITS IN THE 100TH NUMERATOR IN THE CONTINUED FRACTION OF e
# PRINCIPLE PROBLEM IS THAT THE NUMERATOR IS SO LARGE (58 digits)
# TREAT EACH NUMERATOR AS A VECTOR OF DIGITS, SUM THE VECTOR

get_digits <- function(x){ (x %/% 10^((ceiling(log10(x+1))-1):0)) %% 10 }

# TAKES TWO VECTORS OF DIGITS AND ADDS THEM
add_digits <- function(x,y){
  lx <- length(x)
  ly <- length(y)
  x <- rev(x)
  y <- rev(y)
  
  new_digits <- rep(0,max(lx,ly))
  remainder <- 0
  for(i in 1:max(lx,ly)){
    summed <- remainder
    if(i <= lx){
      summed <- summed + x[i]
    }
    if(i <= ly){
      summed <- summed + y[i]
    }
    new_digits[i] <- summed %% 10
    remainder <- (summed - new_digits[i]) %/% 10
  }
  while(remainder > 0){
    new_digits <- c(new_digits, remainder %% 10 )
    remainder <- remainder %/% 10
  }
  return(rev(new_digits))
}

max_num <- 100
a0 <- 2
a <- rep(1,max_num)
a[seq.int(2,max_num,3)] <- 2*(1:length(seq.int(2,max_num,3)))

# INITIALIZE
numer <- vector("list",max_num)
numer[[1]] <- get_digits(a0)
numer[[2]] <- add_digits(numer[[1]]*a[1],1)

for(i in 3:max_num){
  numer[[i]] <- add_digits(numer[[i-1]]*a[i-1],numer[[i-2]])
}
print(sum(numer[[max_num]]))