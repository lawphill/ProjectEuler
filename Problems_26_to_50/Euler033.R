# 49/98 reduces to 4/8, removing the 9s. 30/50=3/5 is a trivial example
#   There are 4 non-trivial fractions, less than 1 in value, with 2digit numerators & 
#   denominators
#   Find the product of the 4 fractions given in lowest common terms, return the denominator

# So, we have either ax/xb, xa/xb, xa/bx, or ax/bx = a/b, a < b, 
#   a != 0, b != 0, which we know b/c a/0 is ill-defined, and 0/b = 0, implying that
#     x = 0, which would make it trivial (e.g. 00/b0 = 0/b)

# Brute force solution, go through all possible values of a, b and x
# Once we grab the four answers, reduce each fraction and then cancel out any duplicates
a_vals <- rep(0,4)
b_vals <- rep(0,4)
x_vals <- rep(0,4)
count <- 0
for(a in 1:8){
  for(b in (a+1):9){
    for(x in 1:9){
      ax <- a*10 + x
      xa <- x*10 + a
      bx <- b*10 + x
      xb <- x*10 + b
      
      poss_fracs <- c(ax/bx,ax/xb,xa/bx,xa/xb)
      if( sum( (poss_fracs == a/b)==TRUE ) > 0 ){
        # If at least one fraction matches
        count <- count + 1
        a_vals[count] <- a
        b_vals[count] <- b
        x_vals[count] <- x
      }
    }
  }
}

# Calculate lcd
poss_factors <- 2:9
for(i in 1:length(a_vals)){
  # Reduce all values
  if(a_vals[i] > 1){
    factors_a <- poss_factors[(a_vals[i] %% poss_factors) == 0]
    factors_b <- poss_factors[(b_vals[i] %% poss_factors) == 0]
    searching <- 1
    f <- length(factors_a)
    while(f >= 1 && searching == 1){
      if(sum((factors_b == factors_a[f])==TRUE) > 0){
        a_vals[i] <- a_vals[i] / factors_a[f]
        b_vals[i] <- b_vals[i] / factors_a[f]
        searching <- 0
      }
      f <- f - 1
    }
  }
}

# Remove duplicates
for(i in 1:length(b_vals)){
  if(sum((a_vals %% b_vals[i]) == 0) > 0){
    b_vals[i] <- 1
  }
}
print(prod(b_vals))