# Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a
#   reduced proper fraction.

# If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
  
#  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7,
#     7/8

# It can be seen that 2/5 is the fraction immediately to the left of 3/7.

# By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the
#   numerator of the fraction immediately to the left of 3/7.

#
# So, we want to solve for the largest reduced proper fraction that's less than 3/7 with d <= 1,000,000
# that's 428,571/1,000,000

gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}

thresh <- 3/7
max_numer <- 1
max_denom <- 7
max_fraction <- max_numer / max_denom # Arbitrary fraction less than 3/7
to_check <- 3:1000000
to_check <- to_check[to_check != 7]
for(d in to_check){
  lower_bound <- ceiling(d*max_fraction)
  upper_bound <- floor(d * thresh)
  
  if(upper_bound < lower_bound){
    next
  }
  
  n <- lower_bound:upper_bound
  
  has_hcf1 <- gcd(n,d) == 1
  if(any(has_hcf1)){
    max_numer <- max(n[has_hcf1])
    max_denom <- d
    max_fraction <- max_numer / max_denom
  }
}
print(c(max_numer,max_denom))
print(c(max_fraction,thresh))