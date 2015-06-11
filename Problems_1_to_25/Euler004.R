# A palindromic number reads the same both ways.
# The largest palindrome made from the product of two 2-digit numbers
# is 9009 = 91 × 99.
#
# Find the largest palindrome made from the product of two 3-digit numbers.

min_factor<-100
max_factor<-999
# Get list of all numbers with two 3-digit factors
m <- sort(unique(c((max_factor:min_factor) %*% t(max_factor:min_factor))),decreasing=TRUE)

# Going from largest to smallest, return first that's a palindrome
for(num in m){
  digits <- as.integer(unlist(strsplit(toString(num),split="")))
  if(all(digits == rev(digits))){
    print(num)
    break
  }
}

