# For all numbers less than 1 million, find the sum for those which are palindromes
# both in base 10, and in base 2

max_num <- 999999L
total <- 0
nums <- 1:max_num

dec_vec1 <- 10^(1:ceiling(log10(max_num+1)))
dec_vec2 <- dec_vec1/10
bin_vec1 <- 2^(1:ceiling(log2(max_num+1)))
bin_vec2 <- bin_vec1/2
ndec <- ceiling(log10(nums+1))
nbin <- ceiling(log2(nums+1))

# Old code, a little slow, finishes in ~9 seconds
# Brute Force, convert into a decimal and binary vector, check if palindrome
# Only calculate binary if decimal is palindrome, to avoid calculating large binary vectors
# Could be made slightly more efficient by only checking even #s in base10
if(1==0){
for(i in nums){
  dec <- (i %% dec_vec1[ndec[i]:1]) %/% dec_vec2[ndec[i]:1]
  if(identical(dec,rev(dec))){
    bin <- (i %% bin_vec1[nbin[i]:1]) %/% bin_vec2[nbin[i]:1]
    if(identical(bin,rev(bin))){
      total <- total + i
    }
  }
}
}

# New code, generates base10 palindromes by taking abc -> abccba and abcba
# Then checks those as binary palindromes (as with old code)
# Much more efficient
for(i in 1:999){
  # Generate a palindrome of the form abc -> abccba
  nchar <- ceiling(log10(i+1))
  digits <- (i %% dec_vec1[nchar:1]) %/% dec_vec2[nchar:1]
  even_digits <- c(digits,rev(digits))
  palindrome <- sum(even_digits * dec_vec2[(nchar*2):1])
  nbin <- ceiling(log2(palindrome+1))

  # Check if binary is palindrome
  bin <- (palindrome %% bin_vec1[nbin:1]) %/% bin_vec2[nbin:1]
  if(identical(bin,rev(bin))){
    total <- total + palindrome
  }
  
  # Generate a palindrome of the form abc -> abcba
  odd_digits <- c(digits,rev(digits[1:nchar-1]))
  palindrome2 <- sum(odd_digits * dec_vec2[(nchar*2-1):1])
  nbin <- ceiling(log2(palindrome2+1))
  
  bin2 <- (palindrome2 %% bin_vec1[nbin:1]) %/% bin_vec2[nbin:1]
  if(identical(bin2,rev(bin2))){
    total <- total + palindrome2
  }
}