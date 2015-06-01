# The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of
#   the digits 0 to 9 in some order, but it also has a rather interesting sub-string
#   divisibility property.

# Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the
#   following:
#   d2d3d4=406 is divisible by 2
#   d3d4d5=063 is divisible by 3
#   d4d5d6=635 is divisible by 5
#   d5d6d7=357 is divisible by 7
#   d6d7d8=572 is divisible by 11
#   d7d8d9=728 is divisible by 13
#   d8d9d10=289 is divisible by 17
# Find the sum of all 0 to 9 pandigital numbers with this property.

total <- 0

# CHUNK LARGE NUMBER INTO SMALLER BITS
# WE CAN THEN ADJOIN THE CHUNKS AT THE END TO CHECK THE FINAL NUMBER
eight_to_ten <- seq.int(17,999,17)
five_to_seven <- seq.int(7,999,7)
five_to_seven <- five_to_seven[(five_to_seven%/%10)%%10 == 5]
three_to_five <- seq.int(12,999,3)
three_to_five <- three_to_five[((three_to_five %/% 10) %% 10) %% 2 == 0]

# Record each successful number
nums <- c()

for(eight_index in 1:length(eight_to_ten)){
  # Check for unique digits
  digits <- strsplit(toString(eight_to_ten[eight_index]),"")
  if(length(digits[[1]]) != length(unique(digits[[1]]))){
    next
  }
  
  for(five_index in 1:length(five_to_seven)){
    num <- five_to_seven[five_index]*1000 + eight_to_ten[eight_index]
    if(((num%/%10)%%1000) %% 13 != 0){ # Check that 7:9 divisible by 13
      next
    }
    if(((num%/%100)%%1000) %% 11 != 0){
      next
    }
    digits <- strsplit(toString(num),"") # Check for unique digits
    if(length(digits[[1]]) != length(unique(digits[[1]]))){
      next
    }
    for(three_index in 1:length(three_to_five)){
      if(((num%/%100000)%%10) != (three_to_five[three_index] %% 10)){
        next # Break if last digit doesn't match first digit here
      }
      num2 <- (three_to_five[three_index] %/% 10) * 1000000 + num
      digits <- strsplit(toString(num2),"") # Check for unique digits
      if(length(digits[[1]]) != length(unique(digits[[1]]))){
        next
      }
      for(two_index in 0:9){
        num3 <- two_index * 100000000 + num2
        digits <- strsplit(toString(num3),"") # Check for unique digits
        if(length(digits[[1]]) != length(unique(digits[[1]]))){
          next
        }
        for(one_index in 1:9){
          num4 <- one_index * 1000000000 + num3
          digits <- strsplit(toString(num4),"") # Check for unique digits
          if(length(digits[[1]]) == length(unique(digits[[1]]))){
            total <- total + num4
            nums <- c(nums,num4)
          }
        }
      }
    }
  }
}