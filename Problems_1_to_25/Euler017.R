# Calculate the number of characters necessary to print out each number from 1:1000
# So 1 = "one" 3 characters
# 234 = "two hundred and thirty four" = 23
# Don't count hyphens or spaces, but include "and"

# Solved by creating a hash for the strings associated with each number
# Work through the number adding letter counts for the thousands, hundreds,
# tens, and then digits. When appropriate, digit columns are merged


library("hash")


num_to_string <- function(n){
  characters <- 0
  num_array <- strsplit(strReverse(toString(n)),'')
  strs <- num_array[[1]]
  nums <- as.numeric(strs)
  print_ones_digit <- 1
  
  if(length(nums)==4){
    if(nums[4]==1){
      characters <- characters + nchar("onethousand")
    }
  }
  if(length(nums)>=3){
    if(nums[3] != 0){
      characters <- characters + nchar(names[[strs[3]]]) + nchar("hundred")
    }
    if(sum(nums[1:2]) != 0){
      characters <- characters + nchar("and")
    }
  }
  if(length(nums)>=2){
    if(nums[2]>=2){ # If tens digit greater than or equal to 2
      characters <- characters + nchar(names[[paste(nums[2],0,sep="")]])
    }
    if(nums[2]==1){
      characters <- characters + nchar(names[[paste(nums[2],nums[1],sep="")]])
      print_ones_digit <- 0
    }
  }
  if(length(nums)>=1 && print_ones_digit == 1 && nums[1] != 0){
    characters <- characters + nchar(names[[strs[1]]])
  }
  
  return(characters)
}

# Taken from R documentation
strReverse <- function(x)
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

numbers <- c(1:20,30,40,50,60,70,80,90)
number_names <- c("one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety")
names <- hash(numbers,number_names)

total_characters <- 0
for(i in 1:1000){
  char <- num_to_string(i)
  if(length(char)==0){ print(i)}
  total_characters <- total_characters + num_to_string(i)
}
print(total_characters)
