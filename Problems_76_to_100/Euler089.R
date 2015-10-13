# For a number written in Roman numerals to be considered valid there are basic rules which must be
#   followed. Even though the rules allow some numbers to be expressed in more than one way there is
#   always a "best" way of writing a particular number.

#For example, it would appear that there are at least six ways of writing the number sixteen:
#  IIIIIIIIIIIIIIII
#  VIIIIIIIIIII
#  VVIIIIII
#  XIIIIII
#  VVVI
#  XVI

# However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to
#   be the most efficient, as it uses the least number of numerals.

# The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains one thousand numbers
#   written in valid, but not necessarily minimal, Roman numerals; see About... Roman Numerals for the
#   definitive rules for this problem.

# Find the number of characters saved by writing each of these in their minimal form.

# Note: You can assume that all the Roman numerals in the file contain no more than four consecutive
#   identical units.

int_to_numeral <- function(n){
  char_list <- vector("character")
  curr_numeral <- length(numerals)
  while(n > 0){
    if(n >= numerals[curr_numeral]){
      char_list = add_char(char_list,names(numerals[curr_numeral]))
      n <- n - numerals[curr_numeral]
      
      # Deal with subtractive combinations
    }else if(numerals[curr_numeral] == 1000 && n >= 900){
      char_list = add_char(char_list,c("C","M"))
      n <- n - 900
    }else if(numerals[curr_numeral] == 500 && n >= 400){
      char_list = add_char(char_list,c("C","D"))
      n <- n - 400
    }else if(numerals[curr_numeral] == 100 && n >= 90){
      char_list = add_char(char_list,c("X","C"))
      n <- n - 90
    }else if(numerals[curr_numeral] == 50 && n >= 40){
      char_list = add_char(char_list,c("X","L"))
      n <- n - 40
    }else if(numerals[curr_numeral] == 10 && n >= 9){
      char_list = add_char(char_list,c("I","X"))
      n <- n - 9
    }else if(numerals[curr_numeral] == 5 && n >= 4){
      char_list = add_char(char_list,c("I","V"))
      n <- n - 4
    }else{
      curr_numeral <- curr_numeral - 1
    }
  }
  return(paste(char_list,collapse=""))
}

add_char <- function(char_list,c_array){
  char_list <- char_list
  for(c in c_array){
    char_list[length(char_list)+1] <- c
  }
  return(char_list)
}

numeral_to_int <- function(string){
  nums <- numerals[unlist(strsplit(string,""))]
  total <- 0
  if(length(nums) == 1){
    return(nums)
  }
  
  i <- 1
  while(i <= length(nums)){
    if(i == length(nums)){
      total <- total + nums[i]
    }else if(nums[i+1] > nums[i]){
      total <- total + nums[i+1] - nums[i]
      i <- i + 1
    }else{
      total <- total + nums[i]
    }
    i <- i + 1
  }
  return(total)
}


filename <- "data/p089_roman.txt"
data<-scan(filename,what="",sep="\n")

numerals <- c(1,5,10,50,100,500,1000)
names(numerals) <- c("I","V","X","L","C","D","M")

savings <- 0
condensed <- vector("character",length(data))
for(i in 1:length(data)){
  curr_len <- nchar(data[i])
  n <- numeral_to_int(data[i])
  condensed[i] <- int_to_numeral(n)
  final_len <- nchar(condensed[i])
  savings <- savings + curr_len - final_len
}
print(savings)

compare <- matrix(data= c(data,condensed),ncol=2)
