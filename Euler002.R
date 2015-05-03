# Calculate sum of all EVEN Fibonacci numbers less than 4,000,000

total <- 0
first_num <- 1
second_num <- 2

while(second_num < 4000000){
  if(second_num %% 2 == 0){
    total <- total + second_num
  }
  next_num <- first_num + second_num
  first_num <- second_num
  second_num <- next_num
}