# The number 145 is well known for the property that the sum of the factorial of its digits is equal to
# 145:
#  1! + 4! + 5! = 1 + 24 + 120 = 145

# Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169;
#   it turns out that there are only three such loops that exist:
  
# 169 → 363601 → 1454 → 169
# 871 → 45361 → 871
# 872 → 45362 → 872

# It is not difficult to prove that EVERY starting number will eventually get stuck in a loop.
#   For example,
# 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
# 78 → 45360 → 871 → 45361 (→ 871)
# 540 → 145 (→ 145)

# Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with
#   a starting number below one million is sixty terms.

# How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?

end_states <- c(145,169,363601,1454,871,45361,872,45362,1,2,40585)
maximum <- 1000000
repeats <- 60
repetitions <- rep(0,maximum)
#repetitions[end_states] <- c(2,4,4,4,3,3,3,3,2,2,2)
#to_check <- c(69,78,540)
#for(i in to_check){
repetitions[1:2] <- c(1,1)
for(i in 3:maximum){
  i0 <- i
  previous <- c(0,0,0)
  non_repeat <- 0
  #while(non_repeat <= repeats){
  while(!any(i == previous)){
    previous <- c(previous[2:3],i)
    i <- sum(factorial((i %/% 10^((ceiling(log10(i+1))-1):0)) %% 10))
    non_repeat <- non_repeat + 1
    
    if(i < maximum & repetitions[i] != 0){
      non_repeat <- non_repeat + repetitions[i]
      break
    }
  }
  repetitions[i0] <- non_repeat
}
print(sum(repetitions == 60))
#print(repetitions[to_check])