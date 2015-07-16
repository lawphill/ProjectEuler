# The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and
#   66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations of its
#   digits which are also cube.
# Find the smallest cube for which exactly five permutations of its digits are cube.

# 1: DETERMINE CUBES
# 2: TALLY DIGITS FOR EACH CUBE AND STORE THE TALLY (0s: 1, 1s: 2, etc...)
# 3: CONVERT THE TALLY INTO AN INTEGER
# 4: GO THROUGH EACH CUBE CHECKING TO SEE HOW MANY CUBES TALLIED TO THE SAME INTEGER
# 5: STOP AFTER FINDING A MATCH FOR THE NUMBER OF PERMUTATIONS WE'RE LOOKING FOR

max_num <- 9999
cubes <- (1:max_num)^3
nchars <- ceiling(log10(cubes+1))

perms <- 5

digits <- list()
tabs <- matrix(data=0,nrow=max_num,ncol=10)
sums <- rep(0,max_num)
for(i in 1:length(cubes)){
  digits[[i]] <- rev((cubes[i] %/% 10^(0:(nchars[i]-1))) %% 10)
  for(d in digits[[i]]){
    tabs[i,(d+1)] <- tabs[i,(d+1)] + 1
  }
  sums[i] <- sum(tabs[i,] * 10^(9:0))
}

for(i in 1:max_num){
  if(sum(sums==sums[i]) == perms){
    print(cubes[i])
    print(i)
    break
  }
}