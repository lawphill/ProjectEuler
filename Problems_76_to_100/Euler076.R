# Counting summations
# 5 can be written as a sum in 6 unique ways:
#   4+1
#   3+2
#   3+1+1
#   2+2+1
#   2+1+1+1
#   1+1+1+1+1

# Note, 1 has 0 ways, 2 has 1 way, 3 has 2 ways, 4 has 4 ways, 5 has 6 ways, 6 has 10 ways

# How many ways can 100 be written as the sum of at least two positive integers?

# Look at each possible first term for the sum. To get the count of sums for that first term, we need
#   to know how many rewrites are possible for the remainder. BUT, in rewriting, none of the rewrites can
#   contain a term larger than the minimum term found so far.
#   To calculate that we call count_rewrites, but with a lower max_num
#   So, e.g. there are 2 ways to rewrite 3, 2+1 and 1+1+1
#     But let's say we really want to know about how many rewrites are possible for 4. Well 1 is a 
#     possible first term for that, but then the remainder leaves 3. Now, 2+1 isn't a possible rewrite
#     Because we can't use terms larger than 1, so we call count_rewrites(3,1) and that gives us the 1
#     possible rewrite that we want.
# To keep computation down, we keep results in a matrix with row = n, col = max_num
count_rewrites <- function(n,max_num){
  # Count the number of ways to rewrite n as a sum with max term equal to max_num
  if(max_num == 0 || n == 0){ return(NA) }
  
  if(rewrites[n,max_num] != -1){
    return(rewrites[n,max_num])
  }
  
  count <- 0
  for(i in min((n-1),max_num):1){
    if(i >= ceiling(n/2)){
      count <- count + 1
    }
    remainder <- n - i
    count <- count + count_rewrites(remainder,min(remainder,i))
  }
  #print(c(n,max_num,count))
  rewrites[n,max_num] <<- count
  return(count)
}

maxN <- 100
rewrites <- matrix(data=-1,nrow=maxN,ncol=maxN)
rewrites[1,] <- 0 # No way to rewrite if n = 1

count_rewrites(maxN,maxN)