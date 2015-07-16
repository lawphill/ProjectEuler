# Solved by hand. The only possible solution has to sum to 14. The largest numbers have to be on the
#   outside (6:10), 6 is the smallest so it has to begin the sequence. The "rows" are
#   6 5 3
#   7 5 2
#   8 4 2
#   9 4 1
#   10 3 1

# The highest ordering is then 6531031914842725


# IGNORE CODE BELOW
library("permute") # For allPerms function

next_combs <- function(combs,prohibited,row_sum,to_match,match_index=3){
  # Takes a set of combinations from the combn function, and returns the set of combinations
  #   which match the values of to_match (at match_index), but which are not in the array prohibited
  
  # Remove prohibited items from consideration
  combs <- combs[, ! 1:ncol(combs) %in% prohibited]
  
  # Remove items which don't have the necessary matches
  if(!is.na(to_match)){
    poss <- which(sapply(1:ncol(combs), function(x) all(to_match %in% combs[,x])))
    combs <- combs[,poss]
  }
  combs <- as.matrix(combs)
  
  if(length(combs)==0){
    return(NA)
  }

  l <- list()
  i <- 1
  for(n in 1:ncol(combs)){
    d <- combs[,n]
    #print(d)
    perms <- matrix(0,ncol=6,nrow=3)
    perms[,1] <- c(1,2,3)
    perms[,2:6] <- t(allPerms(d))
    #print(perms)
    perms <- matrix(d[perms],ncol=6,nrow=3)
    #print(perms)
    
    if(!is.na(to_match)){
      matching <- which(sapply(1:6, function(x) all(perms[match_index,x] == to_match)))
      if(length(matching) == 0){ next }
    }else{
      matching <- 1:6
    }
    for(m in 1:length(matching)){
      l[[i]] <- c(perms[,matching[m]],n) # Add n, so we know which combs the permutation belongs to
      i <- i + 1
    }
    
  }
  #print(l)
  if(i == 1){ #No matches
    return(NA)
  }
  l <- matrix(unlist(l),nrow=4,ncol=length(l))
  #print(l)
  l <- as.matrix(l)
  l <- l[,(colSums(l)-l[4,])==row_sum]
  
  return(sortMatrix(as.matrix(l)))
}

magicNgon <- function(Ngon, nums, nums_per_row, row_sum){
  if(!all(nums == 1:10)){
    return("Function currently requires nums = 1:10")
  }
  combs <- combn(rev(nums),nums_per_row)
  combs <- combs[,colSums(combs) == row_sum]
  
  # This is a hand-coded fix for the fact that we want 10 to be ranked lower than other numbers
  sorted_combs <- combs
  begin10 <- sum(combs[1,] == 10)
  sorted_combs[,1:(ncol(combs)-begin10)] <- combs[,combs[1,] != 10]
  sorted_combs[,(ncol(combs)-begin10+1):ncol(combs)] <- combs[,combs[1,]==10]
  combs <- sorted_combs
  prohibited <- c()
  
  poss1 <- next_combs(combs,prohibited,row_sum,NA)
  if(is.na(poss1)){ 
    return(NA)
  }
  for(i in 1:ncol(poss1)){
    prohibited2 <- c(prohibited, poss1[4,i])
    to_match <- poss1[3,i]
    match_location <- 2
    poss2 <- next_combs(combs,prohibited2,row_sum,to_match,match_location)
    if(is.na(poss2)){
      next
    } 
    
    for(j in 1:ncol(poss2)){
      prohibited3 <- c(prohibited2,poss2[4,j])
      to_match <- poss2[3,j]
      match_location <- 2
      poss3 <- next_combs(combs,prohibited3,row_sum,to_match,match_location)
      if(is.na(poss3)){
        next
      }
      
      for(k in 1:ncol(poss3)){
        prohibited4 <- c(prohibited3,poss3[4,k])
        to_match <- poss3[3,k]
        match_location <- 2
        poss4 <- next_combs(combs,prohibited4,row_sum,to_match,match_location)
        if(is.na(poss4)){
          next
        }
        
        for(l in 1:ncol(poss4)){
          prohibited5 <- c(prohibited4,poss4[4,l])
          to_match <- c(poss4[3,l],poss1[2,i])
          match_location <- c(2,3)
          poss5 <- next_combs(combs,prohibited5,row_sum,to_match,match_location)
          if(is.na(poss5)){
            next
          }
          
          for(m in 1:ncol(poss5)){
            digits <- c(poss1[1:3,i],poss2[1:3,j],poss3[1:3,k],poss4[1:3,l],poss5[1:3,m])
            if(length(unique(digits)) == length(nums)){
              print(digits)
              
              return(matrix(digits,ncol=5,nrow=3))
            }
          }
        }
      }
      
    }
    
  }
}

sortMatrix <- function(x){
  x[x==10] <- 0 # Recode 10s as 0s
  if(nrow(x) < 3){
    return("Function is not implemented for nrow other than 3")
  }
  indices <- order(x[1,],x[2,],x[3,],decreasing=TRUE)
  x <- x[,indices]
  x[x==0] <- 10
  return(as.matrix(x)) # as.matrix in case there's only one item
}


Ngon <- 5
nums <- 1:10
nums_per_row <- 3
row_sum <- 15

m <- magicNgon(Ngon,nums,nums_per_row,row_sum)
print(paste(c(m),collapse=""))