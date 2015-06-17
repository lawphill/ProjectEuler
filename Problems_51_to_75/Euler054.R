find_winner <- function(hand1,hand2){
  # RETURNS 1 IF PLAYER ONE WINS
  # RETURNS 2 IF PLAYER TWO WINS
  # RETURNS 0 IF A TIE
  
  # CHECK FOR ROYAL FLUSH
  same_suit <- c(length(unique(hand1[2,]))==1,length(unique(hand2[2,]))==1)
  
  sorted_hand1 <- sort(hand1[1,])
  sorted_hand2 <- sort(hand2[1,])
  is_royal <- c(all(sorted_hand1 == 9:13),all(sorted_hand2 == 9:13))
  royal_flush <- is_royal & same_suit
  
  if(any(royal_flush)){
    return(return_winner(royal_flush,hand1,hand2))
  }
  
  # CHECK FOR STRAIGHT FLUSH
  is_consecutive <- c(TRUE,TRUE)
  for(j in 2:5){
    if(sorted_hand1[j] != (sorted_hand1[j-1] + 1)){
      is_consecutive[1] <- FALSE
    }
    if(sorted_hand2[j] != (sorted_hand2[j-1] + 1)){
      is_consecutive[2] <- FALSE
    }
  }
  straight_flush <- is_consecutive & same_suit
  if(any(straight_flush)){
    return(return_winner(straight_flush,hand1,hand2))
  }
  
  # CHECK FOR FOUR OF A KIND
  kinds1 <- table(hand1[1,])
  kinds2 <- table(hand2[1,])
  four_ofakind <- c(any(kinds1 == 4), any(kinds2 == 4))
  if(any(four_ofakind)){
    return(return_winner(four_ofakind,hand1,hand2))
  }
  
  # CHECK FOR A FULL HOUSE
  full_house <- c(any(kinds1==2) & any(kinds1==3), any(kinds2==2) & any(kinds2==3))
  if(any(full_house)){
    return(return_winner(full_house,hand1,hand2))
  }
  
  # CHECK FOR A FLUSH
  if(any(same_suit)){
    return(return_winner(same_suit,hand1,hand2))
  }
  
  # CHECK FOR A STRAIGHT
  if(any(is_consecutive)){
    return(return_winner(is_consecutive,hand1,hand2))
  }
  
  # CHECK FOR THREE OF A KIND
  three_ofakind <- c(any(kinds1==3),any(kinds2==3))
  if(any(three_ofakind)){
    return(return_winner(three_ofakind,hand1,hand2))
  }
  
  # CHECK FOR TWO PAIRS
  two_pair <- c(sum(kinds1==2)==2,sum(kinds2==2)==2)
  if(any(two_pair)){
    return(return_winner(two_pair,hand1,hand2))
  }
  
  # CHECK FOR A PAIR
  pair <- c(any(kinds1==2),any(kinds2==2))
  if(any(pair)){
    return(return_winner(pair,hand1,hand2))
  }
  
  # CHECK FOR HIGHEST CARD
  return(has_highest(hand1,hand2))
}


return_winner <- function(bools,hand1,hand2){
  if(bools[1] & !bools[2]){
    return(1)
  }else if(bools[2] & !bools[1]){
    return(2)
  }else if(all(bools)){
    return(has_highest(hand1,hand2))
  }
}

has_highest <- function(hand1,hand2){
  x1 <- rep(0,13)
  x2 <- rep(0,13)
  x1[1:max(hand1[1,])] <- tabulate(hand1[1,])
  x2[1:max(hand2[1,])] <- tabulate(hand2[1,])
  
  winner <- 0
  duplicate_val <- 0
  for(i in 13:1){
    if(x1[i] > x2[i]){
      if(x1[i] > duplicate_val){
        winner <- 1
        duplicate_val <- x1[i]
      }
    }else if(x1[i] < x2[i]){
      if(x2[i] > duplicate_val){
        winner <- 2
        duplicate_val <- x2[i]
      }
    }
  }
  # IF WE FOUND NO WINNER
  return(winner)
}


filename <- "data/p054_poker.txt"

data <- read.table(filename,header=FALSE,sep=" ")
ntrials <- nrow(data)

cards <- c(sapply(2:9,toString),"T","J","Q","K","A")
suits <- c("H","D","C","S")
both <- c(cards,suits)

p1_wins <- 0
p2_wins <- 0
for(i in 1:ntrials){
  # PROCESS EACH HAND
  rawhand1 <- sapply(data[i,1:5], function(x) strsplit(toString(x),""))
  rawhand2 <- sapply(data[i,6:10], function(x) strsplit(toString(x),""))
  
  hand1 <- matrix(data=0,ncol=5,nrow=2)
  hand2 <- matrix(data=0,ncol=5,nrow=2)
  
  for(j in 1:5){
    hand1[,j] <- match(rawhand1[[j]],both)
    hand2[,j] <- match(rawhand2[[j]],both)
  }
  
  # DETERMINE WINNER
  winner <- find_winner(hand1,hand2)
  
  if(winner == 1){
    p1_wins <- p1_wins + 1
  }else if(winner == 2){
    p2_wins <- p2_wins + 1
  }else if(winner == 0){
    print("Tie on line:")
    print(i)
    break
  }
}
print(p1_wins)