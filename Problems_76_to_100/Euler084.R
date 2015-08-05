# LIST THE MONOPOLY SPACES AS 00 to 39, FIND THE SIX-DIGIT MODAL STRING (numbers for top 3 spaces)
#   WHEN USING TWO FOUR-SIDED DICE TO ADVANCE (AS OPPOSED TO THE NORMAL SIX-DIGIT DICE)
#   USING SIX-DIGIT DIE, THE MODAL STRING IS 102400 (10=JAIL,24=E3,00=GO)

# USE 01-40 IN PROBLEM, SINCE R IS ONE INDEXED, SUBTRACT ONE FROM EACH TO GET FINAL ANSWER

# SPECIAL SQUARES
# 01 = "GO"
# 11 = JAIL
# 31 = Go to JAIL
# 03 = CC1
# 18 = CC2
# 34 = CC3
# 08 = CH1
# 23 = CH2
# 37 = CH3

# SPECIAL RULES
# Community Chest (CC) (Each option 1/16 chance)
#   ADVANCE TO GO (01)
#   GO TO JAIL (11)
#
# Chance (CH) (Each option 1/16 chance)
#   ADVANCE TO GO (01)
#   GO TO JAIL (11)
#   GO TO C1 (12)
#   GO TO E3 (25)
#   GO TO H2 (40)
#   GO TO R1 (6)
#   GO TO NEXT R (6,16,26,36)
#   GO TO NEXT R (6,16,26,36)
#   GO TO NEXT U (13,29)
#   GO BACK 3 SQUARES

n_squares <- 40
die_sides <- 4
n_dice <- 2

go <- 1
jail <- 11
c1 <- 12
e3 <- 25
h2 <- 40
r <- c(6,16,26,36)
u <- c(13,29)
g2j <- 31
cc <- c(3,18,34)
ch <- c(8,23,37)

# INITIAL STARTING VECTOR, ALL PROBABILITY ON "GO"
init <- rep(0,n_squares)
init[1] <- 1 

# STANDARD DICE ROLE PROBABILITIES
die_counts <- mapply(function(x) x + 1:die_sides, 1:die_sides)
die_probs <- tabulate(die_counts) / die_sides^n_dice

# We should account for the fact that if we roll doubles twice P(doubles * 2) = (1/die_sides)^2
#   Then we end up in jail. We'll subtract this small probability from the double possibilities
#   And then add it back in later to the probability of ending up at jail

# The cube here is from (1/die_sides)^2 (prior prob of any 2 doubles) * (1/die_sides)^2 (probability of
#   getting a specific new double)
die_probs[seq.int(2,length(die_probs),2)] <- die_probs[seq.int(2,length(die_probs),2)] - (1/die_sides)^4

# TRANSITIONAL PROBABILITY MATRIX
p <- matrix(data=0,ncol=n_squares,nrow=n_squares)

# FILL IN TP TABLE WITH BASIC DICE ROLES
for(i in 1:n_squares){
  p[i,(i:(i+die_sides*n_dice-1) %% n_squares)+1] <- die_probs
}


# ACCOUNT FOR CH ROLES
#   WE HAVE TO DO THIS FIRST BECAUSE LANDING ON ch[3] WE HAVE A CHANCE TO "GO BACK 3 SQUARES"
#   WHICH MEANS WE'D NEED TO ROLL FOR A CC CARD
for(i in ch){
  # SENT TO GO
  p[,go] <- p[,go] + p[,i] * 1/16
  
  # SENT TO JAIL
  p[,jail] <- p[,jail] + p[,i] * 1/16
  
  # SENT TO C1
  p[,c1] <- p[,c1] + p[,i] * 1/16
  
  # SENT TO E3
  p[,e3] <- p[,e3] + p[,i] * 1/16
  
  # SENT TO H2
  p[,h2] <- p[,h2] + p[,i] * 1/16
  
  # SENT TO R1
  p[,r[1]] <- p[,r[1]] + p[,i] * 1/16
  
  # SENT TO NEXT R (2/16 chance)
  next_r <- ifelse(all(i > r),min(r),min(r[r > i]))
  p[,next_r] <- p[,next_r] + p[,i] * 2/16
  
  # SENT TO NEXT U
  next_u <- ifelse(all(i > u),min(u),min(u[u > i]))
  p[,next_u] <- p[,next_u] + p[,i] * 1/16
  
  # GO BACK 3 SQUARES
  p[,(i + (n_squares-3)) %% n_squares] <- p[,(i + (n_squares-3)) %% n_squares] + p[,i] * 1/16
  
  # NORMALIZE
  p[,i] <- p[,i] * 6/16
}

# ACCOUNT FOR CC ROLES
for(i in cc){
  # COULD BE SENT TO GO
  p[,go] <- p[,go] + p[,i] * 1/16
  # COULD BE SENT TO JAIL
  p[,jail] <- p[,jail] + p[,i] * 1/16
  
  # NORMALIZE
  p[,i] <- p[,i] * 14/16
}

# THREE DOUBLE ROLLS IN A ROW LANDS YOU IN JAIL
# Double probability is 1/die_sides, probability we got to current state on a double roll is
#   (1/die_sides)^2
p[,jail] <- p[,jail] + (1/die_sides)^3

# ACCOUNT FOR GO TO JAIL
p[,jail] <- p[,jail] + p[,g2j]
p[,g2j] <- 0

# Calculate total probability of being on any square
iterations <- 1000
#x <- rep(0,n_squares)
#x[1] <- 1
x <- colSums(p)
for(i in 1:iterations){
  x <- x %*% p
}

sorted = sort(colSums(x),decreasing=TRUE,index.return=TRUE)

print(sorted[[2]][1:3]-1)