# A common security method used for online banking is to ask the user for three random characters from a
#   passcode. For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters
#   the expected reply would be: 317.

# The text file, keylog.txt, contains fifty successful login attempts.
# Given that the three characters are always asked for in order, analyse the file so as to determine the
#   shortest possible secret passcode of unknown length.

# Read File
filename <- "data/p079_keylog.txt"
logins <- read.table(filename)
logins <- logins[[1]]


digits <- sapply(1:50,function(x) (logins[x] %/% 10^(2:0)) %% 10)

passcode <- rep(-1,length(unique(c(digits))))

poss <- sort(unique(c(digits)))

followers <- vector("list",length(poss))
for(i in 1:length(poss)){
  if(any(digits[1,] == poss[i])){
    followers[[i]] <- sort(unique(c(digits[2:3,digits[1,]==poss[i]])))
  }else if (any(digits[2,] == poss[i])){
    followers[[i]] <- sort(unique(c(digits[3,digits[2,]==poss[i]])))
  }
}
passcode <- c()
# The last number would be the one that nothing follows
next_digit <- poss[sapply(1:length(poss), function(x) is.null(followers[[x]]))]
passcode <- c(passcode,next_digit)
followers[[which(poss == passcode)]] <- NA # This will keep this from matching later searches

for(i in 2:length(poss)){
  next_digit <- poss[sapply(1:length(poss),function(x) all(passcode %in% followers[[x]]) & all(followers[[x]] %in% passcode))]
  passcode <- c(next_digit,passcode)
}

print(sum(passcode * 10^((length(poss)-1):0)))

