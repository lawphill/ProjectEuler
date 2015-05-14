# names.txt is a list of names
# Sort alphabetically
# Convert each name to a set of numbers, a=1,b=2,c=3,...
# The name's score is it's index (1st, 2nd, 925th, etc) x (sum of name's numbers)

# CODE DOES NOT FUNCTION PROPERLY
# It appears the scan() function is cutting off one of the names
# R find 5162 entries, there should be 5163
# I've written a perl script to do this instead, and it works properly

filename <- "data/names.txt"
names <- scan(file=filename,what="character",sep=",")
names <- sort(tolower(names))

total <- 0
name_cost <- rep(0,length(names))
total_cost <- rep(0,length(names))

for(i in 1:length(names)){
  chars <- strsplit(names[i],"")
  name_cost[i] <- sum(match(chars[[1]],letters))
  total_cost[i] <- (name_cost[i] * i)
  total <- total + total_cost[i]
}