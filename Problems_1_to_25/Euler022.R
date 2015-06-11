# names.txt is a list of names
# Sort alphabetically
# Convert each name to a set of numbers, a=1,b=2,c=3,...
# The name's score is it's index (1st, 2nd, 925th, etc) x (sum of name's numbers)

# CODE FUNCTIONS PROPERLY NOW
# It appears the scan() function was cutting off one of the names
# R found 5162 entries, there should be 5163
# I wrote a perl script to do this instead, and it works properly
# I've updated the R code so that it now does the pre-processing by hand
# And that's fixed the issue

filename <- "data/names.txt"
#names <- scan(file=filename,what="character",sep=",")
#names <- sort(tolower(names))
raw <- readLines(filename)
raw_names <- strsplit(raw,",")
names <- rep("",length(raw_names[[1]]))

for(i in 1:length(names)){
  # Pre-process string
  n <- raw_names[[1]][i]
  n <- tolower(gsub("\"","",n))
  names[i] <- n
}
names <- sort(names)

total <- 0
name_cost <- rep(0,length(names))
total_cost <- rep(0,length(names))

for(i in 1:length(names)){
  
  chars <- strsplit(names[i],"")
  name_cost[i] <- sum(match(chars[[1]],letters))
  total_cost[i] <- (name_cost[i] * i)
  total <- total + total_cost[i]
}
print(total)