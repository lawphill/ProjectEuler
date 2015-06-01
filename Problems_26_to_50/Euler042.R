# The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
#   so the first ten triangle numbers are: 
#  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

# By converting each letter in a word to a number corresponding to its alphabetical
#   position and adding these values we form a word value. For example, the word value
#   for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we
#   shall call the word a triangle word.

# Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing
# nearly two-thousand common English words, how many are triangle words?

word_file <- "data/p042_words.txt"
raw <- readLines(word_file)
raw_words <- strsplit(raw,",")
words <- rep("",length(raw_words[[1]]))

for(i in 1:length(words)){
  # Pre-process string
  n <- raw_words[[1]][i]
  n <- tolower(gsub("\"","",n))
  words[i] <- n
}

# CALCULATE TRIANGLE NUMBERS
max_char <- max(nchar(words))
max_triangle <- max_char * 26
triangles <- c()
n <- 0
while(n <= max_triangle){
  n <- n + 1
  triangles <- c(triangles,0.5*n*(n+1))
}

# CALCULATE NAME SCORES AND COMPARE AGAINST TRIANGLES
word_cost <- rep(0,length(words))
for(i in 1:length(words)){
  chars <- strsplit(words[i],"")
  word_cost[i] <- sum(match(chars[[1]],letters))
}
inds <- which(word_cost %in% triangles)
print(length(word_cost[inds]))

