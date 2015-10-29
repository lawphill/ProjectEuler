# By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively,
#   we form a square number: 1296 = 362. What is remarkable is that, by using the
#   same digital substitutions, the anagram, RACE, also forms a square number:
#   9216 = 962. We shall call CARE (and RACE) a square anagram word pair and specify
#   further that leading zeroes are not permitted, neither may a different letter
#   have the same digital value as another letter.

# Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
#   containing nearly two-thousand common English words, find all the square
#   anagram word pairs (a palindromic word is NOT considered to be an anagram of
#   itself).

# What is the largest square number formed by any member of such a pair?
#   NOTE: All anagrams formed must be contained in the given text file.

filename <- "data/p098_words.txt"
data <- read.csv(filename,header=FALSE,sep=",",colClasses="character")
word_lengths <- nchar(data)
max_length <- max(word_lengths)
word_chars <- lapply(1:length(word_lengths),
                     function(x) unlist(strsplit(as.character(data[x]),split="")))


for(n in max_length:2){
  word_list <- word_chars[word_lengths==n]
  sorted_words <- sapply(1:length(word_list),
                        function(x) paste(sort(word_list[[x]]),collapse=""))
  comparison <- outer(sorted_words,sorted_words,FUN="==")
  diag(comparison) <- FALSE
  
  anagram_indices <- which(comparison,arr.ind=TRUE)
  if(any(anagram_indices)){
    word <- word_list[[anagram_indices[1,1]]]
    #word2 <- word_list[[anagram_indices[1,2]]]
    unique_nums <- length(unique(word))
    
    
    print(word)
    print(word2)
    break
  }
  
  
}