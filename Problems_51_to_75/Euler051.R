# By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
#   13, 23, 43, 53, 73, and 83, are all prime.

# By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first
#   example having seven primes among the ten generated numbers, yielding the family: 56003, 56113,
#   56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is
#   the smallest prime with this property.

# Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with
#   the same digit, is part of an eight prime value family.

library("combinat") # For combn function

# SOLUTION:
# GENERATE ALL PRIMES, REMOVE THOSE TOO SMALL OR WITH NO REPEATING DIGITS
# CHOOSE A NUMBER OF DIGITS TO REPLACE (less than the total number of digits)
# LOOK AT EACH POSSIBLE COMBINATION OF DIGITS TO REPLACE (e.g. digit 1 & 2, 1 & 3, 1 & 4, etc...)
# EXAMINE REMAINING DIGITS TO SEE IF THERE ARE AT LEAST 8 MATCHES
# IF THERE ARE, RECONSTRUCT THE SMALLEST PRIME FROM THAT SET AND BREAK
# solves in about 12 seconds


# GENERATE ALL PRIMES LESS THAN MAX_PRIMES
max_primes <- 999999
primes <- 1:max_primes
primes[1] <-0
for(i in 2:floor(max_primes/2)){
  if(primes[i] != 0){
    primes[seq.int(i*2,max_primes,i)] <- 0
  }
}
primes <- primes[primes!=0 & primes > 56993] # We know 56993 is too small

# CALCULATE DIGITS FOR EACH PRIME
max_digits <- nchar(max(primes))
digits <- matrix(data=0,nrow=length(primes),ncol=max_digits)
for(c in ncol(digits):1){
  digits[,c] = (primes %/% 10^(max_digits-c)) %% 10
}

found <- 0
nreplace <- 2 # number of digits we want to replace
family_size <- 8 # number of digit replacements that are primes
while(found == 0 & nreplace < max_digits){
  # Remove primes without (possibly) enough repeating digits
  primes <- primes[apply(digits,1,function(x) max(tabulate(x)) >= nreplace)]
  digits <- digits[apply(digits,1,function(x) max(tabulate(x)) >= nreplace),]
  
  print(nreplace)
  combs <- combn(1:max_digits,nreplace)
  
  for(i in 1:ncol(combs)){ # Pick a set of replacement digits    
    t<-apply(digits[,combs[,i]],1,function(x) length(unique(x))==1)
    s<-table(apply(digits[t,-(combs[,i])],1,function(x) sum(x*10^((max_digits-nrow(combs)-1):0))))

    if(length(s[s==family_size]) > 0){
      stable_num <- as.integer(names(s[s==family_size]))
      # In case we get more than one match, take the smallest
      if(length(stable_num)>1){ stable_num <- min(stable_num) }
      x <- strsplit(toString(stable_num),"")
      stable_digits <- as.integer(x[[1]])
      
      for(filled_value in 0:9){ # Reconstruct smallest prime in set
        poss_digits <- rep(0,max_digits)
        poss_digits[combs[,i]] <- filled_value
        poss_digits[-(combs[,i])] <- stable_digits
        poss_prime <- sum(poss_digits * 10^((max_digits-1):0))
        if(poss_prime %in% primes){
          found <- 1
          print(poss_prime)
          break
        }
      }
    }
    if(found==1){ break }
  }
  nreplace <- nreplace + 1
}