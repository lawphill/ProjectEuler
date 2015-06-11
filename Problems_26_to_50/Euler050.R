# The prime 41, can be written as the sum of six consecutive primes: 
#   41 = 2 + 3 + 5 + 7 + 11 + 13
# This is the longest sum of consecutive primes that adds to a prime below one-hundred.

# The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and
#   is equal to 953.

# Which prime, below one-million, can be written as the sum of the most consecutive primes?

# BRUTE FORCE SOLUTION, SOLVES IN ~3.8 SECONDS
# CALCULATE ALL PRIMES < 1000000
# LARGEST CONSECUTIVE SEQ OF PRIMES THAT SUM LESS THAN 1000000 IS 546, START THERE AND
#   REDUCE WINDOW SIZE UNTIL WE GET A MATCH

max_primes <- 999999
primes <- 1:max_primes
primes[1] <-0
for(i in 2:floor(max_primes/2)){
  if(primes[i] != 0){
    primes[seq.int(i*2,max_primes,i)] <- 0
  }
}
primes <- primes[primes!=0]

# MAX WINDOW SIZE IS 546, sum(primes[1:547]) > 999,999
window <- 546
max_window <- 0
while(window > 0){
  x<-mapply(function(x,y) sum(primes[x:y]), window:length(primes),1:(length(primes)-(window-1)))
  xprimes <- x[x %in% primes]
  if(length(xprimes) > 0){
    print(window)
    print(xprimes)
    break
  }
  window <- window - 1
}