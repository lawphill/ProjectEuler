# Calculate largest prime factor of 600851475143 
# Go through factors largest to smallest, check primeness
x <- 600851475143
num <- floor(sqrt(x))
found <- 0

while(num > 1 && found==0)
  if((x %% num)%%1==0){
    # Check if prime
    num2 <- num - 1
    found2 <- 0
    while(num2 > 1 and found2==0){
      
    }
  }else{
    num <- num - 1
  }
}