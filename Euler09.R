# Find the pythagorean triplet (a^2 + b^2 = c^2) where a+b+c = 1000
# Return the product of a*b*c

# This approach is super ugly... but it works

for(a in 1:floor(1000/3-1)){
  for(b in a+1:floor(((1000-a)/2)-1)){
    c <- (1000 - a) - b
    if(a<b && b<c){
      if(a^2 + b^2 == c^2){
        print(a*b*c)
      }
    }else{
      #print(c(a,b,c))
    }
  }
}