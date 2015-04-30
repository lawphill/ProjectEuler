# Find nth Fibonacci number

fib1 <- function(n){
  if(n==0) return(1)
  else if(n==1) return(1)
  else {
    return(fib1(n-1)+fib1(n-2))
  }
}

fib2 <- function(n){
  fibs <- c(0,1)
  if(n>1){
    for(i in 3:n){
      fibs[i] = fibs[i-1]+fibs[i-2]
    }
  }
  return(fibs[n])
}

knapsack <- function(w,v,W){
  A <- matrix(rep(0,(W+1) * (length(w)+1)),ncol=W+1)
  for(j in 1:length(w)){
    for (Y in 1:W){
      if(w[j] > Y)
        A[j+1,Y+1]=A[j,Y+1]
      else
        A[j+1, Y+1]=max( A[j, Y+1], v[j] + A[j, Y - w[j]+1])
    }
  }
  return(A)
}