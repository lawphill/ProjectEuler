# ANY UNIT RECTANGLE CAN BE DIVIDED INTO A FINITE NUMBER OF SMALLER RECTANGLES
#   FOR EXAMPLE, A 3x2 RECTANGLE CAN BE DIVIDED INTO 18 RECTANGLES
# NO RECTANGLE IS DIVIDED INTO EXACTLY 2,000,000 BUT FIND THE CLOSEST SOLUTION

# SOLVES IN ~5 SECONDS

subdivide <- function(x,y){
  # TAKE RECTANGLE SIDES AND RETURN NUMBER OF SUBDIVISIONS POSSIBLE
  s <- x*y
  if(x >= 2){
    for(i in 1:(x-1)){
      s <- s + i*y
    }
  }
  if(y >= 2){
    for(j in 1:(y-1)){
      s <- s + j*x
    }
  }
  if(x >= 2 & y >= 2){
    for(i in 1:(x-1)){
      for(j in 1:(y-1)){
        s <- s + i*j
      }
    }
  }
  
  return(s)
}

target <- 2000000
distance <- target
x <- 1
y <- x
s <- subdivide(x,y)
while(s < target){
  y <- y + 1
  s <- subdivide(x,y)
  if(abs(s-target) < distance){
    distance <- abs(s - target)
    best_x <- x
    best_y <- y
  }
}

for(x in 2:100){
  s <- subdivide(x,y)
  
  while(s > target){
    y <- y - 1
    s <- subdivide(x,y)
    if(abs(s-target) < distance){
      distance <- abs(s-target)
      best_x <- x
      best_y <- y
    }
  }
}

print(c(best_x,best_y,best_x*best_y))