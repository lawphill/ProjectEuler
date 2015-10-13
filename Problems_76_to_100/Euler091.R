# The points P(x1, y1) and Q(x2, y2) are plotted at integer co-ordinates and are joined to the origin,
#   O(0,0), to form ΔOPQ.

# There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate
#   lies between 0 and 2 inclusive; that is,
#   0 ≤ x1, y1, x2, y2 ≤ 2.

# Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?

isRightTriangle <- function(x1,y1,x2,y2){
  # Let x1,y1 define point P and x2,y2 define point Q
  # Point O represents the origin at (0,0)
  # Let -1 represent a vertical slope
  if(x1 == x2 && y1 == y2){
    return(FALSE)
  }else if((x1==0 && y1 ==0) || (x2==0 && y2==0)){
    return(FALSE)
  }else if((x1==0 && y2==0) || (x2==0 && y1==0)){
    return(TRUE)
  }
  
  slopes <- c((y1-y2)/(x1-x2),
              y1/x1,
              y2/x2)
  slope_mat <- slopes %o% slopes
  slope_mat[is.nan(slope_mat)] <- -1 # Replace Inf*0 with 1, because that's a valid combination
  for(i in 1:length(slopes)){
    slope_mat[i,i] <- 0
  }

  if(any(slope_mat == -1)){
    return(TRUE)
  }else{
    return(FALSE)
  }   
}



minCoord <- 0
maxCoord <- 50

I = iterpc(maxCoord+1,2,replace=TRUE)
coords = getall(I) - 1

total <- 0
rights <- list()
for(i in 1:(nrow(coords)-1)){
  for(j in (i+1):nrow(coords)){
    x1 <- coords[i,1]
    x2 <- coords[j,1]
    y1 <- coords[i,2]
    y2 <- coords[j,2]
    
    if(isRightTriangle(x1,y1,x2,y2)){
      total <- total + 1
      rights[[total]] <- c(x1,x2,y1,y2)
    }
    
    x1 <- coords[i,2]
    x2 <- coords[i,1]
    if(isRightTriangle(x1,y1,x2,y2)){
      total <- total + 1
      rights[[total]] <- c(x1,x2,y1,y2)
    }
  }
}

results <- rep(0,20)
for(i in 2:20){

minCoord <- 0
maxCoord <- i

total <- 0
rights <- list()
for(x1 in minCoord:maxCoord){
  for(y1 in minCoord:maxCoord){
    for(x2 in minCoord:maxCoord){
      for(y2 in y1:maxCoord){
        if(isRightTriangle(x1,y1,x2,y2)){
          if(total == 0){
            total <- total + 1
            rights[[total]] <- c(x1,y1,x2,y2) 
          }else if(!any(sapply(1:total, function(x)
            all(rights[[x]]==c(x1,y1,x2,y2)) ||
              all(rights[[x]]==c(x2,y2,x1,y1))))){
            total <- total + 1
            rights[[total]] <- c(x1,y1,x2,y2)
          }
        }
      }
    }
  }
}
results[i] <- total
}