
#最大子数组
##递归法
fmcs <- function(a,low,mid,high){
  leftsum <- -1/0
  sum=0
  maxleft <- mid
  for(i in mid:low){
    sum <- sum+a[i]
    if(sum>leftsum){
      leftsum <- sum
      maxleft <- i
    }
  }
  rightsum <- -1/0
  sum=0
  maxright <- mid
  for(j in mid:high){
    sum <- sum+a[j]
    if(sum>rightsum){
      rightsum <- sum
      maxright <- j
    }
  }
  b <- c(maxleft,maxright,leftsum+rightsum-a[mid])
  return(b)
}
fms <- function(a,low,high){
  if(high==low){e <- c(low,high,a[low]);return(e)}
  else mid <- floor((low+high)/2)
  c <- fms(a,low,mid)
  d <- fms(a,mid+1,high)
  b <- fmcs(a,low,mid,high)
  if(c[3]>=d[3]&&c[3]>=b[3]) return(c)
  else if(d[3]>=c[3]&&d[3]>=b[3]) return(d)
  else return(b)
}
a <- rnorm(100,0,1)
fmcs(a,1,50,100)
##DP求解
ms <- function(a,low,high){
  b <- c(low,low,a[low])
  high1 <- high-1
  for(i in low:high1){
    t <- i+1
    sumlj <- -1/0
    sum <- 0 
    for(j in t:low){
      sum <- sum+a[j]
      if(sum>sumlj){sumlj <- sum;low1 <- j}
      c <- c(low1,t,sumlj)
    }
    if(c[3]>b[3])  b<- c 
  }
  return(b)
}  
a <- rnorm(100,0,1)
b <- ms(a,1,100)
c <- fms(a,1,100)
sum(a[b[1]:b[2]])
sum(a[c[1]:c[2]])