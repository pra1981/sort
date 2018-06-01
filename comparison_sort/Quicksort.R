#数组划分
partition <- function(a,p,r){
  x <- a[r]
  i <- p-1
  for(j in p:(r-1)){
    if(a[j]<=x){
      i <- i+1
      s <- a[i]
      a[i] <- a[j]
      a[j] <- s
    }
  }
  t <- a[i+1]
  a[i+1] <- a[r]
  a[r] <- t
  z <- list(a,i+1)
  return(z)
}
a <- c(2,8,7,4,3,5,6,4)
partition(a,1,8)[[1]]
#快速排序
quicksort <- function(a,p,r){
  if(p<r){
    t <- partition(a,p,r)
    a <- t[[1]]
    q <- t[[2]]
    a <- quicksort(a,p,q-1)
    a <- quicksort(a,q,r)
  }
  return(a)
}
a <- rnorm(100000,0,100000)
a <- partition(a,1,100)[[1]]
partition(a,1,100000)
quicksort(a,1,1000)

#随机化快速排序
randomizedpar <- function(a,p,r){
  e <- sample(p:r,1)
  w <- a[e]
  a[e] <- a[r]
  a[r] <- w
  x <- a[r]
  i <- p-1
  for(j in p:(r-1)){
    if(a[j]<=x){
      i <- i+1
      s <- a[i]
      a[i] <- a[j]
      a[j] <- s
    }
  }
  t <- a[i+1]
  a[i+1] <- a[r]
  a[r] <- t
  z <- list(a,i+1)
  return(z)
}
randomizedqs <- function(a,p,r){
  if(p<r){
    t <- randomizedpar(a,p,r)
    a <- t[[1]]
    q <- t[[2]]
    a <- randomizedqs(a,p,q-1)
    a <- randomizedqs(a,q,r)
  }
  return(a)
}
a <- runif(100,0,100)
randomizedqs(a,1,100)
quicksort(a,1,100)==randomizedqs(a,1,100)

#尾递归快速排序(tail recursive quicksort)
trq <- function(a,p,r){
  while(p<r){
    t <- partition(a,p,r)
    a <- t[[1]]
    q <- t[[2]]
    a <- trq(a,p,q-1)
    p <- q+1
  }
  return(a)
}
trq(a,1,100)
system.time(trq(a,1,100))
