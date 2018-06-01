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
randomselect <- function(a,p,r,i){
  if(p==r) return(a[p])
  c<- partition(a,p,r)[[1]]
  q <- partition(a,p,r)[[2]]   
  k <- q-p+1
  if(i==k) return(c[q])
  if(i<k){
    a <- c[p:(q-1)]
    b <- randomselect(a,1,q-p,i)
    return(b) } 
  if(i>k) {
    a <- c[(q+1):r]
    d <- randomselect(a,1,r-q,i-k) 
    return(d)}
}
a <- runif(100)
b <- vector()
for(i in 1:length(a)){
  b[i] <- randomselect(a,1,length(a),i)
}
b == sort(a)


