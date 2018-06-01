
##²åÈëÅÅĞòËã·¨
a <- c(5,1,3,4)
a
sortf <- function(a){
  for(j in 2:length(a))
  {
    key <- a[j]
    i <- j-1
    while(i>0 && a[i]>key){
      a[i+1] <- a[i];
      i <- i-1}
    a[i+1] <- key 
  }
  return(a)
}
sortf(a)
a <- rnorm(1000,1,10)
sortf(a)
system.time(sortf(a))


a <- rnorm(10000,1,10)
mergefun(a)
system.time(mergefun(a))
system.time(sortf(a))