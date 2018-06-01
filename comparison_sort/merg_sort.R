##µİ¹éËã·¨
combine <- function(l,r){
  n1 <- length(l)
  n2 <- length(r)
  n <- n1+n2
  vector <- c()
  i=1
  j=1
  for(k in 1:n){
    if(i<=n1&&j<=n2){
      if(l[i] <= r[j]){
        vector[k] <- l[i] ; i <- i+1}  else {vector[k] <- r[j] ;  j <- j+1}
    }else if(i>n1&&j<=n2){
      vector <- append(vector,r[j:n2]); break
    }else {
      vector <- append(vector,l[i:n1]) ;break
    }}
  return(vector)
}
mergefun <- function(a){
  len <- length(a)
  if(len==1){
    return(a)
  }else{
    q <- floor((len+1)/2)
    l <- a[1:q]
    r <- a[(q+1):len]
    l <- mergefun(l)
    r <- mergefun(r)
    a <- combine(l,r)
  }
  return(a)
}