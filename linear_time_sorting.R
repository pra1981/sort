#计数排序（输入数组中的元素全是整数时有效））
countsort <- function(a,k){
  c <- rep(0,k+1)
  b <- rep(0,length(a))
  for(i in 1:length(a)) c[a[i]+1] <- c[a[i]+1]+1    #计数过程
  for(j in 2:(k+1)) c[j] <- c[j]+c[j-1] #数在数组中正确排序时的位置
  for(j in length(a):1){    
    b[c[a[j]+1]] <- a[j]    #将a[j]放到输出数组b中的正确位置上
    c[a[j]+1] <- c[a[j]+1]-1  # a[i]和a[j]元素相同时,且i和j之间没有
    #与它们相等的元素,若i<j,则a[i]会放在a[j]的前一个位置上
  }     
  return(b)
}
a <- sample(1000000,100000,replace = T)
countsort(a,100000)==sort(a)
system.time(countsort(a,100000))

#基数排序
radixsort <- function(a,b,r){
  m=ceiling(b/r) 
  c <- rep(0,length(a))
  for(i in 1:m){
    e <- rep(0,10^r)
    d <- rep(0,length(a))     #暂时存储a按照前i*r位数排序后的数组
    f <- rep(0,length(a))     #暂时存储c数组正确排序后的数据组
    c <- (a%%10^(r*i)-c)%/%10^(r*i-r)
    for(j in 1:length(a)) e[c[j]+1] <- e[c[j]+1]+1 
    for(j in 2:10^r) e[j] <- e[j]+e[j-1]
    for(j in length(a):1){    
      d[e[c[j]+1]] <- a[j]
      f[e[c[j]+1]] <- c[j]
      e[c[j]+1] <- e[c[j]+1]-1 
    }
    c <- f    #c数组正确排序
    a <- d    #a数组按照前i*r位数排序
  }
  return(a)
}
a <- sample(10000,1000000,replace=T)
which(radixsort(a,5,2)!=sort(a))
system.time(radixsort(a,5,2))
system.time(countsort(a,10000))
system.time(sort(a))

#桶排序
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
bucketsort <- function(a){
  n <- length(a)
  b <- list()
  length(b) <- n
  for(i in 1:n){
   b[[ceiling(n*a[i])]][length(b[[ceiling(n*a[i])]])+1] <- a[i] 
  }
  
  for(i in 1:n){
    if(length(b[[i]])>1) b[[i]] <- sortf(b[[i]])
  }
  a <- unlist(b)
  return(a)
}
a <- c(.78,.17,.39,.26,.72,.94,.21,.12,.23,.68)
bucketsort(a)
b <- runif(100)
bucketsort(b)
