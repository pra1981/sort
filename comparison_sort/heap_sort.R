#���ѽ�����½�������ά��
max_heapify <- function(a,i,n){
  left <- 2*i
  right <- 2*i+1
  if(i<=n/2){
    if(left<=n&&a[left]>a[i]) largest <- left else largest <- i
    if(right<=n&&a[right]>a[largest]) largest <- right
    if(largest!=i) {  
      q <- a[i]
      a[i] <- a[largest]
      a[largest] <- q
      a <- max_heapify(a,largest,n) 
    }
  }
  return(a)
}
#����e�Ľ���
build_max_heap <- function(a){
  for(i in floor(length(a)/2):1)
    a <- max_heapify(a,i,length(a))
  return(a)
}
a <- rnorm(100)
a <- build_max_heap(a)
a==max_heapify(a,5,100)
#����ǰ��������������������ѽ�������
heapsort <- function(a){
  a <- build_max_heap(a)
  for(i in length(a):2){
    q <- a[i]
    a[i] <- a[1]
    a[1] <- q
    a <- max_heapify(a,1,i-1)
  }
  return(a)
}
a <- heapsort(a)
a[heapsort(a)!=sort(a)]

#������ȶ��У�����ʵ�����ȶ��У�
heap_max <- function(a){
  return(a[1])
}
heap_max(a)
formals(heap_max)
body(heap_max)
environment(heap_max)
heap_extract_max <- function(a){
  i <- length(a)
  max <- a[1]
  q <- a[i]
  a[i] <- a[1]
  a[1] <- q
  a <- max_heapify(a,1,i-1)
  return(max)
}   ##��ȡ���ֵ
heap_extract_max(a)
heap_increase_key <- function(a,i,key){
  if(key<a[i])  print('new key is smaller than current key')
  a[i] <- key
  t <- floor(i/2)
  while(i>1&&a[t]<a[i]){
    q <- a[i]
    a[i] <- a[t]
    a[t] <- q
    i <- t
    t <- floor(i/2)
  }
  return(a)
}   ##���ӹؼ��ֵ����ȶ���
a <- rnorm(100)
a <- build_max_heap(a)
heap_increase_key(a,20,5)
max_heap_insert <- function(a,key){
  n <- length(a)
  a[n+1] <- -1/0
  a <- heap_increase_key(a,n+1,key)
  return(a)
}   #����һ���ؼ��ֲ��������޶���
max_heap_insert(a,10)
build_max_heap1 <- function(a){
  b <- a[1:(i-1)]
  for(i in 2:length(a)){
    b <- max_heap_insert(b,a[i])
  }
  return(b)
}  ##����max_heap_insect��һ�����з�������Ԫ��
build_max_heap(build_max_heap1(a))==build_max_heap1(a)  #˵��build_max_heap1ȷʵ������һ����
build_max_heap(a)==build_max_heap1(a) #build_max_heap��build_max_heap1���ַ��������Ķ��ǲ�һ����