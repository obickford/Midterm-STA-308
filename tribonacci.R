############################################

Tribonacci <- function(a1,a2,a3,min,max){
  a4 <- a1 + a2 + a3
  resultVector <- c(a1,a2,a3)
  while((a4 >= min)&(a4 <= max)){ ##restricting the sequence to in between the set min and max
    resultVector <- c(resultVector,a4)
    a1 <- a2
    a2 <- a3
    a3 <- a4
    a4 <- a1 + a2 + a3
  }
  
  print(resultVector)
  
  print(length(resultVector))
}

Tribonacci(0,0,1,0,30)
Tribonacci(1,-3,2,-3,44)
Tribonacci(1,-3,2,0,2)

############################################

graph_trib <- c()
for(i in 2:10000) {
  graph_trib <- c(graph_trib, Tribonacci_max(1,-3,2,i))
}
plot(graph_trib)

############################################







