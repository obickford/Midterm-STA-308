#####
## 
##  Midterm - Take Home
##  Olivia Bickford
##  May 1, 2022
## 
##  For this midterm assignment there is a function that represents
##   the Tribonacci sequence. In part 1 there is a function with 5 
##   parameters. You input three starting values and a minimum and a 
##   maximum. From here the function will preform the sequence by adding
##   up the first three terms and you get the next. It continues to take
##   the previous three numbers to get the next in the sequence. To keep it within
##   the min and max values, the nth value is restricted to be inbetween the set
##   min and max values. The result is the sequence with the starting values and 
##   restricted to the minimum and the maximum. Below, it gives the sequence length. 
##
##  In part 2, by setting the max to values 2-10000, there were the sequences were  
##   generated for three set values (1,-3,2) and the sequence lengths as well. With
##   these lengths, they were graphed in order to look at the distribution and what
##   the graph looked like. The plot uses the maximum value of the sequence and the
##   and the length of the sequence. 
##
##

######
## Code sequence for part 1
######

Tribonacci <- function(a1=0,a2=0,a3=1,min=0,max=30){
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

Tribonacci()
Tribonacci(a1=1,a2=-3,a3=2,min=-3,max=44)
Tribonacci(a1=0,a2=-4,a3=2,min=-60,max=30)

#######
## Part 2 Code
#######

Tribonacci_max_values <- function(a1,a2,a3,max){
  a4 <- a1 + a2 + a3
  resultVector <- c(a1,a2,a3)
  while(a4 <= max){ ##restricting the sequence to in between the set min and max
    resultVector <- c(resultVector,a4)
    a1 <- a2
    a2 <- a3
    a3 <- a4
    a4 <- a1 + a2 + a3
  }
  length(resultVector)
}

graph_trib <- c()
for(i in 2:10000) {
  graph_trib <- c(graph_trib, Tribonacci_max_values(1,-3,2,i))
}
plot(graph_trib, main = "Maximum Value Compared to Length of Sequence",
     xlab= "maximum value of sequence", ylab= "length of the sequence")

## This graph has the shape of an exponential function, however it is broken up
##   into pieces and each step gets longer and longer as the maximum value gets 
##   higher. 

## Source on how to use the plot function:
##   https://www.datamentor.io/r-programming/plot-function/









