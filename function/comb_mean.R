# To combine means for groups
# Better to use it with group_by and summarize function

comb_mean <- function(mean,n){
  
  mean.c <- sum(mean*n)/sum(n)
  
  return(mean.c)
  
}