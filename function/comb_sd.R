# For each group :
# Σx = mean * n; :x
# Σx^2 = SD^2(n-1)+((Σx)^2/n) : x.s
# The values are then added together
# tn = sum of all (n)
# tx = sum of all Σx
# txx = sum of all Σx^2 
# The combine calculations are
# Combined n = tn
# Combined mean = tx / tn
# Combine SD = sqrt((txx-tx^2/tn) / (tn-1)) 

comb_sd <- function(mean,n,sd){
  
  
  # For each group
  # mean * n
  x = mean*n
  # SD^2(n-1)+((Σx)^2/n)
  x.s = (sd^2)*(n-1)+(x^2)/n
  # The values are then added together
  
  
  sd.c=sqrt((sum(x.s)-sum(x)^2/sum(n))/(sum(n)-1))
  return(sd.c)
}
  
  
  
  
