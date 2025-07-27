# Mustafa Anjrini on 26.07.2025

rm(list = ls()) # deleting all the variables

newton_raphson<- function(x){ # the input can be a vector of a values in order to get the convergance of different values
  
  f<-function(x){ 
    return(x^3+4*x^2-x-4) # the function to be considered. Please change it accordingly
  }
  
  f_p<-function(x){
    return(3*x^2+8*x-1) # the derivative of the function to be considered. Please change it accordingly
  }
  # below is the implementation of the method
  for (i in 1:1000) {
    dx<- -f(x)/f_p(x)
    x.old<-x
    x<-x+dx
    x.new=x
    z=0
    for (i in 1:length(x)) {
      if (abs(x.new[i]-x.old[i])<0.001) {
       z=z+1 
      }
    }
    if (z==length(x)) {
      break
    }
    z=0
    
  }
  return(x)
}

# let's give starting values
x<-c(-10,-2,5)

# applying the function
newton_raphson(x)





