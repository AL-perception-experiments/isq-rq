# labelling funciton
# input: 1. vector of cue vlaues x with 6 elements (see cue coding below)
#        2. group identifier 0 or 1
#        3. seed (random integer, unique for each data instance)
# in summary input is an array of 8 elements: 
input <- c(unlist(input))
x = input[1:6]
id = input[7]
seed = input[8]
# output: binary label

#cue coding:
#Question type: 2 levels, coded as {0,1}, denoted as x1
#Voice quality: 2 levels, coded as {0,1}, denoted as x2
#Duration: 2 levels, coded as {0,1}, denoted as x3
#Prenuclear: 4 levels, coded as {1,2,3,4}, denoted as x4
#Edge tone: 4 levels, coded as {1,2,3,4}, denoted as x5
#Nuclear: 6 levels, coded as {1,2,3,4,5,6}, denoted as x6

# a function which returns 0/1 label depending on listener group (aka virtual agent)
#p = function(x,id,seed){}

if (id==1){
  # group 1 of listeners does not take into account edge tone and nuclear (x5 and x6)
  z1 = -3.37 + 2.94*x[1] + 1.91*x[2] + 1.51*x[3] -0.4*x[1]*x[2]*x[3] +0.2*x[4]-0.9*x[4]*x[1] -0.44*x[2]*x[3]
  pr  = plogis(z1)
} else {
  
  #group 2 of listeners does not take into account factor 2 
  z2 = -3.37 + 2.94*x[1] + 1.91*x[2] + 1.51*x[6] -0.4*x[5]*x[6] +0.2*x[4]-0.9*x[4]*x[1] -0.44*x[2]*x[3]
  pr  = plogis(z2)
}
set.seed(seed)
l = rbinom(1,1,pr)

print(l)



