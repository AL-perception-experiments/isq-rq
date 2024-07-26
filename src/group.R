# group identification
# input: numeric array of labels
# output: group (0 or 1)
y = c(unlist(input))

# if the relative difference is large (greater or equal to 0.75) 
# that means the listener does not take into account factors 5 and 6
# otherwise the listener is classified as group 2

idg= function(y){
  dif = abs(sum(y==0)- sum(y==1))/length(y)
  group = ifelse(dif>=0.75,1,0)
  return(group)
}
# first we classify 
idx = idg(y)

print(idx)