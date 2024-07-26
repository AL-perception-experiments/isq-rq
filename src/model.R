library(xgboost)

# model training
# input: labelled data (numerical array of nx7 elements in total, i.e. length divides by 7,
#           where n is the numper of questions and label comes AFTER cues: x1,x2,x3,x4,x5,x6,label) 
# output: 768 estimated probabilities

#input example
#input <-c(0,0,0,2,1,1,0,0,0,0,2,2,1,0,0,0,0,2,3,1,1)

input<-c(unlist(input))
# read input into matrix
n = length(input)/7
tmp<-matrix(0,n,7)
for (i in 1:n){
  tmp[i,] <-input[(1+7*(i-1)):(7+7*(i-1))]
}

D = as.matrix(tmp[,1:6])
y = as.numeric(tmp[,7])

#estimate a boosted tree
m1 <- xgboost(data = D, label = y, max.depth = 2, eta = 1, 
              nthread = 1, nrounds = 10, eval_metric = "logloss", objective = "binary:logistic",verbose = 0)

#let the model label all possible cue combinations
dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5,6))
colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
phat<- predict(m1,as.matrix(dx))

print(phat)