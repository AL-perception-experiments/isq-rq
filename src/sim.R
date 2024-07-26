#install.packages("xgboost")
#install.packages("DiagrammeR")
library(xgboost)

# set working directory
setwd("C:/Users/Ekaterina/Dropbox/Marieke/! big")

# This line removes all variables from the workspace.
rm(list = ls())

# x1: q_type 2
# x2: vq 2
# x3: dur 2
# x4: prenuc 4
# x5: edge_tone 4
# x6: nuc 6

# a function which returns 0/1 label depending on listener group (aka virtual agent)
p = function(x,id,seed){
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
  return(l)
}

####################################################
# the first individual separately to see the mechanics
####################################################
#### assume we know that individuals from group 1 do not rely on x5 and x6
# in order to identify which group does the individual belong to we let the individual label 24 questions
# 4 pairs of same cues, where the only thing which changes is factor 2, other cues are fixed 

d0<-expand.grid(0, 0,0,2,c(1,2,3,4),c(1,2,3,4,5,6))
#d0 = kronecker(matrix(1,mult,1),as.matrix(q0))
colnames(d0) <- c("x1","x2","x3","x4","x5","x6")

#assume the first individual is from group
group0 = 1

y<-0
for (i in 1:dim(d0)[1]){
  y[i] = p(as.numeric(d0[i,]),group0,i)
}
# if the individual does not take into account factors 5 and 6 for classification then the labels should be the same
# i.e. to identify the group we count the relative difference in the number of 0 and 1 labels
dif = abs(sum(y==0)- sum(y==1))/length(y)

# if the relative difference is large (greater or equal to 0.75) that means the listener does not take into account factors 5 and 6
# otherwise the listener is classified as group 2

idg= function(y){
  dif = abs(sum(y==0)- sum(y==1))/length(y)
  group = ifelse(dif>=0.75,1,2)
  return(group)
}
# first we classify 
idx = idg(y)
# we then train the boosted tree for d1 or d2 based on the group identifier
D1 = NULL
D2 = NULL
Y1 = NULL
Y2 = NULL
if (idx==1){
  # group 1 of listeners does not take into account factors 3 and 6
  D1 = rbind(D1,as.data.frame(d0))
  m1 <- xgboost(data = as.matrix(D1[,1:6]), label = as.numeric(rbind(Y1,y)), max.depth = 2, eta = 1, 
                nthread = 1, nrounds = 10, eval_metric = "logloss", objective = "binary:logistic",verbose = 0)

  #let the model label all possible cue combinations
  dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5,6))
  colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
  dx$phat<- predict(m1,as.matrix(dx))
  # select the questions with the closest estimated probability to 0.5
  dx$dist = abs(dx$phat-0.5)
  dnext = dx[dx$dist==min(dx$dist),]
  # we then randomly select a question from dnext to be labeled by the user such that the user in total labells 64 questions
  nstop = 40
  temp = D1[,1:6]
  for (i in 1:nstop){
    #new question
    qn = dnext[sample(nrow(dnext), 1),1:6]
    #label new question
    y = c(y,p(as.numeric(qn),group0,i+24))
    #train the model on additional data
    temp<-rbind(temp,qn)
    m1 <- xgboost(data = as.matrix(temp), label = y, max.depth = 2, eta = 1, nthread = 1, nrounds = 10, 
                  eval_metric = "logloss",objective = "binary:logistic",verbose = 0)
    #let the model label all possible cue combinations
    dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3,4))
    colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
    dx$phat<- predict(m1,as.matrix(dx))
    # select the questions with the closest estimated probability to 0.5
    dx$dist = abs(dx$phat-0.5)
    dnext = dx[dx$dist==min(dx$dist),]
  }
  # save temp as the labelled data after this user
  D1 = temp
  Y1 = y
} else {
  D2 = rbind(D1,as.data.frame(d0))
  m2 <- xgboost(data = as.matrix(D2[,1:6]), label = as.numeric(rbind(Y2,y)), max.depth = 2, eta = 1, 
                nthread = 1, nrounds = 10, eval_metric = "logloss", objective = "binary:logistic",verbose = 0)
  
  #let the model label all possible cue combinations
  dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5,6))
  colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
  dx$phat<- predict(m2,as.matrix(dx))
  # select the questions with the closest estimated probability to 0.5
  dx$dist = abs(dx$phat-0.5)
  dnext = dx[dx$dist==min(dx$dist),]
  # we then randomly select a question from dnext to be labeled by the user such that the user in total labells 64 questions
  nstop = 40
  temp = D2[,1:6]
  for (i in 1:nstop){
    #new question
    qn = dnext[sample(nrow(dnext), 1),1:6]
    #label new question
    y = c(y,p(as.numeric(qn),group0,i+24))
    #train the model on additional data
    temp<-rbind(temp,qn)
    m1 <- xgboost(data = as.matrix(temp), label = y, max.depth = 2, eta = 1, nthread = 1, nrounds = 10, 
                  eval_metric = "logloss",objective = "binary:logistic",verbose = 0)
    #let the model label all possible cue combinations
    dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3,4))
    colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
    dx$phat<- predict(m1,as.matrix(dx))
    # select the questions with the closest estimated probability to 0.5
    dx$dist = abs(dx$phat-0.5)
    dnext = dx[dx$dist==min(dx$dist),]
  }
  # save temp as the labelled data after this user
  D2 = temp
  Y2 = y
}
# so after each user we get new 64 rows in either D1 or D2

####################################################
# other participants in a loop
####################################################
# simulate other n users coming with 50%-50% chance
n = 49

for (j in 1:n){
  set.seed(j)
  #assume the first individual is from group
  group0 = rbinom(1,1,0.5)+1
  
  y<-0
  for (i in 1:dim(d0)[1]){
    y[i] = p(as.numeric(d0[i,]),group0,j*10+i)
  }
  # first we classify 
  idx = idg(y)

  if (idx==1){
    # group 1 of listeners does not take into account factors 3 and 6
    D1 = rbind(D1,as.data.frame(d0))
    y = c(Y1,y)
    m1 <- xgboost(data = as.matrix(D1[,1:6]), label = as.numeric(y), max.depth = 2, eta = 1, 
                  nthread = 1, nrounds = 10, eval_metric = "logloss", objective = "binary:logistic",verbose = 0)
    
    #let the model label all possible cue combinations
    dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5,6))
    colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
    dx$phat<- predict(m1,as.matrix(dx))
    # select the questions with the closest estimated probability to 0.5
    dx$dist = abs(dx$phat-0.5)
    dnext = dx[dx$dist==min(dx$dist),]
    # we then randomly select a question from dnext to be labeled by the user such that the user in total labells 64 questions
    nstop = 40
    temp = D1[,1:6]
    for (i in 1:nstop){
      #new question
      qn = dnext[sample(nrow(dnext), 1),1:6]
      #label new question
      y = c(y,p(as.numeric(qn),group0,j*10+i+24))
      #train the model on additional data
      temp<-rbind(temp,qn)
      m1 <- xgboost(data = as.matrix(temp), label = y, max.depth = 2, eta = 1, nthread = 1, nrounds = 10, 
                    eval_metric = "logloss",objective = "binary:logistic",verbose = 0)
      #let the model label all possible cue combinations
      dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3,4))
      colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
      dx$phat<- predict(m1,as.matrix(dx))
      # select the questions with the closest estimated probability to 0.5
      dx$dist = abs(dx$phat-0.5)
      dnext = dx[dx$dist==min(dx$dist),]
    }
    # save temp as the labelled data after this user
    D1 = temp
    Y1 = y
  } else {
    D2 = rbind(D2,as.data.frame(d0))
    y = c(Y2,y)
    m2 <- xgboost(data = as.matrix(D2[,1:6]), label = as.numeric(y), max.depth = 2, eta = 1, 
                  nthread = 1, nrounds = 10, eval_metric = "logloss", objective = "binary:logistic",verbose = 0)
    
    #let the model label all possible cue combinations
    dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5,6))
    colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
    dx$phat<- predict(m2,as.matrix(dx))
    # select the questions with the closest estimated probability to 0.5
    dx$dist = abs(dx$phat-0.5)
    dnext = dx[dx$dist==min(dx$dist),]
    # we then randomly select a question from dnext to be labeled by the user such that the user in total labells 64 questions
    nstop = 40
    temp = D2[,1:6]
    for (i in 1:nstop){
      #new question
      qn = dnext[sample(nrow(dnext), 1),1:6]
      #label new question
      y = c(y,p(as.numeric(qn),group0,j*10+i+24))
      #train the model on additional data
      temp<-rbind(temp,qn)
      m1 <- xgboost(data = as.matrix(temp), label = y, max.depth = 2, eta = 1, nthread = 1, nrounds = 10, 
                    eval_metric = "logloss",objective = "binary:logistic",verbose = 0)
      #let the model label all possible cue combinations
      dx = expand.grid(c(0,1), c(0,1),c(0,1),c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3,4))
      colnames(dx) <- c("x1","x2","x3","x4","x5","x6")
      dx$phat<- predict(m1,as.matrix(dx))
      # select the questions with the closest estimated probability to 0.5
      dx$dist = abs(dx$phat-0.5)
      dnext = dx[dx$dist==min(dx$dist),]
    }
    # save temp as the labelled data after this user
    D2 = temp
    Y2 = y
  }
}

#analyse the model

names <- c("x1","x2","x3","x4","x5","x6")
xgb.importance(names, model = m1)
xgb.plot.tree(model = m1)

xgb.importance(names, model = m2)
xgb.plot.tree(model = m2)

# collect everything in one dataframe
df = rbind(D1,D2)
df$y = c(Y1,Y2)
#overall forest
m <- xgboost(data = as.matrix(df[,1:6]), label = df$y, max.depth = 3, eta = 1, nthread = 1, nrounds = 1, 
             eval_metric = "logloss",objective = "binary:logistic",verbose = 0)
xgb.importance(names, model = m)
xgb.plot.tree(model = m)


fit = glm(y~(x1+x2+x3+x4+x5+x6)^2,family = binomial(link = "logit"),data = df)
summary(fit)
library(mfx)
tb = logitmfx(y~(x1+x2+x3+x4+x5+x6)^2,data=df,atmean = TRUE)
round(tb$mfxest,2)
