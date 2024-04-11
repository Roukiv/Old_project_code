setwd("C:/Users/Ivor Kuan/Desktop/Exeter/MTHM017/assignment/B")
cls <- read.csv("Classification.csv")
cls <- cls[,-1]
clst <- read.csv("ClassificationTrue.csv")
clst <- clst[,-1]
###########################################

# 1

require(tidyverse)
g1 <- filter(cls, Group == 1)
g2 <- filter(cls, Group == 2)
summary(g1[,-3])
summary(g2[,-3])

ggplot(cls, aes(X1,X2,colour=factor(Group))) + geom_point()
ggplot(clst, aes(X1,X2,colour=factor(Group))) + geom_point() 

###########################################

# 2

require(caret)

set.seed(25908723) # for reproducibility
ind <- sample(2, nrow(cls),
              replace = TRUE,
              prob = c(0.75, 0.25))
training <- cls[ind==1,]
testing <- cls[ind==2,]

###########################################

# 3 & 4

require(MASS)

# repeat 10000 times
# precision : positive predictive value (true positive out of positive)
# NVP       : negative predictive value (true negative out of negative)
# sensitivity : true positive out of tp + fn
# specificity : true negative out of tn + fp
# accuracy    : true out of all

# balanced accuracy : ( sensitivity + specificity ) / 2
# Unbalance data cause problem, so "balanced accuracy" can fix the problem

# Holdout cross validation is used for every method to prevent unfair advantage
# performance on test is more important than train

# mainly look at the mean of each parameters, e.g sensitivity, specificity.

###########################################

# LDA

# perfoemance in 1000 runs
lda.ac <- data.frame(tr.pre = c(rep(NA,1000)), te.pre = c(rep(NA,1000)), 
                     tr.sen = c(rep(NA,1000)), te.sen = c(rep(NA,1000)),
                     tr.spe = c(rep(NA,1000)), te.spe = c(rep(NA,1000)),
                     tr.npv = c(rep(NA,1000)), te.npv = c(rep(NA,1000)),
                     tr.acc = c(rep(NA,1000)), te.acc = c(rep(NA,1000)))
for (i in 1:1000){
  ind <- sample(2, nrow(cls),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- cls[ind==1,]
  testing <- cls[ind==2,]
  ldam1 <- lda(Group ~ X1 + X2, data = training)
  
  # Confusion matrix
  # for training
  t1 <- table(testing$Group,predict(ldam1,testing)$class)
  lda.ac$tr.pre[i] <- precision(t1)
  lda.ac$tr.sen[i] <- sensitivity(t1)
  lda.ac$tr.spe[i] <- specificity(t1)
  lda.ac$tr.npv[i] <- negPredValue(t1)
  lda.ac$tr.acc[i] <- sum(diag(t1))/sum(t1)
  
  # for testing
  t1a <- table(clst$Group,predict(ldam1,clst)$class)
  lda.ac$te.pre[i] <- precision(t1a)
  lda.ac$te.sen[i] <- sensitivity(t1a)
  lda.ac$te.spe[i] <- specificity(t1a)
  lda.ac$te.npv[i] <- negPredValue(t1a)
  lda.ac$te.acc[i] <- sum(diag(t1a))/sum(t1a)
  
  print(i)
}

summary(lda.ac) 

mean(0.5*(lda.ac$tr.sen+lda.ac$tr.spe), na.rm=TRUE) # 0.7185071
var(0.5*(lda.ac$tr.sen+lda.ac$tr.spe), na.rm=TRUE) # 0.009324861

mean(0.5*(lda.ac$te.sen+lda.ac$te.spe), na.rm=TRUE) # 0.714513
var(0.5*(lda.ac$te.sen+lda.ac$te.spe), na.rm=TRUE) # 0.0001757127


###########################################

# QDA

qda.ac <- data.frame(tr.pre = c(rep(NA,1000)), te.pre = c(rep(NA,1000)), 
                     tr.sen = c(rep(NA,1000)), te.sen = c(rep(NA,1000)),
                     tr.spe = c(rep(NA,1000)), te.spe = c(rep(NA,1000)),
                     tr.npv = c(rep(NA,1000)), te.npv = c(rep(NA,1000)),
                     tr.acc = c(rep(NA,1000)), te.acc = c(rep(NA,1000)))

for (i in 1:1000){
  ind <- sample(2, nrow(cls),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- cls[ind==1,]
  testing <- cls[ind==2,]
  qdam1 <- qda(Group ~ X1 + X2, data = training)
  
  # Confusion matrix
  # for training
  t1 <- table(testing$Group,predict(ldam1,testing)$class)
  qda.ac$tr.pre[i] <- precision(t1)
  qda.ac$tr.sen[i] <- sensitivity(t1)
  qda.ac$tr.spe[i] <- specificity(t1)
  qda.ac$tr.npv[i] <- negPredValue(t1)
  qda.ac$tr.acc[i] <- sum(diag(t1))/sum(t1)
  
  # for testing
  t1a <- table(clst$Group,predict(ldam1,clst)$class)
  qda.ac$te.pre[i] <- precision(t1a)
  qda.ac$te.sen[i] <- sensitivity(t1a)
  qda.ac$te.spe[i] <- specificity(t1a)
  qda.ac$te.npv[i] <- negPredValue(t1a)
  qda.ac$te.acc[i] <- sum(diag(t1a))/sum(t1a)
  
  print(i)
}

summary(qda.ac)

mean(0.5*(qda.ac$tr.sen+qda.ac$tr.spe), na.rm=TRUE) # 0.7158751
var(0.5*(qda.ac$tr.sen+qda.ac$tr.spe), na.rm=TRUE) # 0.008163934

mean(0.5*(qda.ac$te.sen+qda.ac$te.spe), na.rm=TRUE) # 0.6868096
var(0.5*(qda.ac$te.sen+qda.ac$te.spe), na.rm=TRUE) # 0




###########################################

# Logistic regression
library(plotROC)



# new dataset "clslr" is cls transform "Group" 2 to 0
clslr <- cls
clslr$Group[clslr$Group==2] <- 0
# for clst
clstlr <- clst
clstlr$Group[clstlr$Group==2] <- 0

bthreshold <- c()
# repeat 100 times
for (k in 1:50){
  lrdf <- data.frame()
  for (i in 1:100){
    ind <- sample(2, nrow(clslr),
                  replace = TRUE,
                  prob = c(0.75, 0.25))
    training <- clslr[ind==1,]
    testing <- clslr[ind==2,]
    
    lr <- glm(Group ~ X1 + X2,data=training,family='binomial')
    
    a = predict(lr, testing[,c(1,2)], type="response")
    for (j in 1:99){
      b = ifelse(a>((j)/100),1,0)
      c <- confusionMatrix(as.factor(b),as.factor(testing$Group))
      lrdf[i,j] <- c$byClass[11]
    }
  }
  bthreshold <- c(bthreshold,which.max(parse_number(summary(lrdf)[4,])))
}

# So the result show the best threshold in general 
#     (in terms of balanced accuracy) is at 0.74
# please don't run again takes long time

# now for the average balanced accuracy
lrdf <- c()
lrdft <- c()
for (i in 1:1000){
  ind <- sample(2, nrow(clslr),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- clslr[ind==1,]
  testing <- clslr[ind==2,]
  
  lr <- glm(Group ~ X1 + X2,data=training,family='binomial')
  
  a = predict(lr, testing[,c(1,2)], type="response")
  at = predict(lr, clstlr[,c(1,2)], type="response")
  
  b = ifelse(a>0.74,1,0)
  bt = ifelse(at>0.74,1,0)
  c <- confusionMatrix(as.factor(b),as.factor(testing$Group))
  ct <- confusionMatrix(as.factor(bt),as.factor(clstlr$Group))
  lrdf[i] <- c$byClass[11]
  lrdft[i] <- ct$byClass[11]
  
  print(i)
}
mean(lrdf) # 0.7648436
var(lrdf) # 0.003498428

mean(lrdft) # 0.7167933
var(lrdft) # 0.003617448

###########################################

# Support vector machines

library(kernlab)

# linear kernal

# in here just calculate the mean and variance of balanced accuracy
# loop takes long time please not to use unless necessary
defaultW <- getOption("warn") 
options(warn = -1) 
ba.svm <- c()
ba.svmt <- c()
sd <- c(rnorm(1000,0,100000))
for (i in 1:1000){
  set.seed(sd[i])
  ind <- sample(2, nrow(clslr),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- clslr[ind==1,]
  testing <- clslr[ind==2,]
  set.seed(sd[i])
  mdl <- train(as.factor(Group) ~ X1 + X2, data = training, method = "svmLinear",
               trControl = trainControl("cv", number = 5),
               tuneGrid = expand.grid(C = seq(0, 2, length = 100)))
  
  # Test model on testing data
  yTestPred <- predict(mdl, newdata=testing[,c(1,2)])
  yTestPredt <- predict(mdl, newdata=clstlr[,c(1,2)])
  
  c <- confusionMatrix(as.factor(yTestPred), as.factor(testing[,3]))
  ct <- confusionMatrix(as.factor(yTestPredt), as.factor(clstlr[,3]))
  ba.svm[i] <- c$byClass[11]
  ba.svmt[i] <- ct$byClass[11]
  
  print(i)
}
options(warn = defaultW)

mean(ba.svm) # 0.5967141
var(ba.svm) # 0.0045459

mean(ba.svmt) # 0.6159967 
var(ba.svmt) # 0.002536254

###########################################

# Polynomial kernel

options(warn = -1) 
ba.svm <- c()
ba.svmt <- c()
sd <- c(rnorm(1000,0,100000))
for (i in 1:1000){
  set.seed(sd[i])
  ind <- sample(2, nrow(clslr),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- clslr[ind==1,]
  testing <- clslr[ind==2,]
  set.seed(sd[i])
  mdl <- train(as.factor(Group) ~ X1 + X2, data = training, method='svmPoly')

  # Test model on testing data
  yTestPred <- predict(mdl, newdata=testing[,c(1,2)])
  yTestPredt <- predict(mdl, newdata=clstlr[,c(1,2)])

  c <- confusionMatrix(as.factor(yTestPred), as.factor(testing[,3]))
  ct <- confusionMatrix(as.factor(yTestPredt), as.factor(clstlr[,3]))
  ba.svm[i] <- c$byClass[11]
  ba.svmt[i] <- ct$byClass[11]
  
  print(i)
}
options(warn = defaultW)

mean(ba.svm) # 0.6568156
var(ba.svm) # 0.004560307

mean(ba.svmt) # 0.6924267
var(ba.svmt) # 0.0008930488

###########################################

# Radial Basis kernel testing for best C

options(warn = -1) 
ba.svm <- c()
ba.svmt <- c()
sd <- c(rnorm(1000,0,100000))
for (i in 1:1000){
  set.seed(sd[i])
  ind <- sample(2, nrow(clslr),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- clslr[ind==1,]
  testing <- clslr[ind==2,]
  set.seed(sd[i])
  mdl <- train(as.factor(Group) ~ X1 + X2, data = training, method='svmRadial')
  
  # Test model on testing data
  yTestPred <- predict(mdl, newdata=testing[,c(1,2)])
  yTestPredt <- predict(mdl, newdata=clstlr[,c(1,2)])
  
  c <- confusionMatrix(as.factor(yTestPred), as.factor(testing[,3]))
  ct <- confusionMatrix(as.factor(yTestPredt), as.factor(clstlr[,3]))
  
  ba.svm[i] <- c$byClass[11]
  ba.svmt[i] <- ct$byClass[11]
  print(i)
}
options(warn = defaultW)

mean(ba.svm) # 0.7796991
var(ba.svm) # 0.004857042

mean(ba.svmt) # 0.8382833
var(ba.svmt) # 0.0004816792


###########################################

# Knn

options(warn = -1) 
ba.knn <- c()
ba.knnt <- c()
bk <- c()
sd <- c(rnorm(1000,0,100000))
for (i in 1:1000){
  set.seed(sd[i])
  ind <- sample(2, nrow(clslr),
                replace = TRUE,
                prob = c(0.75, 0.25))
  training <- clslr[ind==1,]
  testing <- clslr[ind==2,]
  set.seed(sd[i])
  opts <- trainControl(method='repeatedcv', number=10, repeats=5)
  # Find optimal k (model)
  mdl <- train(as.factor(Group) ~ X1 + X2, data = training, # training data
               method='knn', # machine learning model
               trControl=opts, # training options
               tuneGrid=data.frame(k=seq(2, 15))) # range of k's to try
  
  # Test model on testing data
  yTestPred <- predict(mdl, newdata=testing[,c(1,2)])
  yTestPredt <- predict(mdl, newdata=clstlr[,c(1,2)])
  
  c <- confusionMatrix(as.factor(yTestPred), as.factor(testing[,3]))
  ct <- confusionMatrix(as.factor(yTestPredt), as.factor(clstlr[,3]))
  
  ba.knn[i] <- c$byClass[11]
  ba.knnt[i] <- ct$byClass[11]
  
  bk[i] <- mdl$bestTune
  print(i)
}
options(warn = defaultW)

mean(ba.knn) # 0.7883835
var(ba.knn) # 0.004364325

mean(ba.knnt) # 0.8661967
var(ba.knnt) # 0.000314171

bk <- as.data.frame(unlist(bk)) # basically higher the k better the value





###########################################















