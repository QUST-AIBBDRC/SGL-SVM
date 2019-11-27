#SVM
n <- 10;T <- as.data.frame(y);

library(lattice) 
library(ggplot2)
library(caret)
library(kernlab)
library(lpSolve)
library(irr)

class <- c(rep("a",18),rep("b",51),rep("c",12),rep("d",23))
sglG <- colnames(y[,Pos[which(sgl$fit$beta[,which(c==15)]!=0)]])
sglG<-na.omit(sglG)
q <- T[,sglG];q <- as.data.frame(cbind(class,q));ave <- c();ka <- c()
folda <- createFolds(q[,1],k=n)
lop <- str(folda);i=1;
repeat
{if(i>n) break
  else 
  {
    test <- q[folda[[i]],];
    train <- q[-folda[[i]],];
    A1 <- ksvm(class~.,data=train,kernel="rbfdot")
    A2 <- predict(A1,test ,type="response")
    li<-cbind(A2,test$class)
    kappa<-kappa2(data.frame(test$class,A2))$value
    ka[i]<-unlist(kappa)
    table(A2,test$class)
    agreement <- A2==test$class
    f1 <- table(agreement)
    ave[i] <- length(which(agreement==TRUE))/length(agreement)
    i <- i+1;}};
mean(ave);mean(ka)