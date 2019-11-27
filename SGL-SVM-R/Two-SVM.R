library(ROCR)
library(caret)
library(kernlab)
#SVM(AUC)
n <- 10;T <- as.data.frame(y);
#DLBCL数据集
class <- c(rep("a",19),rep("b",58));
sglG <- colnames(y[,Pos[which(sgl$fit$beta[,which(c2==8)]!=0)]])#8为SGL选择的基因个数
q <- T[,sglG];q <- as.data.frame(cbind(class,q));ave <- c();auc <- c();
folda <- createFolds(q[,1],k=n)
lop <- str(folda);i <- 1;
repeat
{if(i>n) break
  else 
  {test <- q[folda[[i]],];train <- q[-folda[[i]],];
  A1 <- ksvm(class~.,data=train,kernel="rbfdot",prob.model=TRUE)
  A2 <- predict(A1,test,type="response")
  result<-predict(A1,test,type="probabilities")
  #list<-cbind(A2,test$class)
  pred<-prediction(predictions=result[,2],labels=test$class)
  perd<-performance(pred,measure="tpr",x.measure="fpr")
  plot(perd,main="ROC curve for SMS spam filter",col="blue",lwd=2)
  abline(a=0,b=1,lwd=2,lty=2)
  perf.auc<-performance(pred,measure="auc")
  #str(perf.auc)
  #unlist(perf.auc@y.values)
  auc[i] <- unlist(perf.auc@y.values)
  table(A2,test$class)
  agreement <- A2==test$class
  f1 <- table(agreement)
  #print(prop.table(table(agreement)))
  ave[i] <- length(which(agreement==TRUE))/length(agreement)
  i <- i+1;}};mean(ave);mean(auc)