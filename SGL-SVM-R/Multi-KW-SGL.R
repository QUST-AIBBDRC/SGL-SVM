library(SGL)
y <- read.csv("D:/生物信息学/Multi-class/m_ly.csv",header=FALSE,sep=",")#导入数据集
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
y <- as.data.frame(lapply(y,normalize))
y <- as.matrix(y)
yLy <- c(rep(1,42),rep(2,9),rep(3,11))
x_751 <- y[1:42,];x_752 <- y[43:51,];x_753 <- y[52:62,]
#KW
i<-1;n2<-as.numeric(dim(y)[2])
h_3<-c(1:n2)
repeat
{	
  if(i>n2) break
  else 
  {   
    h_3[i]<-as.numeric(kruskal.test(list(x_751[,i],x_752[,i],x_753[,i]))[3]);
    i=i+1;
  }
}
Pos <- (order(h_3,decreasing=F)[1:150]);y1 <- y[,Pos]
#SGL
data1 <- list(x=y1,y=yLy)
sgl <- cvSGL(data1,index = c(rep(1:150,each=1)),min.frac=0.3)
c <- c(1:20);i <- 1
for(i in 1:20)
{c[i] <- length(Pos[which(sgl$fit$beta[,i]!=0)])};
c
colnames(y[,Pos[which(sgl$fit$beta[,which(c==8)]!=0)]])

