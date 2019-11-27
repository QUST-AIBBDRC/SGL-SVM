library(SGL)
y <- read.csv("D:/生物信息学/Two-class/D.csv",header=FALSE,sep=",")#导入数据集
normalize <- function(x){return ((x-min(x))/(max(x)-min(x)))}
y <- as.data.frame(lapply(y,normalize))
y <- as.matrix(y)
y0 <- c(rep(0,19),rep(1,58))
x_1 <- y[1:19,];x_2 <- y[20:77,];
#KW
i<-1;n2<-as.numeric(dim(y)[2])
h_3<-c(1:n2)
repeat
{	
  if(i>n2) break
else 
   {   
    h_3[i]<-as.numeric(kruskal.test(list(x_1[,i],x_2[,i]))[3]);
    i=i+1;
    }}
Pos <- (order(h_3,decreasing=F)[1:100]);y1 <- y[,Pos]
#SGL
data <- list(x=y1,y=y0)
sgl <- cvSGL(data ,index <- c(rep(1:100,each=1)),min.frac=0.4)
c2 <- c(1:20);i <- 1
for(i in 1:20)
{c2[i] <- length(Pos[which(sgl$fit$beta[,i]!=0)])}
c2
colnames(y[,Pos[which(sgl$fit$beta[,which(c2==8)]!=0)]])