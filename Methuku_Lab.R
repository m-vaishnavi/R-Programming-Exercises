#################################################################################################
#Problem-1
library(MASS)
library(klaR)
set.seed(0)
dat1 <- mvrnorm(n=100,mu=c(-3,-3),Sigma = matrix(c(1,.8,.8,1),ncol=2,byrow=TRUE))
dat2 <- mvrnorm(n=100,mu=c(3,3),Sigma = matrix(c(1,0,0,1),ncol=2,byrow=TRUE))
dat <- rbind(dat1,dat2)
y <- gl(2,100,labels=c('down','up'))
data <- data.frame(dat, y)
qda_data <- qda(y ~ X1 + X2, data = data)
contour_y <- gl(2,20000,labels=c('down','up'))
contour_dat <- expand.grid(cx1=dat1,cx2=dat2,KEEP.OUT.ATTRS=TRUE,stringsAsFactors=TRUE)
contour_data <- data.frame(contour_dat, contour_y)
qda_contour_data <- qda(contour_y ~ cx1 + cx2, data = contour_data)
partimat(contour_y ~ cx1 + cx2, data=contour_data, method="qda", image.colors = c("red","green"))

#################################################################################################
#Problem-2
library(class)
library(ISLR)
data(Khan)
names(Khan) 
dim(Khan$xtrain ) 
dim(Khan$xtest) 
length(Khan$ytrain) 
length(Khan$ytest ) 
#K=1
set.seed(0)
knn.pred=knn(Khan$xtrain, Khan$xtest, Khan$ytrain, k=1)  
K1=knn.pred=knn(Khan$xtrain, Khan$xtest, Khan$ytrain, k=1)
table(K1,Khan$ytest) # prints confusion matrix 
#K=3
knn.pred=knn(Khan$xtrain, Khan$xtest, Khan$ytrain, k=3) 
K3=knn.pred=knn(Khan$xtrain, Khan$xtest, Khan$ytrain, k=3)
table(K3,Khan$ytest) # prints confusion matrix 
#################################################################################################
#Problem-3
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data
dim(nci.data)
length(nci.labs)
nci.labs[1:4]
table(nci.labs)
pr.out = prcomp(nci.data, scale = T)
aColors = function(vec) {
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1, 1), pin=c(3,3), xpd=TRUE)
plot(pr.out$x[, 1:2], col = aColors(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
legend(x=60,y=-20, legend=unique(nci.labs),pch=19, text.width=27, col=aColors(unique(nci.labs)), cex=0.58, bg="white")
#################################################################################################
#Problem-4
library(ISLR)
set.seed(0)
nci.labs = NCI60$labs
nci.data = NCI60$data
dim(nci.data)
length(nci.labs)
nci.labs[1:4]
table(nci.labs)
pr.out = prcomp(nci.data, scale = T)
aColors = function(vec) {
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1, 1), pin=c(3,3), xpd=TRUE)
km.out=kmeans(pr.out$x[, 1:2],centers=14,nstart=1)
plot(pr.out$x[, 1:2], col=aColors(nci.labs), main="K-Means Clustering Results with K=14", xlab="Z1", ylab="Z2", pch=19)
text(km.out$centers, labels=rownames(km.out$centers))
legend(x=60,y=-20, legend=unique(nci.labs),pch=19, text.width=27, col=aColors(unique(nci.labs)), cex=0.58, bg="white")
#################################################################################################
#Problem-5
library(rpart)
library(rpart.plot)
data(golub) 
library(multtest)
row.names(golub)<- paste("gene", 1:3051, sep = "")
golubData <- data.frame(t(golub[1:3051,]))
golubFactor <- factor(golub.cl,levels=0:1, labels= c("ALL","AML")) 
golubRpart <- rpart(golubFactor~., data=golubData, method="class", cp=0.001) 
prp(golubRpart, branch.lwd=4, branch.col="blue", extra=101)
golub.gnames[896,]  
#################################################################################################
#Problem-6
library(class)
library(ISLR)
data(Khan)
names(Khan) 
dim(Khan$xtrain ) 
dim(Khan$xtest) 
length(Khan$ytrain) 
length(Khan$ytest ) 
table(Khan$ytrain ) 
table(Khan$ytest ) 

#Training on Train Data
data=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain )) 
out=svm(y~., data=data , kernel ="linear",cost=10) 
summary (out) 
table(out$fitted , data$y)

#Testing on Train Data
dat1.test=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain )) 
pred1.test=predict (out , newdata =dat1.test)
table(pred1.test, dat1.test$y) 

#Training on Train Data
data=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain )) 
out=svm(y~., data=data , kernel ="linear",cost=10) 
summary (out) 
table(out$fitted , data$y)

#Testing on Test Data
dat2.test=data.frame(x=Khan$xtest , y=as.factor(Khan$ytest )) 
pred2.test=predict (out , newdata =dat2.test)
table(pred2.test, dat2.test$y) 

#################################################################################################
#Problem-7
library(randomForest)
set.seed(0)
dat <- matrix(rnorm(400*10),ncol=10,byrow=TRUE)
dat[1:100,1] <- dat[1:100,1] - 4
dat[1:100,2] <- dat[1:100,2] - 4
dat[101:200,3] <- dat[101:200,3] + 4
dat[101:200,4] <- dat[101:200,4] + 4
dat[201:300,6] <- dat[201:300,6] - 4
dat[201:300,7] <- dat[201:300,7] - 4
dat[301:400,10] <- dat[301:400,10] + 6
fData <- gl(4,100,labels=c('red','green','blue','black'))
rfResult <- randomForest(dat, fData, nTree=1000, importance=TRUE, proximity=TRUE)
rfResult
plot(rfResult)
dFrame <- data.frame(dat, fData)
pairs(dFrame, col=c(rep('red',100),rep('green',100),rep('blue',100),rep('black',100)))
varImpPlot(rfResult, n.var = 10, pch=19, main="Variable Importance", col="red", gcolor="blue", lcolor="darkgreen")

#################################################################################################
#Problem-8
rm(list=ls())
set.seed(0)
dat <- matrix(rnorm(400*10),ncol=10,byrow=TRUE)
dat[1:100,1] <- dat[1:100,1] - 4
dat[1:100,2] <- dat[1:100,2] - 4
dat[101:200,3] <- dat[101:200,3] + 4
dat[101:200,4] <- dat[101:200,4] + 4
dat[201:300,6] <- dat[201:300,6] - 4
dat[201:300,7] <- dat[201:300,7] - 4
dat[301:400,10] <- dat[301:400,10] + 6

#Average Linkage Clustering
hc.average=hclust(dist(dat,method = "euclidean"), method ="average") 
par(mfrow=c(1,1))
plot(hc.average , main="Average Linkage", xlab="", sub="", cex=0.9)  
rCutree=cutree(hc.average , 4)
ftable(rCutree) 
cLabel=gl(4,100) #cLabel is a class label vector consisting of 100 1’s, followed by 100 2’s, then 100 3’s and finally 100 4’s.  
ftable(cLabel)
table(rCutree,cLabel) # prints confusion matrix 

#Complete Linkage Clustering
hc.complete=hclust(dist(dat,method = "euclidean"), method ="complete")
par(mfrow=c(1,1))
plot(hc.complete , main="Complete Linkage", xlab="", sub="", cex=0.9) 
rCutree1=cutree(hc.complete , 4)
ftable(rCutree1)
cLabel=gl(4,100)
ftable(cLabel)
table(rCutree1,cLabel) # prints confusion matrix 
#################################################################################################
#Problem-9
rm(list=ls())
library(nnet)
set.seed(0)
dat <- matrix(rnorm(400*10),ncol=10,byrow=TRUE)
dat[1:100,1] <- dat[1:100,1] - 4
dat[1:100,2] <- dat[1:100,2] - 4
dat[101:200,3] <- dat[101:200,3] + 4
dat[101:200,4] <- dat[101:200,4] + 4
dat[201:300,6] <- dat[201:300,6] - 4
dat[201:300,7] <- dat[201:300,7] - 4
dat[301:400,10] <- dat[301:400,10] + 6
cLabels <- gl(4,100,labels=c('red','green','blue','black')) # Column having class labels - 100 red, 100 green, 100 blue, and 100 black values.
dFrame <- data.frame(dat, cLabels) # Data frame with first 4 columns from given data and last column with labels.
nnest <- nnet(cLabels ~ .,data = dFrame, size = 5, maxit = 500, decay = 0.01, MaxNWts = 5000)
pred <- predict(nnest, type = "class")
table(pred, cLabels) # prints confusion matrix  
#################################################################################################
#Problem-10
rm(list=ls())
library(MASS)
set.seed(0)
dat1<-mvrnorm(n=100,mu=c(-3,-3),Sigma=matrix(c(1,.8,.8,1),ncol=2,byrow=TRUE))
dat2<-mvrnorm(n=100,mu=c(3,3),Sigma=matrix(c(1,0,0,1),ncol=2,byrow=TRUE))
dat3<-mvrnorm(n=100,mu=c(-3,3),Sigma=matrix(c(1,-.8,-.8,1),ncol=2,byrow=TRUE))
dat<-rbind(dat1,dat2,dat3)
par(pty='s')
plot(dat)
summary(Mclust(dat))
plot(mclustBIC(dat))
#################################################################################################