metalsimuestimationholdout2WR <- function(cases,filenamess){
 #cases='c1'; filenamess='SC8'
set.seed(100)
library(corpcor)
library(randomForest)
indexset=as.numeric(read.table('lambdaset.txt'))

source('simudatasa3.R')
#source('simu100AIC.R')
source('simu100AICGGM.R')
#source('tptn.R')
library(SparseM)
#nodes, samples, covars,degrees, covhubcor, normalcor, binary
library(dendextend)
library(glmnet)
library(SparseM)
library(mice)
library(grpreg)
library(qgraph)
library(plyr)
library(igraph)


L=function(){return(runif(1,10,20))}; M=function(){return(runif(1,3,9))}; S=function(){return(runif(1,0,2))}
X=as.matrix(read.table(paste('Xsimu2',filenamess,'.txt',sep='')))
nodes=dim(X)[2]
#fixedposneg=sample(rep(c(1,1),(nodes/2)))
#fixedposneg=sample(rep(c(1,1,1,1,1,1,1,1,-1,-1),(nodes/10)))   # 8:2
#fixedposneg=sample(rep(c(1,1,1,1,1,-1,-1,-1,-1,-1),(nodes/10)))  # 5:5
#fixedposneg=sample(rep(c(1,1,1,1,1,1,-1,-1,-1,-1),(nodes/10))) #6:4 
fixedposneg=sample(rep(c(1,1,1,1,1,1,1,-1,-1,-1),(nodes/10))) #7:3


b=rnorm(nodes,0.001, 0.002)   #rep(0.2, nodes)
# case 1: each cluster has strong and weak
if (cases== 'c1'){

  b[1:54]=rep(c(runif(1,10,20),runif(1,3,9),runif(1,0,2)),18)
  b[55:80]=runif(26,0,20)  

  write.table(b,'simucoefsc1.txt')
#  # b[30]= runif(1,2,5)
}else if (cases=='c2'){

 b[1:54]=rep(c(runif(1,0,2),runif(1,0,2),runif(1,0,2)),18)
  b[55:80]=runif(26,0,20)

#  b[1:54]=runif(1,0,2)
#   b[55:70]=runif(16,1,20)
  write.table(b,'simucoefsc2.txt')
}else if (cases=='c3'){
  b[1:54]=rep(c(runif(1,3,9),runif(1,3,9),runif(1,3,9)),18)
  b[55:80]=runif(26,0,20) 

  write.table(b,'simucoefsc3.txt')
}else if (cases=='c4'){
 b[1:54]=rep(c(runif(1,10,20),runif(1,10,20),runif(1,10,20)),18)
  b[55:80]=runif(26,0,20) 

  write.table(b,'simucoefc4.txt')
}else if(cases=='c5') {

 b[1:54]=rep(c(runif(1,10,20),runif(1,3,9),runif(1,0,2)),18)*rep(c(1,1,1,1,-1,-1),9)
  b[55:80]=runif(26,0,20)  #*   c(rep(c(1,-1),8),rep(1,10))
  b=b*fixedposneg
  write.table(b,'simucoefc5.txt')
}else if(cases=='c6'){

   b[1:54]=rep(c(runif(1,0,2),runif(1,0,2),runif(1,0,2)),18)* rep(c(1,1,1,1,-1,-1),9)
  b[55:80]=runif(26,0,20)   #*   c(rep(c(1,-1),8),rep(1,10))
  b=b*fixedposneg
  write.table(b,'simucoefc6.txt')

}else if(cases=='c7'){
    b[1:54]=rep(c(runif(1,3,9),runif(1,3,9),runif(1,3,9)),18)* rep(c(1,1,1,1,-1,-1),9)
  b[55:80]=runif(26,0,20)    #*  c(rep(c(1,-1),8),rep(1,10))
  b=b*fixedposneg
  write.table(b,'simucoefc7.txt')
}else if(cases=='c8'){
   b[1:54]=rep(c(runif(1,10,20),runif(1,10,20),runif(1,10,20)),18)* rep(c(1,1,1,1,-1,-1),9)
  b[55:80]=runif(26,0,20)   #*   c(rep(c(1,-1),8),rep(1,10))
  b=b*fixedposneg
  write.table(b,'simucoefc8.txt')
}else if(cases == 'c9'){ # large clusters with large coeffs 
 b[1:27]=rep(c(runif(1,10,20),runif(1,10,20),runif(1,10,20)),9)
 b[28:54]=rep(c(runif(1,10,20),runif(1,3,9),runif(1,0,2)),9)
  b[1:54]= b[1:54]#* rep(c(1,1,1,1,-1,-1),9)
  b[55:80]=runif(26,0,20)#*   c(rep(c(1,-1),8),rep(1,10))
  b=b*fixedposneg

 write.table(b,'simucoefc9.txt')
}else if(cases == 'c10'){ # large and middle clusters with large coeffs
 b[1:45]=rep(c(runif(1,10,20),runif(1,10,20),runif(1,10,20)),15)
 b[46:54]=rep(c(runif(1,10,20),runif(1,3,9),runif(1,0,2)),3)
  b[1:54]= b[1:54]   #* rep(c(1,1,1,1,-1,-1),9)
  b[55:80]=runif(26,0,20) ###33*   c(rep(c(1,-1),8),rep(1,10))
  b=b*fixedposneg

 write.table(b,'simucoefc10.txt')
}

#...45#
bw= 20 + X%*%b + runif(dim(X)[1], -20,20)# rnorm (dim(X)[1], 0, 0.9) #a[sample(c(1:760),26),11]
# ...46 bw= runif(, -10.10)
# ... 200 bw= 20 + X%*%b + runif(dim(X)[1], -35,35)
#bw= 20 + X%*%b + rnorm(dim(X)[1], -15,15)
#bw= 200 + X%*%b + runif(dim(X)[1], -200,200)

  mat=matrix(0,dim(X)[2],dim(X)[2])
  for (haha in 1:1){
    mat= mat+  read.table(paste('metal-inter2-simu',haha,'counts',filenamess,'.txt',sep='') )
    #metal-inter2-simu1countsSC12
  }
  mat=mat/1
  
colnames(mat)=c(1:dim(X)[2])

tempmat=mat
### grouping 
nodes= dim(mat)[2]
onegroup= rep(1,nodes)
allgroups= c(1:nodes)

# kmean
km <- kmeans(t(X), 5)#length(unique(fgcmembership)))
kmmembership5=(km$cluster)   
km <- kmeans(t(X), 4)#length(unique(fgcmembership)))
kmmembership4=(km$cluster)   
km <- kmeans(t(X), 3)#length(unique(fgcmembership)))
kmmembership3=(km$cluster)   
#hclust
distmat= dist(t(X))
hclumat= hclust(distmat)
plot(hclumat)
colnames(mat)
dend1 <- as.dendrogram(hclumat)

#png(height=1200, width=1200, file=paste("dendo",cases,filenamess,".png",sep=''))
#plot(dend1)
#dev.off()
hclustmembership5= cutree(dend1,k=5)#length(unique(fgcmembership))) # it now works on a dendrogra  
hclustmembership4= cutree(dend1,k=4)
hclustmembership3= cutree(dend1,k=3)
#proposed grouping 
mat=tempmat; mat[abs(mat)<0.01]=0
testingg=graph_from_adjacency_matrix(as.matrix(abs(mat)),weighted=T)
optgrouping001= cluster_fast_greedy(as.undirected(testingg))$membership

mat=tempmat;mat[abs(mat)<0.1]=0
testingg=graph_from_adjacency_matrix(as.matrix(abs(mat)),weighted=T)
optgrouping010= cluster_fast_greedy(as.undirected(testingg))$membership

mat=tempmat;mat[abs(mat)<0.01]=0
testingg=graph_from_adjacency_matrix(as.matrix(abs(mat)),weighted=T)
lougrouping001= cluster_louvain(as.undirected(testingg))$membership

mat=tempmat;mat[abs(mat)<0.1]=0
testingg=graph_from_adjacency_matrix(as.matrix(abs(mat)),weighted=T)
lougrouping010= cluster_louvain(as.undirected(testingg))$membership

print(unique(optgrouping001))
print(unique(lougrouping001))

km <- kmeans(t(X), length(unique(optgrouping001)))#length(unique(fgcmembership)))
kmmembership5=(km$cluster)
hclustmembership5= cutree(dend1,k=length(unique(optgrouping001)))

mat=tempmat

write.table(rbind(onegroup,allgroups,kmmembership3,kmmembership4,kmmembership5,hclustmembership3,hclustmembership4,hclustmembership5
                  ,optgrouping001,optgrouping010,
                  #optgrouping015,optgrouping020,optgrouping025,optgrouping030,optgrouping035,optgrouping040,
                  lougrouping001,lougrouping010
                  #,lougrouping015,lougrouping020,lougrouping025,lougrouping030,lougrouping035,lougrouping040
                  ),paste('groupindextruecovSIMU2',cases,filenamess,'.txt',sep=''))
############################################################################
##########
#############################################################################################
X2=X;bw2=bw

orderingset=c()
for(k in 1:100){
orderingset=cbind(orderingset, sample(dim(X2)[1], replace=T))
}

group=allgroups
errorsummatrices=c()
errorsumvectors=c()
###############

bspoint= round(dim(X)[1]*0.7)
sumsq=0
coefvals=0 
errorsum=c()
errorsumvectors=rep(0,100)
predictedsum=0
predictedpcc=0 

for(i in 1:100){
#print(i)
for (j in 1:100){ # lambda set 
  ordering=orderingset[,i]
     X=X2[ordering,]; bw=bw2[ordering]
  ###  
  fit <- grpreg(lambda= indexset[j], X[1:bspoint,], bw[1:bspoint], group, penalty="grLasso")
# print(length(fit$beta))
#  print(dim(X))
#  print(length(fit$beta[2:dim(X)[2]]))
  errorsum=c(sum((fit$beta[1]+X[(bspoint+1):dim(X)[1],] %*% (fit$beta[2:(1+dim(X)[2])])- bw[(bspoint+1):dim(X)[1]])^2) ,errorsum)

}
 errorsumvectors=(errorsumvectors+errorsum)
errorsummatrices= rbind(errorsummatrices, errorsum)
errorsum=c()

}
print('2)simu lasso all in different groups')
print(min(errorsumvectors/100))
write.csv(errorsummatrices, paste(cases, filenamess, 'agerror.csv', sep=''))
print(coefvals/100)
print(sumsq/100)
#errorsumvectorstotal=rbind(errorsumvectorstotal,errorsumvectors)
#grLassoresults= c(grLassoresults, errorsum/100)
#grLassocoefresults=rbind(grLassocoefresults, coefvals)
#grLassocoeferrorresults=c(grLassocoeferrorresults, sumsq/100)
#print(cor(predictedsum, bw))
#print(predictedpcc/100)


errorsumvectors=c()
errorsummatrices=c() # from here remove #s
bspoint= round(dim(X)[1]*0.7)
sumsq=0;coefvals=0;errorsum=c()
errorsumvectors=rep(0,100);predictedsum=0;predictedpcc=0
data=cbind(bw2,X2)
names=colnames(data)=c('outcome',paste('a',1:dim(X2)[2],sep=''))

for(i in 1:100){
print(i)
for (j in 1:100){ # lambda set
  ordering=orderingset[,i]
  train=data[1:bspoint,];test=data[(bspoint+1):dim(X2)[1],]
  ###

val=randomForest(outcome ~ .,data=train,ntree=round(dim(X)[2]^0.5), importance =TRUE, proximity=TRUE)
val=randomForest(outcome ~ .,data=train,ntree=500, importance =TRUE, proximity=TRUE)
test.pred.forest <- predict(val,test)
MSE.forest <- (sum((test.pred.forest-test[,1])^2))
  errorsum=c(MSE.forest  ,errorsum)

}
 errorsumvectors=(errorsumvectors+errorsum)
errorsummatrices= rbind(errorsummatrices, errorsum)
}
print('2.5)simu randomforest')
print(min(errorsumvectors/100))
write.csv(errorsummatrices, paste(cases, filenamess, 'rferror.csv', sep=''))

#############################RFRFRFRFRF


print('5)simu lasso all km 5 centers')
print(min(errorsumvectors/100))
#print(coefvals/100)
#print(sumsq/100)
#errorsumvectorstotal=rbind(errorsumvectorstotal,errorsumvectors)
#grLassoresults= c(grLassoresults, errorsum/100)
#grLassocoefresults=rbind(grLassocoefresults, coefvals)
#grLassocoeferrorresults=c(grLassocoeferrorresults, sumsq/100)
#print(cor(predictedsum, bw))
#print(predictedpcc/100)


group=hclustmembership5
errorsumvectors=c()
errorsummatrices=c()
###############
bspoint= round(dim(X)[1]*0.7)
sumsq=0
coefvals=0
errorsum=c()
errorsumvectors=rep(0,100)
predictedsum=0
predictedpcc=0

for(i in 1:100){
print(i)
for (j in 1:100){ # lambda set
ordering=orderingset[,i]
     X=X2[ordering,]; bw=bw2[ordering]
  ###
  fit <- grpreg(lambda= indexset[j], X[1:bspoint,], bw[1:bspoint], group, penalty="grLasso")
# print(length(fit$beta))
  errorsum=c(sum((fit$beta[1]+X[(bspoint+1):dim(X)[1],] %*% (fit$beta[2:(1+dim(X)[2])])- bw[(bspoint+1):dim(X)[1]])^2)   ,errorsum)



}
 errorsumvectors=(errorsumvectors+errorsum)
errorsummatrices= rbind(errorsummatrices, errorsum)
errorsum=c()

}


print('8)lasso all hc 5 centers')
print(min(errorsumvectors/100))
write.csv(errorsummatrices, paste(cases, filenamess, 'hcerror.csv', sep=''))
#errorsumvectorstotal=rbind(errorsumvectorstotal,errorsumvectors)
#grLassoresults= c(grLassoresults, errorsum/100)
#grLassocoefresults=rbind(grLassocoefresults, coefvals)
#grLassocoeferrorresults=c(grLassocoeferrorresults, sumsq/100)
#print(coefvals/100)
#print(sumsq/100)

#print(cor(predictedsum, bw))
#print(predictedpcc/100)


group=lougrouping001
errorsumvectors=c()
errorsummatrices=c()
###############
bspoint= round(dim(X)[1]*0.7)
sumsq=0
coefvals=0
errorsum=c()
errorsumvectors=rep(0,100)
predictedsum=0
predictedpcc=0

for(i in 1:100){
print(i)
for (j in 1:100){ # lambda set
ordering=orderingset[,i]
     X=X2[ordering,]; bw=bw2[ordering]
  ###
  fit <- grpreg(lambda= indexset[j], X[1:bspoint,], bw[1:bspoint], group, penalty="grLasso")
# print(length(fit$beta))
  errorsum=c(sum((fit$beta[1]+X[(bspoint+1):dim(X)[1],] %*% (fit$beta[2:(1+dim(X)[2])])- bw[(bspoint+1):dim(X)[1]])^2)   ,errorsum)


}
 errorsumvectors=(errorsumvectors+errorsum)
errorsummatrices= rbind(errorsummatrices, errorsum)
errorsum=c()
}

print('17)lasso all lou001')
print(min(errorsumvectors/100))
write.csv(errorsummatrices, paste(cases, filenamess, 'louerror.csv', sep=''))
#####################
library(neuralnet)
errorsumvectors=c()
errorsummatrices=c()
bspoint= round(dim(X)[1]*0.7)
sumsq=0;coefvals=0;errorsum=c()
errorsumvectors=rep(0,100);predictedsum=0;predictedpcc=0
data=cbind(bw2,X2)
names=colnames(data)=c('outcome',paste('a',1:dim(X2)[2],sep=''))

for(i in 1:100){
print(i)
for (j in 1:100){ # lambda set
  ordering=orderingset[,i]
  train=data[1:bspoint,];test=data[(bspoint+1):dim(X2)[1],]
  ###

val=neuralnet(outcome ~ .,data=train,hidden=120, stepmax=1e+07,linear.output=T)
test.pred.nn <- predict(val,test)
MSE.nn <- (sum((test.pred.nn-test[,1])^2))
  errorsum=c(MSE.nn  ,errorsum)

}
 errorsumvectors=(errorsumvectors+errorsum)
errorsummatrices= rbind(errorsummatrices, errorsum)
errorsum=c()

}
print('10.5)simu neural network')
print(min(errorsumvectors/100))
write.csv(errorsummatrices, paste(cases, filenamess, 'nnerror.csv', sep=''))

}