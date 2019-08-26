metalsimunetwork2 <- function(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval){

set.seed(100)
library(corpcor)
#source('simudatasa32.R')
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
bootnum=1
case1=TRUE
###nodes=120#40
randomindex=sample(c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8)))
###samples=120
###cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
###cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
###rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
betweencorr=0.15
betweenrangecorr=0.1
betweentrue=TRUE 

alpha=1
valmet='AIC'
# no-covariate-adjustment
#X=simudatasa32(nodes, samples, cliques, cliquescorr, rangecorr,betweencorr,betweenrangecorr,betweentrue,densityval,filenamess)
X=simudatasa3(nodes, samples, cliques, cliquescorr, rangecorr,betweencorr,betweenrangecorr,betweentrue)

#a=read.csv('metal_r.csv')


mc=X

X=as.matrix(mc)
covars=0
samples= dim(X)[1]
nodes= dim(X)[2]
alpha=1
valmet='AIC'
#print(cor(X))

for (lll in 1:bootnum){
#  print(lll)
  Y= simu100AICGGM( X, covars,nodes,samples,alpha,valmet,F)
  
  write.table(Y,paste('metal-inter2-simu',lll,'counts',filenamess,'.txt',sep='') )
}


namesMC= colnames(mc)#t(read.table('metal-inter.txt'))



write.table(X,paste('Xsimu2',filenamess,'.txt',sep=''))
return(X)
#write.table(bw,'bwsimu2.txt')

}