set.seed(100)
library(corpcor)
library(MASS)
simudatasa3 <- function(nodes, samples, cliques, cliquescorr, rangecorr, betweencorr, betweenrangecorr,betweentrue){
#nodes indicate the number of features
# samples are number of samples
#covars number of covariates
# covhubcor is the correlation strength between covariates and some nodes
# normalcor is the correaltion among all other nodes
# degrees is the number of neighbors for covariates 



#sigma= matrix(0.01,nodes,nodes)
sigma=matrix(runif(nodes*nodes,0,0), nodes,nodes)
posnegsign=c()
  
cliquesval=0
for (i in 1:length(cliques)){
   
#sigma[i,(covars+1+(i-1)*degrees):((covars+1+(i-1)*degrees)+degrees-1)]=covhubcor
#sigma[(covars+1+(i-1)*degrees):((covars+1+(i-1)*degrees)+degrees-1),i]=covhubcor
for (kk in 1:(cliques[i]^2)){
 posnegsign=c(posnegsign, (-1)^kk)
}
  sigma[(1+cliquesval):(cliquesval+cliques[i]),(1+cliquesval):(cliquesval+cliques[i])]= posnegsign * runif(cliques[i]^2,cliquescorr[i]-rangecorr[i], cliquescorr[i]+rangecorr[i])
posnegsign=c()
#sample(c(-1,1),cliques[i]^2,replace=T)* runif(cliques[i]^2,cliquescorr[i]-rangecorr[i], cliquescorr[i]+rangecorr[i])
  
cliquesval= cliquesval+cliques[i]
}

if(betweentrue){
for (j in 1:sum(cliques)){
  for (k in 1:sum(cliques)){
    if (sigma[j,k]==0){
      sigma[j,k]=runif(1,betweencorr-betweenrangecorr, betweencorr+betweenrangecorr)
    }
  }
}
}


sigmaoutside=matrix(runif(nodes*nodes,0.01,0.012), nodes,nodes)
sigmaoutside[1:sum(cliques),1:sum(cliques)]=sigma[1:sum(cliques),1:sum(cliques)]
sigma=sigmaoutside 


sigma[lower.tri(sigma)] = t(sigma)[lower.tri(sigma)]


diag(sigma)=1
sigma=make.positive.definite(sigma)

sigma=sigma/max(sigma)

library(corrplot)
corrplot(sigma)
#sigma=cov(sigma)
out <- mvrnorm(samples, mu = rep(0,(nodes)), Sigma = (sigma),
               empirical = TRUE)


return(out)
}