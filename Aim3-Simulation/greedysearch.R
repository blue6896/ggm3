
library(rje)

greedysearch<-function(matrices){
colnames(matrices)=currenttargetcandidate=c(1:dim(matrices)[1])
grouplist=list()
close( file('groupsbygreedy.txt', open="w" ) )
threshes=0.9
removed=0
greedysearch <- function(matrices, threshes){
  current= which(colSums(matrices)== max(colSums(matrices)))
  currenttarget= current[1]
  currenttargetgroup= which( matrices[currenttarget,]!=0)
    if(max(colSums(matrices))<=1){
    return(c())
  }
  else{
  currentmatrix= matrices[currenttargetgroup,currenttargetgroup]
  currentpowerset= powerSet(currenttargetgroup)
  currentpowerset= currentpowerset[lapply(currentpowerset,length)>2]
  currentpowersetscore=c(); currentpowersetscoreindex=list()
  
  for (j in 1:length(currentpowerset)){
    tempmat=matrices[currentpowerset[[j]],currentpowerset[[j]]] 
    if (sum(tempmat)> threshes*dim(tempmat)[1]*dim(tempmat)[2]){
      currentpowersetscore=c(currentpowersetscore,sum(tempmat))
      currentpowersetscoreindex=append(currentpowersetscoreindex, list(as.numeric(currentpowerset[[j]])))
    }
  }
  
    currenttargetgroup= as.numeric(unlist(currentpowersetscoreindex[which(currentpowersetscore==max(currentpowersetscore))[1]]))
    #print(currenttargetgroup)
    
towrite=colnames(matrices)[as.numeric(unlist(currentpowersetscoreindex[which(currentpowersetscore==max(currentpowersetscore))[1]]))]
    write(paste(as.numeric(towrite),collapse=','),'groupsbygreedy.txt',append = T)
    grouplist=append(grouplist,(as.numeric(names((currenttargetgroup)))))
    greedysearch(matrices[-currenttargetgroup,-currenttargetgroup],threshes)
  }
}
start_time <- Sys.time()
greedysearch(matrices,0.9)
end_time <- Sys.time()
print(end_time- start_time)

# Read in the data
x <- scan("groupsbygreedy.txt", what="", sep="\n")
appendedlist=list()
for(i in 1:length(x)){
  appendedlist=append(appendedlist,list(as.numeric(unlist(strsplit(x,split=',')[i]))))
}
return(appendedlist)
}
