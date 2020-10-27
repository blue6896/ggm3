fullfilenames= sort(list.files(pattern="*error*"))
meanbins=c()
for (i in fullfilenames){
  aa1=read.csv(i)
  sortedaa1=sort(aa1[,1+which.min(colSums(aa1[,2:101]))])
  print(i)
  print(paste('[',paste(round(median(sortedaa1[2],sortedaa1[3]),2),
              round(median(sortedaa1[98],sortedaa1[99]),2), sep = ',' ),']',sep='') )
  print(mean(sortedaa1))
  meanbins=c(meanbins, mean(sortedaa1))
}

fullfilenamesnew= fullfilenames
meanbinsnext= meanbins
for(j in 1:length(meanbinsnext) ){
  if(j%%5==0){
print( round( (meanbinsnext[c(j-4,j, j-3,j-1  ,j-2)] - meanbinsnext[j-2])/meanbinsnext[j-2]*100,2) )
    print(fullfilenamesnew[j])
  }

}