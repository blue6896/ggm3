fullfilenamesforsubnetwork= sort(list.files(pattern=".*indextruecovSIMU2c10.*\\.txt$"))
for (i in fullfilenamesforsubnetwork){
  tempfi= read.csv(i)
  print(i)
  print('Hierarchical Clustering')
  tempforhc= strsplit(as.character(tempfi[8,]),split=' ')
  tempforhc= tempforhc[[1]][-1]; 
  print(c(max(table(tempforhc[1:9])), names(which.max((table(tempforhc[1:9])))),
  max(table(tempforhc[10:18])),names(which.max((table(tempforhc[10:18])))),
  max(table(tempforhc[19:27])),names(which.max((table(tempforhc[19:27])))),
  max(table(tempforhc[28:33])),names(which.max((table(tempforhc[28:33])))),
  max(table(tempforhc[34:39])),names(which.max((table(tempforhc[34:39])))),
  max(table(tempforhc[40:45])),names(which.max((table(tempforhc[40:45])))),
  max(table(tempforhc[46:48])),names(which.max((table(tempforhc[46:48])))),
  max(table(tempforhc[49:51])),names(which.max((table(tempforhc[49:51])))),
  max(table(tempforhc[51:54])),names(which.max((table(tempforhc[52:54]))))))
  print('NET-C')
  tempfornetc= strsplit(as.character(tempfi[11,]),split=' ')
  tempfornetc= tempfornetc[[1]][-1]; 
  print(c(
  max(table(tempfornetc[1:9])), sum(as.numeric(tempfornetc[1:9]))/9,
  max(table(tempfornetc[10:18])),sum(as.numeric(tempfornetc[10:18]))/9,
  max(table(tempfornetc[19:27])),sum(as.numeric(tempfornetc[19:27]))/9,
  max(table(tempfornetc[28:33])),sum(as.numeric(tempfornetc[28:33]))/6,
  max(table(tempfornetc[34:39])),sum(as.numeric(tempfornetc[34:39]))/6,
  max(table(tempfornetc[40:45])),sum(as.numeric(tempfornetc[40:45]))/6,
  max(table(tempfornetc[46:48])),sum(as.numeric(tempfornetc[46:48]))/3,
  max(table(tempfornetc[49:51])),sum(as.numeric(tempfornetc[49:51]))/3,
  max(table(tempfornetc[51:54])),sum(as.numeric(tempfornetc[52:54]))/3))
}