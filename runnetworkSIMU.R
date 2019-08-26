
#set.seed(100)
source('metalsimunetwork2.R')

runnetworkSIMU<-function(samples){
if(samples ==150){
  #sample 150 & strategy 1 
  nodes=120#40
 # samples=150
  cliques=c(9,9,9,6,6,6,3,3,3)#c(15,9,3)
  cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
  rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
  filenamess='SA150'
  densityval=1
#  varcoefcase='case1'
return(metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval))
}
else if(samples ==180){
    #sample 180 & strategy2
  nodes=120#40
  #samples=180
  cliques=c(9,9,9,6,6,6,3,3,3)#c(15,9,3)
  cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
  rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
  filenamess='SA180'
  densityval=1
#  varcoefcase='ca'
return(metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval))
}
else if (samples ==210){
   #sample 210 & strategy 3
  nodes=120
  #samples=210
  cliques=c(9,9,9,6,6,6,3,3,3)#c(15,9,3)
  cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
  rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
  filenamess='SA210'
  densityval=1
#  varcoefcase='case1'
return(metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval))
}
else if (samples ==240){
   #sample 240 & strategy 4
  nodes=120#40
  #samples=240
  cliques=c(9,9,9,6,6,6,3,3,3)#c(15,9,3)
  cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
  rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
  filenamess='SA240'
  densityval=1
#  varcoefcase='case1'
return(metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval))
}
}
runnetworkSIMU(150)
runnetworkSIMU(180)
runnetworkSIMU(210)
runnetworkSIMU(240)


#######################################################
 #  sssss


  #SC1to4 c1
 # nodes=120#40
 # samples=120
 # cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
 # cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
 # rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
 # filenamess='SC1to4'
 # densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)




  #SC1to4 c1
 # nodes=120#40
 # samples=120
 # cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
 # cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
 # rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
 # filenamess='SC1to4'
 # densityval=1
# # varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC5  c5 
#nodes=120#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC5'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC6  c6
#nodes=120#40
#samples=120
#cliques=c(15,9,3)#c(15,9,3)
#cliquescorr=c(0.5,0.5,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.05,0.05,0.05)#c(0.1,0.05,0.1)
#filenamess='SC6'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)


#SC7c1
#nodes=120#40
#samples=160
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC7'
#densityval=1
##  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)


#SC8 c1
#nodes=120#40
#samples=200
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC8'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)


#SC9 c6 
#nodes=80#40
#samples=120
#cliques=c(15,9,3)#c(15,9,3)
#cliquescorr=c(0.5,0.5,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.05,0.05,0.05)#c(0.1,0.05,0.1)
#filenamess='SC9'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC10 c6
#nodes=40#40
#samples=120
#cliques=c(15,9,3)#c(15,9,3)
#cliquescorr=c(0.5,0.5,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.05,0.05,0.05)#c(0.1,0.05,0.1)
#filenamess='SC10'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC11 c1
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC11'
#densityval=0.67
# # varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC12 c1
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC12'
#densityval=0.33
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC13 c1 # the number of clusters may be 6, not 9
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.1,0.6,0.5,0.1,0.6,0.5,0.1)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC13'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC14 c1
#nodes=120#40 # the number of clusters may be 3, not 9 
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.1,0.1,0.6,0.1,0.1,0.6,0.1,0.1)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC14'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)


#SC15 c1 # density 0.83
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC15'
#densityval=0.83
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC16 c1 # density 0.5 
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC16'
#densityval=0.5
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC17 c1 # the number of clusters may be big 3, not 9
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.1,0.1,0.1,0.1,0.1,0.1)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC17'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC18 c1 # the number of clusters may be middle 3, not 9
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.1,0.1,0.1,0.6,0.5,0.4,0.1,0.1,0.1)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC18'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC19 c1 # the number of clusters may be small 3 , not 9
#nodes=120#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.1,0.1,0.1,0.1,0.1,0.1,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC19'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC20c1 # samples 240
#nodes=120#40
#samples=240
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC20'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)


#SC21 c1 # samples  280
#nodes=120#40
#samples=280
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC21'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC22 c6   60 fs 
#nodes=60#40
#samples=120
#cliques=c(15,9,3)#c(15,9,3)
#cliquescorr=c(0.5,0.5,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.05,0.05,0.05)#c(0.1,0.05,0.1)
#filenamess='SC22'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC23 c6    100 fs 
#nodes=100#40
#samples=120
#cliques=c(15,9,3)#c(15,9,3)
#cliquescorr=c(0.5,0.5,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.05,0.05,0.05)#c(0.1,0.05,0.1)
#filenamess='SC23'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC24  c5 
#nodes=80#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC24'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC25  c5 
#nodes=90#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC25'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC26  c5 
#nodes=100#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC26'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC27  c5 
#nodes=110#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC27'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC28  c5 
#nodes=60#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC28'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#SC29  c5 
#nodes=70#40
#samples=120
#cliques=c(15,15,9,9,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.6,0.5,0.6,0.5)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.05,0.1,0.05)#c(0.1,0.05,0.1)
#filenamess='SC29'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

######################### 120 by 120 setting (c1) to more'
#sc30  c1
#nodes=90#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC30'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#sc31  c1
#nodes=100#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC31'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

#sc32  c1
#nodes=110#40
#samples=120
#cliques=c(15,15,15,9,9,9,3,3,3)#c(15,9,3)
#cliquescorr=c(0.6,0.5,0.4,0.6,0.5,0.4,0.6,0.5,0.4)#c(0.6,0.5,0.4)
#rangecorr=c(0.1,0.05,0.1,0.1,0.05,0.1,0.1,0.05,0.1)#c(0.1,0.05,0.1)
#filenamess='SC32'
#densityval=1
#  varcoefcase='case1'
#metalsimunetwork2(nodes,samples,cliques,cliquescorr,rangecorr,filenamess,densityval)

