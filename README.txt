Aim 3 simulation 


simu100AICGGM.R finds optimal tuning paramter for network estimation 
simudatasa3.R produces simulated data using size of clusters, samples size
the number of features, and so on.
lambdaset.txt includes 100 different lambda values for finding the optimal
tuning paramters 

metalsimunetwork2.R generates networks using simu100AICGGM.R 
runnetworkSIMU.R implements metalsimunetwork2.R to simulate and solve
each simulated case 

metalsimuestimationholdout2WR.R estimates prediction error after 
predicting an outcome. 
You can run this R script using metalsimuestimationholdout2WR.sh or run.sh 
runestimationSIMUholdoutWRcase#.R
(#= 1-5, each case with different coefficients between features 
in the subnetwork and an outcome is implemented)

  

