# DDG-classifier in network clustering
# R package fda.usc version higher than 1.2.0

# inputs:
# eigenvector_1.txt: the text file which store first K eigenvectors of unregularized Laplacian matrix, where K is number of communities
# eigenvector_2.txt: the text file which store first K eigenvectors of regularized Laplacian matrix, where K is number of communities
# group: true membership
# p: percentage of training set

# outputs:
# mis_TD: misclassification rate of DDG-classifier with random half-space depth
# mis_MhD: misclassification rate of DDG-classifier with Mahalanobis depth
# mis_RP: misclassification rate of DDG-classifier with random projection depth
# nmi_TD: NMI of DDG-classifier with random half-space depth
# nmi_MhD: NMI of DDG-classifier with Mahalanobis depth
# nmi_RP: NMI of DDG-classifier with random projection depth

# note
# please ignore warnings, it won't effect the outputs.
# please comment out 'np' method when the input is unregularized political blog dataset, since 
# 'np' does not work under this specific data.

library(fda.usc)
library(igraph)
#set the path, replace the path with the location where the data is stored
#setwd("/Users/yahuitian/Documents/Network_DDpaper/DDexample_synthetic_real/RealData_PoliticalBlog")
#setwd("/Users/yahuitian/Documents/Network_DDpaper/DDexample_synthetic_real/SyntheticData")
group=read.table("group.txt",header=FALSE)#read the true membership
group=as.matrix(group)
p=0.2#for synthetic data
#p=0.4#for blog data
#matrix to store misclassification rate, rows are different methods,
#column 1 for unregularized case, column 2 for regularized case.
mis_TD=matrix(0,2,6)
mis_MhD=matrix(0,2,6)
mis_RP=matrix(0,2,6)

#matrix to store NMI (normalized mutual information), rows are different methods,
#column 1 for unregularized case, column 2 for regularized case.
nmi_TD=matrix(0,2,6)
nmi_MhD=matrix(0,2,6)
nmi_RP=matrix(0,2,6)

for (j in 1:2){
  data=read.table(paste("eigenvectors_",j,".txt",sep = "",collapse = NULL),header=FALSE)#read eigenvectors
  data=as.matrix(data)
  N=dim(data)[1]#number of nodes
  q=dim(data)[2]#number of communities
  data=cbind(data,group)
  e=sample(nrow(data),nrow(data)*p,replace=FALSE)#select index of training set
  train=data[e,]#select training set
  group.train=train[,q+1]#true membership of training set
  x.train=train[,1:q]#multivariate points in the training set
  test=data[-e,]#select testing set
  group.test=test[,3]
  x.test=test[,1:2]
  #use DDG-classifier to do classification
  
  #random half-space depth
  out1=classif.DD(group.train,x.train,depth="TD",classif="glm")
  out2=classif.DD(group.train,x.train,depth="TD",classif="gam")
  out3=classif.DD(group.train,x.train,depth="TD",classif="lda")
  out4=classif.DD(group.train,x.train,depth="TD",classif="qda")
  out5=classif.DD(group.train,x.train,depth="TD",classif="knn")
  out6=classif.DD(group.train,x.train,depth="TD",classif="np")
  
  pred1=predict.classif.DD(out1,x.test)
  pred2=predict.classif.DD(out2,x.test)
  pred3=predict.classif.DD(out3,x.test)
  pred4=predict.classif.DD(out4,x.test)
  pred5=predict.classif.DD(out5,x.test)
  pred6=predict.classif.DD(out6,x.test)
  
  incorrect1<-pred1!=group.test
  mis_TD[j,1]=mean(incorrect1)
  nmi_TD[j,1]=compare(pred1,group.test,method='nmi')
  
  incorrect2<-pred2!=group.test
  mis_TD[j,2]=mean(incorrect2)
  nmi_TD[j,2]=compare(pred2,group.test,method='nmi')
  
  incorrect3<-pred3!=group.test
  mis_TD[j,3]=mean(incorrect3)
  nmi_TD[j,3]=compare(pred3,group.test,method='nmi')
  
  incorrect4<-pred4!=group.test
  mis_TD[j,4]=mean(incorrect4)
  nmi_TD[j,4]=compare(pred4,group.test,method='nmi')
  
  incorrect5<-pred5!=group.test
  mis_TD[j,5]=mean(incorrect5)
  nmi_TD[j,5]=compare(pred5,group.test,method='nmi')
  
  incorrect6<-pred6!=group.test
  mis_TD[j,6]=mean(incorrect6)
  nmi_TD[j,6]=compare(pred6,group.test,method='nmi')
  
  #Mahalanobis depth
  out1=classif.DD(group.train,x.train,depth="MhD",classif="glm")
  out2=classif.DD(group.train,x.train,depth="MhD",classif="gam")
  out3=classif.DD(group.train,x.train,depth="MhD",classif="lda")
  out4=classif.DD(group.train,x.train,depth="MhD",classif="qda")
  out5=classif.DD(group.train,x.train,depth="MhD",classif="knn")
  out6=classif.DD(group.train,x.train,depth="MhD",classif="np")
  
  pred1=predict.classif.DD(out1,x.test)
  pred2=predict.classif.DD(out2,x.test)
  pred3=predict.classif.DD(out3,x.test)
  pred4=predict.classif.DD(out4,x.test)
  pred5=predict.classif.DD(out5,x.test)
  pred6=predict.classif.DD(out6,x.test)
  
  incorrect1<-pred1!=group.test
  mis_MhD[j,1]=mean(incorrect1)
  nmi_MhD[j,1]=compare(pred1,group.test,method='nmi')
  
  incorrect2<-pred2!=group.test
  mis_MhD[j,2]=mean(incorrect2)
  nmi_MhD[j,2]=compare(pred2,group.test,method='nmi')
  
  incorrect3<-pred3!=group.test
  mis_MhD[j,3]=mean(incorrect3)
  nmi_MhD[j,3]=compare(pred3,group.test,method='nmi')
  
  incorrect4<-pred4!=group.test
  mis_MhD[j,4]=mean(incorrect4)
  nmi_MhD[j,4]=compare(pred4,group.test,method='nmi')
  
  incorrect5<-pred5!=group.test
  mis_MhD[j,5]=mean(incorrect5)
  nmi_MhD[j,5]=compare(pred5,group.test,method='nmi')
  
  incorrect6<-pred6!=group.test
  mis_MhD[j,6]=mean(incorrect6)
  nmi_MhD[j,6]=compare(pred6,group.test,method='nmi')
  
  #random projection depth
  out1=classif.DD(group.train,x.train,depth="RP",classif="glm")
  out2=classif.DD(group.train,x.train,depth="RP",classif="gam")
  out3=classif.DD(group.train,x.train,depth="RP",classif="lda")
  out4=classif.DD(group.train,x.train,depth="RP",classif="qda")
  out5=classif.DD(group.train,x.train,depth="RP",classif="knn")
  out6=classif.DD(group.train,x.train,depth="RP",classif="np")
  
  pred1=predict.classif.DD(out1,x.test)
  pred2=predict.classif.DD(out2,x.test)
  pred3=predict.classif.DD(out3,x.test)
  pred4=predict.classif.DD(out4,x.test)
  pred5=predict.classif.DD(out5,x.test)
  pred6=predict.classif.DD(out6,x.test)
  
  incorrect1<-pred1!=group.test
  mis_RP[j,1]=mean(incorrect1)
  nmi_RP[j,1]=compare(pred1,group.test,method='nmi')
  
  incorrect2<-pred2!=group.test
  mis_RP[j,2]=mean(incorrect2)
  nmi_RP[j,2]=compare(pred2,group.test,method='nmi')
  
  incorrect3<-pred3!=group.test
  mis_RP[j,3]=mean(incorrect3)
  nmi_RP[j,3]=compare(pred3,group.test,method='nmi')
  
  incorrect4<-pred4!=group.test
  mis_RP[j,4]=mean(incorrect4)
  nmi_RP[j,4]=compare(pred4,group.test,method='nmi')
  
  incorrect5<-pred5!=group.test
  mis_RP[j,5]=mean(incorrect5)
  nmi_RP[j,5]=compare(pred5,group.test,method='nmi')
  
  incorrect6<-pred6!=group.test
  mis_RP[j,6]=mean(incorrect6)
  nmi_RP[j,6]=compare(pred6,group.test,method='nmi')  
}

















