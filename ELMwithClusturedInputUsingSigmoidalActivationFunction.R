#parameter generation

datamatrix<-matrix(0,15,13)
for(i in 1:15)
{
  for(j in 1:4)
  {
    datamatrix[i,j]=runif(1,0,0.5)
  }
}
a1=datamatrix[,1]+datamatrix[,2]
datamatrix[,5]=a1
a2=datamatrix[,2]+datamatrix[,3]
datamatrix[,6]=a2
a3=datamatrix[,3]+datamatrix[,4]
datamatrix[,7]=a3
a4=datamatrix[,1]+datamatrix[,4]
datamatrix[,8]=a4
a5=datamatrix[,1]+datamatrix[,2]+datamatrix[,3]
datamatrix[,9]=a5
a6=datamatrix[,2]+datamatrix[,3]+datamatrix[,4]
datamatrix[,10]=a6
a7=datamatrix[,3]+datamatrix[,4]+datamatrix[,1]
datamatrix[,11]=a7
a8=datamatrix[,4]+datamatrix[,1]+datamatrix[,2]
datamatrix[,12]=a8
a9=datamatrix[,1]+datamatrix[,2]+datamatrix[,3]+datamatrix[,4]
datamatrix[,13]=a9
datamatrix

temp=rowSums(datamatrix)
temp

targetmatrix<-matrix(0,1,15)
{
  for(i in 1:15)
  {
    if(temp[i]<=6.6)
    {
      targetmatrix[,i]=-1
    }
    if(temp[i]>6.6)
    {
      targetmatrix[,i]=1
    }
  }
}
targetmatrix

weightmatrix<-matrix(0,13,14)
for(i in 1:13)
{
  for(j in 1:14 )
  {
    weightmatrix[i,j]=runif(1,-1,1)
  }
}
weightmatrix

bias<-matrix(0,15,14)
for(i in 1:14)
{
  bias[,i]=runif(1,-1,1)
}
bias

#calculating the hidden layer matrix

temphidden<-matrix(0,15,14)
temphidden=(datamatrix%*%weightmatrix) +bias
temphidden

hidden<-matrix(0,15,14)
for(i in 1:15)
{
  for(j in 1:14)
  {
    hidden[i,j]=1 / (1 + exp(-(temphidden[i,j])))
  }
}
hidden

#to calculate beta

beta=(solve(t(hidden)%*%hidden)) %*% (t(hidden)) %*% (t(targetmatrix))
beta

#calculating target

calculatedtarget=hidden%*%beta
calculatedtarget

#testing

datamatrixtest<-matrix(0,1,13)
for(i in 1:1)
{
  for(j in 1:4)
  {
    datamatrixtest[i,j]=runif(1,0,0.5)
  }
}
x1=datamatrixtest[,1]+datamatrixtest[,2]
datamatrixtest[,5]=x1
x2=datamatrixtest[,2]+datamatrixtest[,3]
datamatrixtest[,6]=x2
x3=datamatrixtest[,3]+datamatrixtest[,4]
datamatrixtest[,7]=x3
x4=datamatrixtest[,1]+datamatrixtest[,4]
datamatrixtest[,8]=x4
x5=datamatrixtest[,1]+datamatrixtest[,2]+datamatrixtest[,3]
datamatrixtest[,9]=x5
x6=datamatrixtest[,2]+datamatrixtest[,3]+datamatrixtest[,4]
datamatrixtest[,10]=x6
x7=datamatrixtest[,3]+datamatrixtest[,4]+datamatrixtest[,1]
datamatrixtest[,11]=x7
x8=datamatrixtest[,4]+datamatrixtest[,1]+datamatrixtest[,2]
datamatrixtest[,12]=x8
x9=datamatrixtest[,1]+datamatrixtest[,2]+datamatrixtest[,3]+datamatrixtest[,4]
datamatrixtest[,13]=x9
datamatrixtest

temptest=rowSums(datamatrixtest)
temptest

targetmatrixtest<-matrix(0,1,1)
{
  for(i in 1:1)
  {
    if(temptest[i]<=6.6)
    {
      targetmatrixtest[,i]=-1
    }
    if(temptest[i]>6.6)
    {
      targetmatrixtest[,i]=1
    }
  }
}
targetmatrixtest

biastest<-matrix(0,1,14)
for(i in 1:1)
{
  biastest[i,]=bias[i,]
}
biastest

temphiddentest=(datamatrixtest%*%weightmatrix) + biastest
temphiddentest

hiddentest<-matrix(0,1,14)
for(i in 1:1)
{
  for(j in 1:14)
  {
    hiddentest[i,j]=1 / (1 + exp(-(temphiddentest[i,j])))
  }
}
hiddentest

calculatedtargettest=hiddentest%*%beta
calculatedtargettest