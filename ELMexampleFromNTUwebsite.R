f<-matrix(0,12,10)
f<-read.table("New Microsoft Excel Worksheet.txt",header = TRUE)
f
input<-matrix(0,12,9)
for(i in 1:9)
  {
   input[,i]=f[,i+1] 
}
input

target<-matrix(0,1,12)
for(i in 1:12)
{
  target[,i]=f[i,1]
}
target

weight<-matrix(0,9,11)
  for(i in 1:9)
  {
    for(j in 1:11)
    {
      weight[i,j]=runif(1,-1,1)
    }
  }
weight

bias<-matrix(0,12,11)
for(i in 1:12)
{
  for(j in 1:11)
  {
    bias[i,j]=runif(1,-1,1)
  }
}
bias

temph<-matrix(0,12,11)
temph=(input%*%weight) +bias
temph

h<-matrix(0,12,11)
for(i in 1:12)
{
  for(j in 1:11)
  {
    h[i,j]=1 / (1 + exp(-(temph[i,j])))
  }
}
h

beta=(solve(t(h)%*%h)) %*% (t(h)) %*% (t(target))
beta

ctarget=h%*%beta
ctarget

f1<-matrix(0,3,10)
f1<-read.table("New Microsoft Excel Worksheet (2).txt",header = FALSE)
f1

inputtest<-matrix(0,3,9)
for(i in 1:9)
{
  inputtest[,i]=f1[,i+1] 
}
inputtest

targettest<-matrix(0,3,1)
for(i in 1:3)
{
  targettest[i,]=f1[i,1]
}
targettest

biastest<-matrix(0,3,11)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}
biastest

temphtest=(inputtest%*%weight) + biastest
temphtest

htest<-matrix(0,3,11)
for(i in 1:3)
{
  for(j in 1:11)
  {
    htest[i,j]=1 / (1 + exp(-(temphtest[i,j])))
  }
}
htest

ttest=htest%*%beta
ttest
