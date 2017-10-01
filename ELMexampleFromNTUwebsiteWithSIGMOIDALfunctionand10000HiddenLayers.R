f<-matrix(0,12,10)
f<-read.table("New Microsoft Excel Worksheet.txt",header = TRUE)

input<-matrix(0,12,9)
for(i in 1:9)
{
  input[,i]=f[,i+1] 
}

target<-matrix(0,1,12)
for(i in 1:12)
{
  target[,i]=f[i,1]
}

weight<-matrix(0,9,10000)
for(i in 1:9)
{
  for(j in 1:10000)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,10000)
for(i in 1:12)
{
  for(j in 1:10000)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,10000)
temph=(input%*%weight) +bias

h<-matrix(0,12,10000)
for(i in 1:12)
{
  for(j in 1:10000)
  {
    h[i,j]=1 / (1 + exp(-(temph[i,j])))
  }
}

beta=t(h)%*%(solve(h%*%t(h)))%*%t(target)

ctarget=h%*%beta

f1<-matrix(0,3,10)
f1<-read.table("New Microsoft Excel Worksheet (2).txt",header = FALSE)

inputtest<-matrix(0,3,9)
for(i in 1:9)
{
  inputtest[,i]=f1[,i+1] 
}

targettest<-matrix(0,3,1)
for(i in 1:3)
{
  targettest[i,]=f1[i,1]
}

biastest<-matrix(0,3,10000)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,10000)
for(i in 1:3)
{
  for(j in 1:10000)
  {
    htest[i,j]=1 / (1 + exp(-(temphtest[i,j])))
  }
}

ttest=htest%*%beta
ttest

error<-matrix(0,3,1)
for(i in 1:3)
{
  error[i,]=abs(targettest[i,]-ttest[i,])
}
err=(colSums(error))/3
err