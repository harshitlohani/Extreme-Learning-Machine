t1=Sys.time()
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

weight<-matrix(0,9,50)
for(i in 1:9)
{
  for(j in 1:50)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,50)
for(i in 1:12)
{
  for(j in 1:50)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,50)
temph=(input%*%weight) +bias

h<-matrix(0,12,50)
for(i in 1:12)
{
  for(j in 1:50)
  {
    h[i,j]=atan(temph[i,j])
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

biastest<-matrix(0,3,50)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,50)
for(i in 1:3)
{
  for(j in 1:50)
  {
    htest[i,j]=atan(temphtest[i,j])
  }
}

ttest=htest%*%beta
ttest

error<-matrix(0,3,1)
for(i in 1:3)
{
  error[i,]=abs(targettest[i,]-ttest[i,])
}
err1=(colSums(error))/3
t2=Sys.time()
overalltime=t2-t1
overalltime