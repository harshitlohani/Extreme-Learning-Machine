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

weight<-matrix(0,9,500)
for(i in 1:9)
{
  for(j in 1:500)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,500)
temph=(input%*%weight) +bias

h<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
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

biastest<-matrix(0,3,500)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,500)
for(i in 1:3)
{
  for(j in 1:500)
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

weight<-matrix(0,9,500)
for(i in 1:9)
{
  for(j in 1:500)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,500)
temph=(input%*%weight) +bias

h<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    h[i,j]<-if(temph[i,j] > 0) temph[i,j] else 0.01*temph[i,j]
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

biastest<-matrix(0,3,500)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,500)
for(i in 1:3)
{
  for(j in 1:500)
  {
    htest[i,j]<- if(temphtest[i,j] > 0) temphtest[i,j] else 0.01*temphtest[i,j]
  }
}

ttest=htest%*%beta
ttest

error<-matrix(0,3,1)
for(i in 1:3)
{
  error[i,]=abs(targettest[i,]-ttest[i,])
}
err2=(colSums(error))/3
err2

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

weight<-matrix(0,9,500)
for(i in 1:9)
{
  for(j in 1:500)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,500)
temph=(input%*%weight) +bias

h<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    h[i,j]<-if(temph[i,j] > 0) temph[i,j] else 0.00001*temph[i,j]
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

biastest<-matrix(0,3,500)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,500)
for(i in 1:3)
{
  for(j in 1:500)
  {
    htest[i,j]<- if(temphtest[i,j] > 0) temphtest[i,j] else 0.00001*temphtest[i,j]
  }
}

ttest=htest%*%beta
ttest

error<-matrix(0,3,1)
for(i in 1:3)
{
  error[i,]=abs(targettest[i,]-ttest[i,])
}
err3=(colSums(error))/3
err3

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

weight<-matrix(0,9,500)
for(i in 1:9)
{
  for(j in 1:500)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,500)
temph=(input%*%weight) +bias

h<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
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

biastest<-matrix(0,3,500)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,500)
for(i in 1:3)
{
  for(j in 1:500)
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
err4=(colSums(error))/3
err4

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

weight<-matrix(0,9,500)
for(i in 1:9)
{
  for(j in 1:500)
  {
    weight[i,j]=runif(1,-1,1)
  }
}

bias<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    bias[i,j]=runif(1,-1,1)
  }
}

temph<-matrix(0,12,500)
temph=(input%*%weight) +bias

h<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    h[i,j]=temph[i,j]/(1+abs(temph[i,j]))
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

biastest<-matrix(0,3,500)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,500)
for(i in 1:3)
{
  for(j in 1:500)
  {
    htest[i,j]=temphtest[i,j]/(1+abs(temphtest[i,j]))
  }
}

ttest=htest%*%beta
ttest

error<-matrix(0,3,1)
for(i in 1:3)
{
  error[i,]=abs(targettest[i,]-ttest[i,])
}
err5=(colSums(error))/3
err5

f<-matrix(0,12,10)
f<-read.table("New Microsoft Excel Worksheet.txt",header = TRUE)
a=0.01

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

weight<-matrix(0,9,500)
for(i in 1:9)
{
  for(j in 1:500)
  {
    weight[i,j]=runif(1)
  }
}

bias<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    bias[i,j]=runif(1)
  }
}

temph<-matrix(0,12,500)
temph=(input%*%weight) +bias

h<-matrix(0,12,500)
for(i in 1:12)
{
  for(j in 1:500)
  {
    h[i,j]=tanh(temph[i,j])
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

biastest<-matrix(0,3,500)
for(i in 1:3)
{
  biastest[i,]=bias[i,]
}

temphtest=(inputtest%*%weight) + biastest

htest<-matrix(0,3,500)
for(i in 1:3)
{
  for(j in 1:500)
  {
    htest[i,j]=tanh(temphtest[i,j])
  }
}

ttest=htest%*%beta
ttest

error<-matrix(0,3,1)
for(i in 1:3)
{
  error[i,]=abs(targettest[i,]-ttest[i,])
}
err6=(colSums(error))/3
err6

comparisionofaccuracy<-array(0,6)
comparisionofaccuracy[1]=((colSums(targettest)-err1)/(colSums(targettest)))*100
comparisionofaccuracy[2]=((colSums(targettest)-err2)/(colSums(targettest)))*100
comparisionofaccuracy[3]=((colSums(targettest)-err3)/(colSums(targettest)))*100
comparisionofaccuracy[4]=((colSums(targettest)-err4)/(colSums(targettest)))*100
comparisionofaccuracy[5]=((colSums(targettest)-err5)/(colSums(targettest)))*100
comparisionofaccuracy[6]=((colSums(targettest)-err6)/(colSums(targettest)))*100
comparisionofaccuracy

plot(comparisionofaccuracy, type = "p", xlim=c(1, 6), ylim=c(0, 100), 
     main="Performance with Different Activation Functions with 500 Hidden Nodes", col.main="red",
     xlab="Activation Functions", ylab="Accuracy",
     col.lab="green") 
text(1,100,"atan")
text(2,100,"leakyRelu")
text(3,100,"PRelu")
text(4,100,"Sig.")
text(5,100,"Softmax")
text(6,100,"tanh")