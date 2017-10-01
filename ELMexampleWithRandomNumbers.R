#initializing the parameters

x<-matrix(0,15,9)
for(i in 1:15)
{
  for(j in 1:9)
  {
    x[i,j]=runif(1,-1,1)
  }
}
x

w<-matrix(0,9,14)
for(i in 1:9)
{
  for(j in 1:14 )
  {
    w[i,j]=runif(1,-1,1)
  }
}
w

b<-matrix(0,15,14)
for(i in 1:14)
{
  b[,i]=runif(1,-1,1)
}
b

#calculating the h matrix

temph<-matrix(0,15,14)
temph=(x%*%w) +b
temph
h<-matrix(0,15,14)
for(i in 1:15)
{
  for(j in 1:14)
  {
    h[i,j]=1 / (1 + exp(-(temph[i,j])))
  }
}
h
t<-matrix(0,1,15)
for(i in 1:15)
{
  t[,i]=runif(1,-1,1)
}
t
#to calculate beta

beta=(solve(t(h)%*%h)) %*% (t(h)) %*% (t(t))
beta

#calculating target

target=h%*%beta
target

#testing 

xtest<-matrix(0,1,9)
for(i in 1:9)
{
  xtest[,i]=runif(1,-1,1)
}

a=nrow(xtest)
btest<-matrix(0,a,14)
for(i in 1:a)
{
  btest[i,]=b[i,]
}
btest

temphtest=(xtest%*%w) + btest
temphtest
b=ncol(temphtest)

htest<-matrix(0,a,b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    htest[i,j]=1 / (1 + exp(-(temphtest[i,j])))
  }
}
htest

ttest=htest%*%beta
ttest