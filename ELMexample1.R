#initializing the parameters

x<-matrix(0,4,2)
x[1,1]=-2.2
x[1,2]=1.8
x[2,1]=-1.2
x[2,2]=-1.4
x[3,1]=2.6
x[3,2]=-1.7
x[4,1]=1.9
x[4,2]=2.1
x

w<-matrix(0,2,3)
w[1,1]=-0.3766
w[1,2]=0.0571
w[1,3]=-0.6687
w[2,1]=0.2040
w[2,2]=-0.4741
w[2,3]=0.3082
w

b<-matrix(0,4,3)
b[,1]=0.6892
b[,2]=0.7482
b[,3]=0.4505
b

#calculating the h matrix

temph<-matrix(0,4,3)
temph=(x%*%w) +b
temph
h<-matrix(0,4,3)
for(i in 1:4)
{
  for(j in 1:3)
  {
    h[i,j]=1 / (1 + exp(-(temph[i,j])))
  }
}
h
t<-matrix(0,1,4)
t[1,1]=-1
t[1,2]=1
t[1,3]=-1
t[1,4]=1
t
#to calculate beta

beta=(solve(t(h)%*%h)) %*% (t(h)) %*% (t(t))
beta

#calculating target

target=h%*%beta
target

#testing 

xtest<-matrix(0,1,2)
xtest[1,1]=2.5
xtest[1,2]=-1.3

a=nrow(xtest)
btest<-matrix(0,a,3)
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
    htest[i,j]=1 / (1 + exp(-(temph[i,j])))
  }
}
htest

ttest=htest%*%beta
ttest

desired=-1
ttest=htest%*%beta
ttest

error=desired+ttest
error=abs(error)
error
accuracy=(1-error)*100
accuracy
