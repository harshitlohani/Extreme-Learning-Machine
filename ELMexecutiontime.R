atan<-c(0.75,0.68,0.77,0.85,0.76)
leaky<-c(0.71,0.825,0.77,0.79,0.81)
prelu<-c(0.86,0.79,0.75,0.82,0.72)
sig<-c(0.51,0.48,0.53,0.51,0.57)
softmax<-c(0.49,0.5,0.51,0.52,0.53)
tnh<-c(0.487,0.50,0.525,0.54,0.537)
hn<-c(50,100,300,500,1000)
plot(-1,-1,xlim=c(0,1000), ylim=c(0.4,1.5),
     xlab="No. of Hidden Nodes", ylab="Execution time (in secs)",
     main="Comparision of Execution time", col.main="red",
     col.lab="green")
for(i in 1:1)
{
  lines(hn,atan,col = "red",type = 'b')
  lines(hn,leaky,col = "darkgoldenrod",type = 'b')
  lines(hn,prelu,col = "blue",type = 'b')
  lines(hn,sig,col = "deepskyblue1",type = 'b')
  lines(hn,softmax,col = "green",type = 'b')
  lines(hn,tnh,col = "orange",type = 'b')
}
legend(0, 1.5, legend=c("Inv. Tan", "Leaky Relu","PRelu","sigmoidal","Softmax","tanh"),
       col= c("red","darkgoldenrod","blue","deepskyblue1","green","orange"), lty=1:2, cex=0.8)

