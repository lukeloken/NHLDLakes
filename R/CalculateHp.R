

CalculateH<-function(x, a=NULL){
  
  if(is.null(a)){a= rep(1, length(x))}
  
  M<-x*a
  M_order<-M[order(M, decreasing=T)]
  A_order<-a[order(M, decreasing=T)]
  
  Mprime<-cumsum(M_order)/sum(M_order)
  Aprime<-cumsum(A_order)/sum(A_order)
  
  dAprime<-diff(c(0,Aprime))
  
  h<-(1-sum(Mprime*dAprime))/.5*100
  
  print(plot(Aprime, Mprime, xlim=c(0,1), ylim=c(0,1)))
  abline(0,1)
  
  return(h)
}

CV<-function (x){
  sd(x)/mean(x)
}

x<-c(.021,2,3,2,5,4,3,5,7,4)
x<-seq(1:10)
a<-rep(1,10)
a<-c(1,1,1,1,1,2,2,2,2,2)
x<-sample(x)

x<-c(100,200,300,400,5)
par(mar=c(3,3,.5,.5), mgp=c(2,.5,0), tck=(-.02))
CalculateH(x)
CV(x)

CalculateH(x*7)

