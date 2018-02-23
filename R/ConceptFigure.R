
setwd("E:/Git_Repo/NHLDLakes")
source('R/VariogramFunctions.R')

library(gstat)
library(dplyr)
library(tidyr)
source()

x<-rep(1:16, 16)

z1<-c(rep(c(rep(1,8), rep(0,8)), 8), rep(c(rep(0,8), rep(1,8)), 8))

z2<-rep(c(rep(c(rep(1,4), rep(0,4)), 8), rep(c(rep(0,4), rep(1,4)), 8)), 2)

c1<-c(1,1,0,0)
c2<-c(0,0,1,1)
z3<-rep(c(rep(c1, 8), rep(c2,8)), 4)

c3<-0:1
c4<-1:0
z4<-rep(c(rep(c3,8), rep(c4,8)),8)

z1[which(z1==0)]<-rnorm(2,0.2)
z1[which(z1==1)]<-rnorm(1,0.2)

z2[which(z2==0)]<-rnorm(2,0.2)
z2[which(z2==1)]<-rnorm(1,0.2)

z3[which(z3==0)]<-rnorm(2,0.2)
z3[which(z3==1)]<-rnorm(1,0.2)

z4[which(z4==0)]<-rnorm(2,0.2)
z4[which(z4==1)]<-rnorm(1,0.2)


m1<-matrix(z1, nrow=16, ncol=16)
m2<-matrix(z2, nrow=16, ncol=16)
m3<-matrix(z3, nrow=16, ncol=16)
m4<-matrix(z4, nrow=16, ncol=16)

d1<-as.data.frame(m1)
names(d1)<- c(1:16)
d1$x<-(1:16)

d1<-gather(d1, key='y', value='z', 1:16)
d1$y<-as.numeric(d1$y)
coordinates(d1) <- ~x+y

d2<-d1
d2$z<-z2
d3<-d1
d3$z<-z3
d4<-d1
d4$z<-z4

v1 = variogram(z~1, data=d1, cutoff=20, width=0.5)
fit1<-fit.variogram(v1, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v1, fit1)
plot(v1, fit1)

v2 = variogram(z~1, data=d2, cutoff=20, width=0.5)
fit2<-fit.variogram(v2, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v2, fit2)
plot(v2, fit2)

v3 = variogram(z~1, data=d3, cutoff=20, width=0.5)
fit3<-fit.variogram(v3, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v3, fit3)
plot(v3, fit3)

v4 = variogram(z~1, data=d4, cutoff=20, width=0.5)
plot(v4)
fit4<-fit.variogram(v4, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v4, fit4)
plot(v1, fit1)

par(mfrow=c(2,2), mar=c(2,2,.5,.5), oma=c(2,2,0,0))

vplot(v1, fit1, xlab='', ylab='')
vplot(v2, fit2, xlab='', ylab='')
vplot(v3, fit3, xlab='', ylab='')
vplot(v4, fit4, xlab='', ylab='')
mtext('Distance',1,1, outer=T)
mtext('Semivariance',2,1, outer=T)



z5<-c(rep(c(rep(1,8), rep(0,8)), 16), rep(c(rep(0,8), rep(1,8)), 16))
m5<-matrix(z5, nrow=32, ncol=32)
m5

z6<-rep(c(rep(c(rep(1,4), rep(0,4)), 16), rep(c(rep(0,4), rep(1,4)), 16)), 2)
m6<-matrix(z6, nrow=32, ncol=32)
m6

z7<-rep(c(rep(c1, 16), rep(c2,16)), 4)
m7<-matrix(z7, nrow=32, ncol=32)
m7


z8<-rep(c(rep(c3,16), rep(c4,16)),8)
m8<-matrix(z8, nrow=32, ncol=32)
m8

z5[which(z5==0)]<-rnorm(2,0.2)
z5[which(z5==1)]<-rnorm(1,0.2)

z6[which(z6==0)]<-rnorm(2,0.2)
z6[which(z6==1)]<-rnorm(1,0.2)

z7[which(z7==0)]<-rnorm(2,0.2)
z7[which(z7==1)]<-rnorm(1,0.2)

z8[which(z8==0)]<-rnorm(2,0.2)
z8[which(z8==1)]<-rnorm(1,0.2)



d5<-as.data.frame(m5)
names(d5)<- c(1:32)
d5$x<-(1:32)

d5<-gather(d5, key='y', value='z', 1:32)
d5$y<-as.numeric(d5$y)
coordinates(d5) <- ~x+y

d6<-d5
d6$z<-z6
d7<-d5
d7$z<-z7
d8<-d5
d8$z<-z8

v5 = variogram(z~1, data=d5, cutoff=20, width=0.5)
fit5<-fit.variogram(v5, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v5, fit5)

v6 = variogram(z~1, data=d6, cutoff=20, width=0.5)
fit6<-fit.variogram(v6, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v6, fit6)


v7 = variogram(z~1, data=d7, cutoff=20, width=0.5)
fit7<-fit.variogram(v7, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v7, fit7)

v8 = variogram(z~1, data=d8, cutoff=20, width=0.5)
fit8<-fit.variogram(v8, vgm(c("Lin", "Sph")), fit.method = 2)
vplot(v8, fit8)

par(mfrow=c(2,2), mar=c(2,2,.5,.5), oma=c(2,2,0,0))

vplot(v5, fit5, xlab='', ylab='')
vplot(v6, fit6, xlab='', ylab='')
vplot(v7, fit7, xlab='', ylab='')
vplot(v8, fit8, xlab='', ylab='')
mtext('Distance',1,1, outer=T)
mtext('Semivariance',2,1, outer=T)


