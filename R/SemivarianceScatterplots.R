

rm(list = ls())

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(vioplot)
library(ggplot2)

setwd("E:/Dropbox/FLAME_NHLDLakes/")

range_model <- readRDS(file='SpatialOutputs/rangemodeltable.rds')
range_95 <- readRDS(file='SpatialOutputs/range95table.rds')
range_best <- readRDS(file='SpatialOutputs/rangebesttable.rds')

range_model$flame_name <- gsub(".*_","",range_model$lake_day)
range_model$flame_name <- gsub("[0-9]","",range_model$flame_name )

range_95 $flame_name <- gsub(".*_","",range_95$lake_day)
range_95 $flame_name <- gsub("[0-9]","",range_95 $flame_name )

range_best$flame_name <- gsub(".*_","",range_best$lake_day)
range_best$flame_name <- gsub("[0-9]","",range_best$flame_name )


setwd("E:/Git_Repo/NHLDLakes")
mylagos2<-readRDS(file='Data/MyLakesLagos.rds')

a<-left_join(range_model, mylagos2, by='flame_name')

x<-seq(0,max(a$lake_area_ha, na.rm=T)*1.05)
y<-sqrt(x*10000)

#Semivariogram range boxplots
png("Figures/ScatterplotAreaTempRange.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(a$lake_area_ha, a$TmpC_h, ylim=c(0,max(y)*1.1), xaxs='i', xlim=c(0, max(x)), pch=16, xlab='', ylab='')
lines(x,y, lty=1)
text(1100,3500, 'Maximum meaningful range', srt=27, cex=0.8)
mtext('Lake area (ha)',1,2)
mtext('Temperature semivariogram range (m)',2,2)

dev.off()

#Semivariogram range boxplots
png("Figures/ScatterplotAreaBGARange.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(a$lake_area_ha, a$BGAPCRFU_h, ylim=c(0,max(y)*1.1), xaxs='i', xlim=c(0, max(x)), pch=16, xlab='', ylab='')
lines(x,y, lty=1)
text(1100,3500, 'Maximum meaningful range', srt=27, cex=0.8)
mtext('Lake area (ha)',1,2)
mtext('BGA semivariogram range (m)',2,2)

dev.off()
