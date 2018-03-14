

rm(list = ls())

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(vioplot)
library(ggplot2)


setwd("E:/Git_Repo/NHLDLakes")
mylagos2<-readRDS(file='Data/MyLakesLagos.rds')


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




a<-left_join(range_model, mylagos2, by='flame_name')
b<-left_join(range_best, mylagos2, by='flame_name')
x<-seq(0,max(a$lake_area_ha, na.rm=T)*1.05)
y<-sqrt(x*10000)

b$Colour <- as.character(cut(b$tp, breaks = seq(0,60,10), labels = brewer.pal(9, "PuBuGn")[3:8]))
b$pch <- cut(b$lake_area_ha, breaks=c(0,10,20,40,80,160,320,640,2000), labels=seq(.5,4,.5))

b$pch <- as.numeric(as.character(b$pch))
b$pch[which(is.na(b$pch))] <- 1

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



#Semivariogram range boxplots withcutoff
png("Figures/ScatterplotAreaTempRange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$lake_area_ha, b$TmpC_h, ylim=c(0,max(y)*1.1), xaxs='i', xlim=c(0, max(x)), pch=16, xlab='', ylab='', col=b$Colour)
lines(x,y, lty=1)
text(1100,3500, '~Maximum meaningful range', srt=27, cex=0.8)
mtext('Lake area (ha)',1,2)
mtext('Temperature semivariogram range (m)',2,2)

dev.off()

#Semivariogram range boxplots
png("Figures/ScatterplotAreaBGARange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$lake_area_ha, b$BGAPCRFU_h, ylim=c(0,max(y)*1.1), xaxs='i', xlim=c(0, max(x)), pch=16, xlab='', ylab='', col=b$Colour)
lines(x,y, lty=1)
text(1100,3500, '~Maximum meaningful range', srt=27, cex=0.8)
mtext('Lake area (ha)',1,2)
mtext('BGA semivariogram range (m)',2,2)

dev.off()


#Semivariogram range boxplots
png("Figures/ScatterplotAreachlaRange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$lake_area_ha, b$ChlARFU_h, ylim=c(0,max(y)*1.1), xaxs='i', xlim=c(0, max(x)), pch=16, xlab='', ylab='', col=b$Colour)
lines(x,y, lty=1)
text(1100,3500, '~Maximum meaningful range', srt=27, cex=0.8)
mtext('Lake area (ha)',1,2)
mtext('Chl a semivariogram range (m)',2,2)

dev.off()


#Semivariogram range boxplots
png("Figures/ScatterplotTPchlaRange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$tp, b$ChlARFU_h, ylim=c(0,max(y)*1.1), pch=16, xlab='', ylab='', col=b$Colour)
mtext('TP (ug/L)',1,2)
mtext('Chl a semivariogram range (m)',2,2)

dev.off()


#Semivariogram range boxplots
png("Figures/ScatterplotDOCchlaRange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$doc, b$ChlARFU_h, ylim=c(0,max(y)*1.1), pch=16, xlab='', ylab='', col=b$Colour)
mtext('TP (ug/L)',1,2)
mtext('Chl a semivariogram range (m)',2,2)

dev.off()



#Semivariogram range boxplots
png("Figures/ScatterplotTPTempoverchlaRange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$tp, b$TmpC_h/b$ChlARFU_h, col=b$Colour, pch=16, cex=b$pch*.75, xlab='', ylab='')
abline(h=1, lty=3)

mtext('TP (ug/L)',1,2)
mtext('Temp/Chl a semivariogram range',2,2)

dev.off()

#Semivariogram range boxplots
png("Figures/ScatterplotAreaTempoverchlaRange_withcutoff.png", res=200, width=4,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-.02)

plot(b$lake_area_ha, b$TmpC_h/b$ChlARFU_h, col=b$Colour, cex=b$pch*.75, pch=16, xlab='', ylab='')
abline(h=1, lty=3)

mtext('Lake area (ha)',1,2)
mtext('Temp/Chl a semivariogram range',2,2)

dev.off()