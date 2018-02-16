
rm(list = ls())

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(vioplot)
library(ggplot2)

setwd("E:/Dropbox/FLAME_NHLDLakes/")

moran_ncf_alllakes <- readRDS( file='SpatialOutputs/moran_ncf_alllakes.rds')
semivar_alllakes <- readRDS(file='SpatialOutputs/semivar_alllakes.rds')

boxnames<-c('Temp', 'SPC', 'Turb', 'fDOM', 'pH','DO', 'ChlA', 'BGA')
varcolors<-c('#377eb8', '#e41a1c', '#4daf4a')

plot(semivar_alllakes$range95, semivar_alllakes$range_best)
abline(0,1)

range_best <-
  semivar_alllakes %>%
  select(lake_day, variable, range_best) %>%
  spread(key=variable, value=range_best) %>%
  select(lake_day, TmpC_h, SPCScm_h, TrbFNU_h, fDOMRFU_h,  pH_h, ODOmgL_h, ChlARFU_h, BGAPCRFU_h)


boxplot(range_best[2:ncol(range_best)], names=boxnames)

range_95 <-
  semivar_alllakes %>%
  select(lake_day, variable, range95) %>%
  spread(key=variable, value=range95) %>%
  select(lake_day, TmpC_h, SPCScm_h, TrbFNU_h, fDOMRFU_h,  pH_h, ODOmgL_h, ChlARFU_h, BGAPCRFU_h)

boxplot(range_95[2:ncol(range_95)], names=boxnames)

range_model <-
  semivar_alllakes %>%
  select(lake_day, variable, model_range) %>%
  spread(key=variable, value=model_range) %>%
  select(lake_day, TmpC_h, SPCScm_h, TrbFNU_h, fDOMRFU_h,  pH_h, ODOmgL_h, ChlARFU_h, BGAPCRFU_h)


#Semivariogram range boxplots
png("Figures/BoxplotRanges.png", res=200, width=6,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=0)
boxplot(range_model[,2:ncol(range_model)], names=boxnames, log='y', yaxt='n', col=varcolors[c(1,1,1,2,2,2,3,3)], pch=16, cex=0.5, boxwex=0.6)

axis(2, at=c(1,10,100,1000,10000, 100000), labels=c(expression('10'^'1'), expression('10'^'2'), expression('10'^'3'), expression('10'^'4'), expression('10'^'5'), expression('10'^'6')), las=1, tck=0.01)
mtext('Semivariogram range (m)', 2, 2)

dev.off()

#Semivariogram range boxplots
png("Figures/BoxplotRangeskm.png", res=200, width=6,height=4, units="in")

par(mar=c(2,3,.5,.5))
par(mgp=c(3,.3,0), tck=0)
boxplot(range_model[,2:ncol(range_model)]/1000, names=boxnames, log='y', yaxt='n', col=varcolors[c(1,1,1,2,2,2,3,3)], pch=16, cex=0.5, boxwex=0.6)

axis(2, at=10^(-2:3), labels=10^(-2:3), las=1, tck=0.02)
mtext('Semivariogram range (km)', 2, 2)

dev.off()


boxplot(range_model[,2:ncol(range_model)], names=boxnames, ylim=c(0,5000))
vioplot(range_model[,2], range_model[,3], range_model[,4],range_model[,5],range_model[,6],range_model[,7],names=boxnames[1:6])





colors<-brewer.pal(ncol(range_best)-1,"Dark2")

column<-2
for (column in 2:ncol(ranges)){
  if (column ==2){
    plot(ranges$TmpC_h, ranges[,column], col=colors[column-1], pch=16, xlab='', ylab='')
    abline(0,1, lty=3)
    mtext('Range temperature (m)', 1,3)
    mtext('Range other variabless (m)', 2,3)
  } else {
    points(semivar_alllakes$range_best[semivar_alllakes$variable==lessvars[1]], semivar_alllakes$range_best[semivar_alllakes$variable==vars[var]], col=colors[var], pch=16)
  }
}


points(semivar_alllakes$range_best[semivar_alllakes$variable==vars[1]], semivar_alllakes$range_best[semivar_alllakes$variable==vars[3]], col=colors[2], pch=16)

abline(0,1, lty=3)
