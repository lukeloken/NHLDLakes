
rm(list = ls())

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(vioplot)
library(ggplot2)
library(scales)

setwd("E:/Dropbox/FLAME_NHLDLakes/")

moran_ncf_alllakes <- readRDS( file='SpatialOutputs/moran_ncf_alllakes.rds')
semivar_alllakes <- readRDS(file='SpatialOutputs/semivar_alllakes.rds')

write.table(semivar_alllakes, file='SpatialOutputs/semivar_alllakes.csv', sep=',', row.names=F)

boxnames<-c('Temp', 'SPC', 'Turb', 'fDOM', 'pH','DO', 'ChlA', 'BGA')
varcolors<-c('#377eb8', '#e41a1c', '#4daf4a')

b<-subset(semivar_alllakes, variable %in% c('TmpC_h', 'SPCScm_h', 'TrbFNU_h', 'fDOMRFU_h',  'pH_h', 'ODOmgL_h', 'ChlARFU_h', 'BGAPCRFU_h'))
b$names <- boxnames[match(b$variable, c('TmpC_h', 'SPCScm_h', 'TrbFNU_h', 'fDOMRFU_h',  'pH_h', 'ODOmgL_h', 'ChlARFU_h', 'BGAPCRFU_h'))]
b$names<-factor(b$names, levels=boxnames)

b[which(b[,c('range_best')]==Inf), c('range_best')]<-b[which(b[,c('range_best')]==Inf), c('cutoff')]



range_best <-
  b %>%
  dplyr::select(lake_day, variable, range_best, cutoff) %>%
  spread(key=variable, value=range_best) %>%
  dplyr::select(lake_day, TmpC_h, SPCScm_h, TrbFNU_h, fDOMRFU_h,  pH_h, ODOmgL_h, ChlARFU_h, BGAPCRFU_h)


boxplot(range_best[2:ncol(range_best)], names=boxnames)

range_95 <-
  semivar_alllakes %>%
  dplyr::select(lake_day, variable, range95) %>%
  spread(key=variable, value=range95) %>%
  dplyr::select(lake_day, TmpC_h, SPCScm_h, TrbFNU_h, fDOMRFU_h,  pH_h, ODOmgL_h, ChlARFU_h, BGAPCRFU_h)

boxplot(range_95[2:ncol(range_95)], names=boxnames)

range_model <-
  semivar_alllakes %>%
  dplyr::select(lake_day, variable, model_range) %>%
  spread(key=variable, value=model_range) %>%
  dplyr::select(lake_day, TmpC_h, SPCScm_h, TrbFNU_h, fDOMRFU_h,  pH_h, ODOmgL_h, ChlARFU_h, BGAPCRFU_h)


saveRDS(range_model, file='SpatialOutputs/rangemodeltable.rds')
saveRDS(range_95, file='SpatialOutputs/range95table.rds')
saveRDS(range_best, file='SpatialOutputs/rangebesttable.rds')


#Semivariogram range boxplots
png("Figures/BoxplotRanges.png", res=200, width=6,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=0)
boxplot(range_model[,2:ncol(range_model)], names=boxnames, log='y', yaxt='n', col=varcolors[c(1,1,1,2,2,2,3,3)], pch=16, cex=0.5, boxwex=0.6)

axis(2, at=c(1,10,100,1000,10000, 100000), labels=c(expression('10'^'1'), expression('10'^'2'), expression('10'^'3'), expression('10'^'4'), expression('10'^'5'), expression('10'^'6')), las=1, tck=0.01)
mtext('Semivariogram range (m)', 2, 2)

dev.off()

#Semivariogram range boxplots
png("Figures/BoxplotRanges_withcutoff.png", res=200, width=6,height=4, units="in")

par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=0.01)
boxplot(range_best[,3:ncol(range_best)], names=boxnames, col=varcolors[c(1,1,1,2,2,2,3,3)], pch=NA, boxwex=0.6, outlier=NA, ylim=c(0,2000))

mtext('Semivariogram range (m)', 2, 2)

dev.off()

#Semivariogram range boxplots
png("Figures/BoxplotRangeskm_NoOutliers.png", res=200, width=6,height=4, units="in")

par(mar=c(2,3,.5,.5))
par(mgp=c(3,.3,0), tck=0)
boxplot(range_model[,2:ncol(range_model)]/1000, names=boxnames, log='y', yaxt='n', col=varcolors[c(1,1,1,2,2,2,3,3)], pch=16, cex=0.5, boxwex=0.6, outline=F)

axis(2, at=10^(-2:3), labels=10^(-2:3), las=1, tck=0.02)
mtext('Semivariogram range (km)', 2, 2)

dev.off()

#Semivariogram range violin
png("Figures/ViolinplotRangesm.png", res=200, width=6,height=4, units="in")

par(mar=c(2,3,.5,.5))
par(mgp=c(3,.3,0), tck=0)

p <- ggplot(b, aes(x=names, y=(model_range), fill=names, color=names)) + 
  scale_y_log10(limits=c(1,1000000), labels=comma) + 
  # scale_y_continuous(limits = c(-1, 5)) + 
  labs(x = "Variable", y='Semivariance range (m)') + 
  geom_violin(alpha=0.4, color='black', trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  geom_jitter(width=0.02, size=1, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=4, color='black', shape=18) +
  theme_minimal() + 
  theme(legend.position="none")
p 

dev.off()

#Semivariogram range violin
png("Figures/ViolinplotRangesm_withcutoff.png", res=200, width=6,height=4, units="in")

par(mar=c(2,3,.5,.5))
par(mgp=c(3,.3,0), tck=0)

p <- ggplot(b, aes(x=names, y=(range_best), fill=names, color=names)) + 
  scale_y_continuous(labels=comma, limits=c(0,3000)) + 
  # scale_y_continuous(limits = c(-1, 5)) + 
  labs(x = "Variable", y='Semivariance range (m)') + 
  geom_violin(alpha=0.4, color='black', trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  geom_jitter(width=0.02, size=1, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=4, color='black', shape=18) +
  theme_minimal() + 
  theme(legend.position="none")
p 

dev.off()


#Semivariogram range boxplots
png("Figures/GGBoxplotNotchesRangesm.png", res=200, width=6,height=4, units="in")

par(mar=c(2,3,.5,.5))
par(mgp=c(3,.3,0), tck=0)

p <- ggplot(b, aes(x=names, y=(model_range), fill=names, color=names)) + 
  scale_y_log10(labels=comma, limits=c(20,10000)) + 
  # coord_cartesian(ylim=c(0, 7)) + 
  labs(x = "Variable", y='Semivariance range (m)') + 
  # geom_violin(alpha=0.4, color='black', trim=F) + 
  geom_boxplot(width=0.5, color='black', notch=T, outlier.shape=NA) + 
  scale_fill_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=1, alpha=1) + 
  # stat_summary(fun.y=median, geom="point", size=4, color='black', shape=18) + 
  theme_bw() + 
  theme(legend.position="none", panel.grid.minor= element_blank())
p 

dev.off()



#Semivariogram range boxplots
png("Figures/GGBoxplotNotchesRangesm_withcutoff.png", res=200, width=6,height=4, units="in")

par(mar=c(2,3,.5,.5))
par(mgp=c(3,.3,0), tck=0)

p <- ggplot(b, aes(x=names, y=(range_best), fill=names, color=names)) + 
  scale_y_continuous(limits=c(0,2000)) + 
  # coord_cartesian(ylim=c(0, 7)) + 
  labs(x = "Variable", y='Semivariance range (m)') + 
  # geom_violin(alpha=0.4, color='black', trim=F) + 
  geom_boxplot(width=0.5, color='black', notch=T, outlier.shape=NA) + 
  scale_fill_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=1, alpha=1) + 
  # stat_summary(fun.y=median, geom="point", size=4, color='black', shape=18) + 
  theme_bw() + 
  theme(legend.position="none", panel.grid.minor= element_blank())
p 

dev.off()




png("Figures/ScatterRangesTempvsOthers.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

column<-3
for (column in 3:ncol(range_model)){
  if (column ==3){

    plot(range_model$TmpC_h, range_model[,column], col=colors[column], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
    abline(0,1, lty=3)
    mtext('Range temperature (m)', 1,2)
    mtext('Range other variabless (m)', 2,2)
    legend('topleft', inset=0.01, boxnames[2:length(boxnames)], text.col=colors[3:length(colors)], bty='n', x.intersp=0)
  } else {
    points(range_model$TmpC_h, range_model[,column], col=colors[column], pch=16, xlab='', ylab='')
  }
}

dev.off()



png("Figures/ScatterRangesSPCvsOthers.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

column<-3
for (column in 3:ncol(range_model)){
  if (column ==3){
    
    plot(range_model[,3], range_model$TmpC_h, col=colors[3], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
    abline(0,1, lty=3)
    mtext('Range SPC (m)', 1,2)
    mtext('Range other variabless (m)', 2,2)
    legend('topleft', boxnames[c(1,3:length(boxnames))], text.col=colors[3:length(colors)], bty='n', x.intersp=0)
  } else {
    points(range_model[,3], range_model[,column], col=colors[column], pch=16, xlab='', ylab='')
  }
}

dev.off()


png("Figures/ScatterRangesTempvsBGA.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

plot(range_model$TmpC_h, range_model$BGAPCRFU_h, col=varcolors[3], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
abline(0,1, lty=3)
mtext('Range temperature (m)', 1,2)
mtext('Range BGA (m)', 2,2)

dev.off()



png("Figures/ScatterRangesTempvschla.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

plot(range_model$TmpC_h, range_model$ChlARFU_h, col=varcolors[3], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
abline(0,1, lty=3)
mtext('Range temperature (m)', 1,2)
mtext('Range Chl a (m)', 2,2)

dev.off()


png("Figures/ScatterRangesTempvsTurb.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

plot(range_model$TmpC_h, range_model$TrbFNU_h, col=varcolors[1], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
abline(0,1, lty=3)
mtext('Range temperature (m)', 1,2)
mtext('Range Turbidity (m)', 2,2)

dev.off()





png("Figures/ScatterRangesTempvsOthers_withcutoff.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

column<-3
for (column in 3:ncol(range_best)){
  if (column ==3){
    
    plot(range_best$TmpC_h, range_best[,column], col=colors[column], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
    abline(0,1, lty=3)
    mtext('Range temperature (m)', 1,2)
    mtext('Range other variabless (m)', 2,2)
    legend('topleft', inset=0.01, boxnames[2:length(boxnames)], text.col=colors[3:length(colors)], bty='n', x.intersp=0)
  } else {
    points(range_best$TmpC_h, range_best[,column], col=colors[column], pch=16, xlab='', ylab='')
  }
}

dev.off()



png("Figures/ScatterRangesSPCvsOthers_withcutoff.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(range_model[,2:ncol(range_best)])

column<-3
for (column in 3:ncol(range_best)){
  if (column ==3){
    
    plot(range_best[,3], range_best$TmpC_h, col=colors[3], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
    abline(0,1, lty=3)
    mtext('Range SPC (m)', 1,2)
    mtext('Range other variabless (m)', 2,2)
    legend('topleft', boxnames[c(1,3:length(boxnames))], text.col=colors[3:length(colors)], bty='n', x.intersp=0)
  } else {
    points(range_best[,3], range_best[,column], col=colors[column], pch=16, xlab='', ylab='')
  }
}

dev.off()


png("Figures/ScatterRangesTempvsBGA_withcutoff.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(model_best[,2:ncol(range_best)])

plot(range_best$TmpC_h, range_best$BGAPCRFU_h, col=varcolors[3], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
abline(0,1, lty=3)
mtext('Range temperature (m)', 1,2)
mtext('Range BGA (m)', 2,2)

dev.off()



png("Figures/ScatterRangesTempvschla_withcutoff.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(model_best[,2:ncol(range_best)])

plot(range_best$TmpC_h, range_best$ChlARFU_h, col=varcolors[3], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
abline(0,1, lty=3)
mtext('Range temperature (m)', 1,2)
mtext('Range Chl a (m)', 2,2)

dev.off()


png("Figures/ScatterRangesTempvsTurb_withcutoff.png", res=200, width=6,height=6, units="in")
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('black', 'black', brewer.pal(3,"Blues")[c(2,3)], brewer.pal(3,"Reds"), brewer.pal(3,"Greens")[c(2,3)])
lim=c(0,2000)
# lim=range(model_best[,2:ncol(range_best)])

plot(range_best$TmpC_h, range_best$TrbFNU_h, col=varcolors[1], pch=16, xlab='', ylab='', xlim=lim, ylim=lim)
abline(0,1, lty=3)
mtext('Range temperature (m)', 1,2)
mtext('Range Turbidity (m)', 2,2)

dev.off()




points(semivar_alllakes$range_best[semivar_alllakes$variable==vars[1]], semivar_alllakes$range_best[semivar_alllakes$variable==vars[3]], col=colors[2], pch=16)

abline(0,1, lty=3)
