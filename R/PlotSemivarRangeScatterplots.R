
rm(list = ls())

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(PerformanceAnalytics)
library(car)

# ########################################
# Plot scatterplots of semivariance ranges for each variable/day
# ########################################
setwd("E:/Git_Repo/NHLDLakes")

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_points<-paste(goodvars, 'points', sep='_')

SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')

df_semi<-j[SemiRange_columns]
names(df_semi)<-shortnames

# Plot correlation matrix of all semivariance ranges
png("Figures/Scatterplots/SemivarRangesAll.png", res=200, width=12,height=12, units="in")
par(mfrow=c(1,1))
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)

lim=c(0,2000)

# plot(df_semi, cex=0.8, pch=16)
scatterplotMatrix(df_semi, diagonal="histogram", smooth=FALSE, xlim=c(0,2500), ylim=c(0,2500), pch=16, col=c('darkgrey', 'black', 'black'))

# chart.Correlation(df_semi, histogram=T, pch=16)
mtext('Semivarance range (m)',1,1.5)
mtext('Semivarance range (m)',2,1.5)

dev.off()

# Plot scatterplots of temperature range vs all other variables ranges
png("Figures/Scatterplots/SemivarRangesTempvsAll.png", res=200, width=5,height=5, units="in")
par(mfrow=c(1,1))
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)
lim=c(0,2100)

column<-1
for (column in 1:ncol(df_semi)){
  if (column ==1){

    plot(df_semi[,1], df_semi[,column], type='n', xlab='', ylab='', xlim=lim, ylim=lim)
    abline(0,1, lty=3)
    mtext('Semivariance range temperature (m)', 1,2)
    mtext('Semivariance range other variables (m)', 2,2)
    legend('topleft', inset=0.01, shortnames[2:length(shortnames)], text.col=colorbyvar[2:length(colorbyvar)], bty='n', x.intersp=0)
  } else {
    points(df_semi[,1], df_semi[,column], col=colorbyvar[column], pch=16, cex=0.8)
  }
}
dev.off()

# Plot scatterplots of SPC range vs all other variables ranges
png("Figures/Scatterplots/SemivarRangesSPCvsAll.png", res=200, width=5,height=5, units="in")
par(mfrow=c(1,1))
par(mar=c(3,3,.5,.5))
par(mgp=c(3,.5,0), tck=-0.02)
lim=c(0,2100)

column<-1
for (column in c(2,1,3:10)){
  if (column ==2){
    
    plot(df_semi[,2], df_semi[,column], type='n', xlab='', ylab='', xlim=lim, ylim=lim)
    abline(0,1, lty=3)
    mtext('Semivariance range SPC (m)', 1,2)
    mtext('Semivariance range other variables (m)', 2,2)
    legend('topleft', inset=0.01, shortnames[c(1,3:length(shortnames))], text.col=colorbyvar[c(1,3:length(colorbyvar))], bty='n', x.intersp=0)
  } else {
    points(df_semi[,1], df_semi[,column], col=colorbyvar[column], pch=16, xlab='', ylab='', cex=0.8)
  }
}
dev.off()

