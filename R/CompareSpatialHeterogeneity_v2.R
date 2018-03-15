# ===================================
# Compare spatial summaries across systems
# Load all NHLD pointsummaries data
# Luke Loken, Feb 2018
# ===================================

rm(list = ls())

library(gtools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

setwd("E:/Git_Repo/NHLDLakes")

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

#Subset or select which columns to use...
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_pixels<-paste(goodvars, 'pixels', sep='_')
goodvars_points<-paste(goodvars, 'points', sep='_')

CVstats <- c('CV', 'MADMOverMedian', 'QuartileDispersion', 'SDL', 'skewness', 'shape')

SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')


# ##############
# plotting
# ##############

#Set universal colors box widths, others
boxwex=0.6
colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]


# Boxplots of spatial variability across variables using pixels
png("Figures/CV_andothers_BoxplotsAmongVariablesPixels.png", res=200, width=4.5,height=12, units="in")
par(mfrow=c(length(CVstats) ,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,2,0))
par(mgp=c(2, .5, 0))


ymin<-c(rep(0,4),-5,-1.5)
ymax<-c(0.5,0.2,0.2,0.2,5,1.5)

nu <- 1
for (nu in 1:length(CVstats)){
boxplot(j[paste(goodvars_pixels, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar)
axis(1, labels=shortnames, at=1:length(shortnames))
mtext(CVstats[nu], 2, 2)
abline(h=0, lty=3)
boxplot(j[paste(goodvars_pixels, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar, add=T)
}

mtext('Spatial stats across lakes and variable types (pixels)', 3, 0, outer=T)
mtext('Variable', 1, .5, outer=T)

dev.off()



# Boxplots of spatial variability across variables using points
png("Figures/CV_andothers_BoxplotsAmongVariablesPoints.png", res=200, width=4.5,height=12, units="in")
par(mfrow=c(length(CVstats),1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,2,0))
par(mgp=c(2, .5, 0))

ymin<-c(rep(0,4),-5,-1.6)
ymax<-c(1.1,0.75,0.75,0.45,5,1.6)

nu <- 1
for (nu in 1:length(CVstats)){
  boxplot(j[paste(goodvars_points, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar)
  axis(1, labels=shortnames, at=1:length(shortnames))
  mtext(CVstats[nu], 2, 2)
  abline(h=0, lty=3)
  boxplot(j[paste(goodvars_points, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar, add=T)
}

mtext('Spatial stats across lakes and variable types (points)', 3, 0, outer=T)
mtext('Variable', 1, .5, outer=T)

dev.off()



# Boxplots of semivariance ranges across variables using points
png("Figures/SemiVarRanges_BoxplotsAmongVariablesPoints.png", res=200, width=5,height=3, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

ylim<-c(0,2000)

  boxplot(j[SemiRange_columns], ylim=ylim, ylab='', names=NA , boxwex=boxwex, col=colorbyvar, cex=0.5, pch=16, yaxt='n')
  axis(2, at=seq(0,2000,500), cex.axis=.7)
  axis(1, labels=shortnames, at=1:length(shortnames), cex.axis=.7)
  mtext('Semivariance range (m)', 2, 1.5)
  abline(h=0, lty=3)

mtext('Variable', 1, 0, outer=T)

dev.off()

# Boxplots of semivariance ranges across variables using points
png("Figures/SemiVarRanges_BoxplotsAmongVariablesPointsNoLGR.png", res=200, width=4,height=2.5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

ylim<-c(0,2000)

boxplot(j[SemiRange_columns[-c(7:8)]], ylim=ylim, ylab='', names=NA , boxwex=boxwex, col=colorbyvar[-c(7:8)], cex=0.5, pch=16, yaxt='n')
axis(2, at=seq(0,2000,500), cex.axis=.7)
axis(1, labels=shortnames[-c(7:8)], at=1:length(shortnames[-c(7:8)]), cex.axis=.7)
mtext('Semivariance range (m)', 2, 1.5)
abline(h=0, lty=3)

mtext('Variable', 1, 0, outer=T)

dev.off()


