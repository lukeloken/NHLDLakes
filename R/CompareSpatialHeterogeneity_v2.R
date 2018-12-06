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

asinTransform <- function(p) { asin(sqrt(p)) }

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

#Subset or select which columns to use...
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "pH","TurbFNU",  "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
# goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "pH","TurbFNU",  "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU", "ChlAugL", "BGAPCgL")
shortnames<-c("Temp", "SPC", "fDOM", "pH", "Turb", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_pixels<-paste(goodvars, 'pixels', sep='_')
goodvars_points<-paste(goodvars, 'points', sep='_')

CVstats <- c('CV', 'MADMOverMedian', 'QuartileDispersion', 'SDL', 'skewness', 'shape')
CVstats_short<-c('CV','skewness')

Tablestats<-c('MaxMinusMin', 'sd', 'mad', 'CV', 'MADMOverMedian', 'QuartileDispersion', 'SDL', 'skewness', 'shape')

SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'pH_h', 'TrbFNU_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')
SemiRangeRatio_columns<-paste(SemiVars, 'points', 'SemiRangeOverCutoff', sep='_')
SemiSillPercent_columns<-paste(SemiVars, 'points', 'SillPercent', sep='_')
SemiTotalSill_columns<-paste(SemiVars, 'points', 'SillTotal', sep='_')
SemiPSill_columns<-paste(SemiVars, 'points', 'PSill', sep='_')


Table_columns<-c(paste(goodvars_points, Tablestats[1], sep="_"),
                 paste(goodvars_points, Tablestats[2], sep="_"),
                 paste(goodvars_points, Tablestats[3], sep="_"),
                 paste(goodvars_points, Tablestats[4], sep="_"),
                 paste(goodvars_points, Tablestats[5], sep="_"),
                 paste(goodvars_points, Tablestats[6], sep="_"),
                 paste(goodvars_points, Tablestats[7], sep="_"),
                 paste(goodvars_points, Tablestats[8], sep="_"),
                 paste(goodvars_points, Tablestats[9], sep="_"),
                 SemiRange_columns,
                 SemiRangeRatio_columns,
                 SemiTotalSill_columns,
                 SemiPSill_columns,
                 SemiSillPercent_columns)

Stats<-j[Table_columns]

StatsMeans<- Stats %>% 
  summarize_all(mean, na.rm=T) %>%
  as.numeric() %>%
  signif(digits=2) %>%
  matrix(nrow=14, ncol=10, byrow=T)

StatsMins<- Stats %>% 
  summarize_all(min, na.rm=T)%>%
  as.numeric() %>%
  signif(digits=2) %>%
  matrix(nrow=14, ncol=10, byrow=T)

StatsMaxs<- Stats %>% 
  summarize_all(max, na.rm=T) %>%
  as.numeric() %>%
  signif(digits=2) %>%
  matrix(nrow=14, ncol=10, byrow=T)

StatsMatrix<-matrix(paste(StatsMins, ' to ', StatsMaxs, sep=''), nrow=14, ncol=10)

table_SH<-as.data.frame(matrix(nrow=28, ncol=10))
table_SH[seq(1,27,2),]<-StatsMeans
table_SH[seq(2,28,2),]<-StatsMatrix

names(table_SH)<-c(shortnames)

table_SH$Stat<-''

table_SH$Stat[seq(1,27,2)]<-c('Range (Max - Min)', 'SD', 'MAD', 'CV', 'MAD/Median', 'QuartileDispersion', 'SDL', 'Skewness', 'EVD-shape', 'SemiRange', 'SemiRangeRatio', 'Total Sill', 'P-sill', 'Sill %')

table_SH_out<-table_SH[,c(11,1:10)]



write.table(table_SH_out, file='Data/AcrossLake_SpatialHeterogeneity_Summary.csv', sep=',', row.names=F)
# ##############
# plotting
# ##############

#Set universal colors box widths, others
boxwex=0.6
colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,1,2,2,2,2,2)]


# Boxplots of spatial variability across variables using pixels
png("Figures/Boxplots/CV_andothers_BoxplotsAmongVariablesPixels.png", res=200, width=4.5,height=12, units="in")
par(mfrow=c(length(CVstats) ,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,2,0))
par(mgp=c(2, .5, 0))


ymin<-c(rep(0,4),-5,-1.5)
ymax<-c(0.5,0.2,0.2,0.2,5,1.5)

nu <- 1
for (nu in 1:length(CVstats)){
boxplot(j[paste(goodvars_pixels, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar)
axis(1, labels=shortnames, at=1:length(shortnames))
if (CVstats[nu]=='MADMOverMedian'){
  mtext('MAD/Median', 2, 2)
} else {
  mtext(CVstats[nu], 2, 2)}
abline(h=0, lty=3)
boxplot(j[paste(goodvars_pixels, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar, add=T)
}

mtext('Spatial stats across lakes and variable types (pixels)', 3, 0, outer=T)
mtext('Variable', 1, .5, outer=T)

dev.off()



# Boxplots of spatial variability across variables using points
png("Figures/Boxplots/CV_andothers_BoxplotsAmongVariablesPoints.png", res=200, width=4.5,height=12, units="in")
par(mfrow=c(length(CVstats),1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,2,0))
par(mgp=c(2, .5, 0))

ymin<-c(rep(0,4),-5,-1.6)
ymax<-c(1.1,0.75,0.75,0.45,5,1.6)

nu <- 1
for (nu in 1:length(CVstats)){
  boxplot(j[paste(goodvars_points, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar)
  axis(1, labels=shortnames, at=1:length(shortnames))
  if (CVstats[nu]=='MADMOverMedian'){
    mtext('MAD/Median', 2, 2)
  } else {
    mtext(CVstats[nu], 2, 2)}
  abline(h=0, lty=3)
  boxplot(j[paste(goodvars_points, CVstats[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar, add=T)
}

mtext('Spatial stats across lakes and variable types (points)', 3, 0, outer=T)
mtext('Variable', 1, .5, outer=T)

dev.off()



# Boxplots of spatial variability across variables using points just CV and skewness
png("Figures/Boxplots/CV_skewness_BoxplotsAmongVariablesPoints.png", res=200, width=4.5,height=4, units="in")
par(mfrow=c(length(CVstats_short),1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0), tck=-0.04)

ymin<-c(0,-5)
ymax<-c(1.1,5)

nu <- 1
for (nu in 1:length(CVstats_short)){
  boxplot(j[paste(goodvars_points, CVstats_short[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar, cex.axis=0.8, cex=0.6, las=1)
  if (nu==2){abline(h=c(-2,2), lty=3)}
  axis(1, labels=shortnames, at=1:length(shortnames), cex=0.6,  cex.axis=0.6)
  mtext(CVstats_short[nu], 2, 2)
  abline(h=0, lty=3)
  boxplot(j[paste(goodvars_points, CVstats_short[nu], sep="_")], ylim=c(ymin[nu], ymax[nu]) , ylab='', names=NA , boxwex=boxwex, col=colorbyvar, add=T, cex.axis=0.8, cex=.6, las=1)
}

# mtext('Spatial stats across lakes and variable types (points)', 3, 0, outer=T)
mtext('Variable', 1, .5, outer=T)

dev.off()





# Boxplots of semivariance ranges across variables using points
png("Figures/Boxplots/SemiVarRanges_BoxplotsAmongVariablesPoints.png", res=200, width=5,height=3, units="in")
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
png("Figures/Boxplots/SemiVarRanges_BoxplotsAmongVariablesPointsNoLGR.png", res=200, width=4,height=2.5, units="in")
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




# Boxplots of semivariance range ratios across variables using points
png("Figures/Boxplots/SemiVarRangeRatios_BoxplotsAmongVariablesPoints.png", res=200, width=5,height=3, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

ylim<-c(0,1)

boxplot(j[SemiRangeRatio_columns], ylim=ylim, ylab='', names=NA , boxwex=boxwex, col=colorbyvar, cex=0.5, pch=16, yaxt='n')
axis(2, at=seq(0,1,0.5), cex.axis=.7, las=1)
axis(1, labels=shortnames, at=1:length(shortnames), cex.axis=.7)
mtext('Semivariance range ratio', 2, 1.5)
abline(h=c(0,1), lty=3)

mtext('Variable', 1, 0, outer=T)

dev.off()

# Boxplots of semivariance range ratios across variables using points
png("Figures/Boxplots/SemiVarRangeRatios_BoxplotsAmongVariablesPointsNoLGR.png", res=200, width=4,height=2.5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

ylim<-c(0,1)

boxplot(j[SemiRangeRatio_columns[-c(7:8)]], ylim=ylim, ylab='', names=NA , boxwex=boxwex, col=colorbyvar[-c(7:8)], cex=0.5, pch=16, yaxt='n')
axis(2, at=seq(0,1,.5), cex.axis=.7, las=1)
axis(1, labels=shortnames[-c(7:8)], at=1:length(shortnames[-c(7:8)]), cex.axis=.7)
mtext('Semivariance range ratios', 2, 1.5)
abline(h=c(0,1), lty=3)

mtext('Variable', 1, 0, outer=T)

dev.off()


#With arcsin sqrt transform

# Boxplots of semivariance range ratios across variables using points
png("Figures/Boxplots/SemiVarRangeRatiosArcSinTran_BoxplotsAmongVariablesPoints.png", res=200, width=5,height=3, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

ylim<-c(0,1)

boxplot(asinTransform(j[SemiRangeRatio_columns]), ylab='', names=NA , boxwex=boxwex, col=colorbyvar, cex=0.5, pch=16, yaxt='n')
axis(2, cex.axis=.7, las=1)
axis(1, labels=shortnames, at=1:length(shortnames), cex.axis=.7)
mtext('Semivariance range ratio', 2, 1.5)
abline(h=c(0,pi/2), lty=3)

mtext('Variable', 1, 0, outer=T)

dev.off()


# Boxplots of semivariance range ratios arcsin transform across variables using points
png("Figures/Boxplots/SemiVarRangeRatiosArcSinTran_BoxplotsAmongVariablesPointsNoLGR.png", res=200, width=4,height=2.5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

ylim<-c(0,1)

boxplot(asinTransform(j[SemiRangeRatio_columns[-c(7:8)]]), ylab='', names=NA , boxwex=boxwex, col=colorbyvar[-c(7:8)], cex=0.5, pch=16, yaxt='n')
axis(2, cex.axis=.7, las=1)
axis(1, labels=shortnames[-c(7:8)], at=1:length(shortnames[-c(7:8)]), cex.axis=.7)
mtext('Arcsin(sqrt(Semivariance range ratios))', 2, 1.5)
abline(h=c(0,pi/2), lty=3)

mtext('Variable', 1, 0, outer=T)

dev.off()


# Convert data for violin plots
vdata <- j %>% 
  dplyr::select(SemiRangeRatio_columns) %>%
  gather(key=variable, value=rangeratio)

vdata$VariableShort <- shortnames[match(vdata$variable, SemiRangeRatio_columns)]
vdata$VariableShort = factor(vdata$VariableShort, shortnames)

rdata <- j %>% 
  dplyr::select(SemiRange_columns) %>%
  gather(key=variable, value=semivarrange)

rdata$VariableShort <- shortnames[match(rdata$variable, SemiRange_columns)]
rdata$VariableShort = factor(rdata$VariableShort, shortnames)


# Violin plot semivariance range ratios variables using points
png("Figures/Boxplots/SemiVarRangeRatios_ViolinplotsAmongVariablesPoints.png", res=200, width=4,height=2.5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

p1 <- ggplot(vdata, aes(x=VariableShort, y=rangeratio, fill=VariableShort, color=VariableShort)) +  
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = "", y='Semivariance range ratio') + 
  geom_violin(alpha=0.15, na.rm=T) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=colorbyvar) +
  scale_color_manual(values=colorbyvar) +
  geom_jitter(width=0.1, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

print(p1)

dev.off()

# Violin plot semivariance range ratios variables using points no LGR
png("Figures/Boxplots/SemiVarRangeRatios_ViolinplotsAmongVariablesPointsNoLGR.png", res=200, width=4,height=2.5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

p1 <- ggplot(vdata[-which(vdata$VariableShort %in% c('CO2', 'CH4')),], aes(x=VariableShort, y=rangeratio, fill=VariableShort, color=VariableShort)) +  
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = "", y='Semivariance range ratio') + 
  geom_violin(alpha=0.15, na.rm=T) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=colorbyvar[-c(7:8)]) +
  scale_color_manual(values=colorbyvar[-c(7:8)]) +
  geom_jitter(width=0.1, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

print(p1)

dev.off()

# Violin plot semivariance range ratios variables using points no LGR
png("Figures/Boxplots/SemiVarRange_ViolinplotsAmongVariablesPointsNoLGR.png", res=200, width=4,height=2.5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,2,.5,.5), oma=c(1,1,0,0))
par(mgp=c(2, .3, 0), tck=-0.02)

p1 <- ggplot(rdata[-which(rdata$VariableShort %in% c('CO2', 'CH4')),], aes(x=VariableShort, y=semivarrange, fill=VariableShort, color=VariableShort)) +  
  scale_y_continuous(limits=c(0,2000)) + 
  labs(x = "", y='Semivariance range (m)') + 
  geom_violin(alpha=0.15, na.rm=T, trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=colorbyvar[-c(7:8)]) +
  scale_color_manual(values=colorbyvar[-c(7:8)]) +
  geom_jitter(width=0.1, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

print(p1)

dev.off()