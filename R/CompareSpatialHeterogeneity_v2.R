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

#Still need to clean this part up.
#Subset or select which columns to use...
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_pixels<-paste(goodvars, 'pixels', sep='_')
goodvars_points<-paste(goodvars, 'points', sep='_')

CVstats <- c('CV', 'MADMOverMedian', 'QuartileDispersion', 'SDL', 'skewness', 'shape')

SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')

#Only use tau data values
tauvars<-c("TempC_t", "SPCScm_t", "fDOMRFU_t", "TrbFNU_t", "pH_tau", "ODOmgL_t",  "CO2uM_t", "CH4uM_t", "ChlARFU_t", "BGAPCRFU_t")

variabletype<-c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3)

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





# ########################################
# Compare Lagos Data to Spatial statistics
# ########################################

data_short2<-left_join(lagos_subset, merged_short, by = c("flame_name" = "Lake"))
data_CV <-data_short2 %>%
  dplyr::filter(Statistic =='CV')
data_QuartileDispersion <-data_short2 %>%
  dplyr::filter(Statistic =='QuartileDispersion')
data_skewness <-data_short2 %>%
  dplyr::filter(Statistic =='skewness')
data_minmax <-data_short2 %>%
  dplyr::filter(Statistic =='MaxMinusMin')


plot(data_CV$lake_area_ha, data_CV$TempC)
plot(data_CV$lake_area_ha, data_CV$SPCuScm)
plot(data_CV$lake_area_ha, data_CV$fDOMRFU)
plot(data_CV$lake_area_ha, data_CV$TurbFNU)
plot(data_CV$lake_area_ha, data_CV$pH)
plot(data_CV$lake_area_ha, data_CV$ODOmgL)
plot(data_CV$lake_area_ha, data_CV$ChlARFU)
plot(data_CV$lake_area_ha, data_CV$BGAPCRFU)

plot(data_skewness$lake_area_ha, data_skewness$TempC)
plot(data_skewness$lake_area_ha, data_skewness$SPCuScm)
plot(data_skewness$lake_area_ha, data_skewness$fDOMRFU)
plot(data_skewness$lake_area_ha, data_skewness$TurbFNU)
plot(data_skewness$lake_area_ha, data_skewness$pH)
plot(data_skewness$lake_area_ha, data_skewness$ODOmgL)
plot(data_skewness$lake_area_ha, data_skewness$ChlARFU)
plot(data_skewness$lake_area_ha, data_skewness$BGAPCRFU)

plot(data_skewness$ShorelineIndex, data_skewness$TempC)
plot(data_skewness$ShorelineIndex, data_skewness$SPCuScm)
plot(data_skewness$ShorelineIndex, data_skewness$fDOMRFU)
plot(data_skewness$ShorelineIndex, data_skewness$TurbFNU)
plot(data_skewness$ShorelineIndex, data_skewness$pH)
plot(data_skewness$ShorelineIndex, data_skewness$ODOmgL)
plot(data_skewness$ShorelineIndex, data_skewness$ChlARFU)
plot(data_skewness$ShorelineIndex, data_skewness$BGAPCRFU)

plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$TempC)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$SPCuScm)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$fDOMRFU)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$TurbFNU)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$pH)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$ODOmgL)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$ChlARFU)
plot(data_QuartileDispersion$lake_area_ha, data_QuartileDispersion$BGAPCRFU)


plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$TempC)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$SPCuScm)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$fDOMRFU)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$TurbFNU)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$pH)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$ODOmgL)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$ChlARFU)
plot(data_QuartileDispersion$ShorelineIndex, data_QuartileDispersion$BGAPCRFU)


plot(data_minmax$lake_area_ha, data_minmax$TempC)
plot(data_minmax$lake_area_ha, data_minmax$SPCuScm)
plot(data_minmax$lake_area_ha, data_minmax$fDOMRFU)
plot(data_minmax$lake_area_ha, data_minmax$TurbFNU)
plot(data_minmax$lake_area_ha, data_minmax$pH)
plot(data_minmax$lake_area_ha, data_minmax$ODOmgL)
plot(data_minmax$lake_area_ha, data_minmax$ChlARFU)
plot(data_minmax$lake_area_ha, data_minmax$BGAPCRFU)



# ################################
# Make wide and long tables
# ################################

data_long<-gather(data_short, key='stat', value='value', 1:17) 

data_wide<- data_short %>% 
  dplyr::select(-c(VariableFactor)) %>%
  gather(key=stat, value=value, 1:17) %>%
  unite(temp, Variable, stat) %>%
  spread(temp, value)

plot(data_wide$TempC_CV, data_wide$ChlARFU_CV)
abline(0,1)

plot(data_wide$SPCuScm_CV, data_wide$ChlARFU_CV)
abline(0,1)

plot(data_wide$TurbFNU_CV, data_wide$ChlARFU_CV)
abline(0,1)


