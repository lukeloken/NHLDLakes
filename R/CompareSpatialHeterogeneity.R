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

#Get Lagos data
mylagos2<-readRDS(file='Data/MyLakesLagos.rds')
lagosvars<-c("flame_name"  , "lake_area_ha", "lake_perim_meters", "meandepth", "maxdepth", "iws_ha", "iws_perimkm", "iws_lakeareaha", "ShorelineIndex")
lagos_subset <- mylagos2 %>%
  dplyr::select(lagosvars)

# Spatial means, ranges
merged_points<-readRDS(file='Data/FlamePointsSummaries.rds')
#Only use raw data values
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
#Only use tau data values
tauvars<-c("TempC_t", "SPCScm_t", "fDOMRFU_t", "TrbFNU_t", "pH_tau", "ODOmgL_t",  "CO2uM_t", "CH4uM_t", "ChlARFU_t", "BGAPCRFU_t")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")
variabletype<-c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3)

merged_short<-merged_points[c('Lake', 'Statistic', 'Date', goodvars)]

# Set working directory to root data directory
setwd("E:/Dropbox/FLAME_NHLDLakes/")
filenames <- list.files(path = paste(getwd(), "/Pointsummaries", sep=""))

# Compile point summaries
data_combine<-data.frame(matrix(nrow=0, ncol=19))
file<-filenames[1]
for (file in filenames){
    data1<-read.csv(paste0("Pointsummaries/", file), header=T, stringsAsFactors = F)
    data1$file<-file
    if (nrow(data_combine)==0){
      data_combine<-data1}
    else {
      data_combine<-smartbind(data_combine, data1, fill=NA)}
  }

#Put lake and date in dataframe
LakeDate<-gsub("cleaned.csv","", data_combine$file, perl=T)
data_combine$Date<-gsub("[A-z]", "", LakeDate)
data_combine$Lake<-unlist(lapply(strsplit(LakeDate, '20'), `[[`, 1))



data_short<-data_combine[data_combine$Variable %in% goodvars,]
data_short$VariableShort = shortnames[match(data_short$Variable, goodvars)]
data_short$VariableShort = factor(data_short$VariableShort, shortnames)

data_short$VariableFactor = factor(data_short$Variable,goodvars)

data_tau<-data_combine[data_combine$Variable %in% tauvars,]
data_tau$VariableFactor = factor(data_tau$Variable,tauvars)


# ##############
# plotting
# ##############

# Boxplots of spatial variability across variables
png("Figures/spatialheterogeneityvariables.png", res=200, width=4.5,height=10, units="in")
par(mfrow=c(5,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

boxwex=0.6
colors<-c('blue', 'red', 'green')

#boxplots of CV
boxplot(data_short$CV ~ data_short$VariableFactor, ylim=c(0,1), ylab='', names=NA , boxwex=boxwex, col=colors[variabletype])
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('CV', 2, 2)

#boxplots of MADM over median
boxplot(data_short$MADMOverMedian ~ data_short$VariableFactor, ylim=c(0,.8), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('MADM over median', 2, 2)

#boxplots of QuartileDispersion
boxplot(data_short$QuartileDispersion ~ data_short$VariableFactor, ylim=c(0,.8), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('QuartileDispersion', 2, 2)

#boxplots of skewness
boxplot((data_short$skewness) ~ data_short$VariableFactor, ylim=c(-5,5), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
abline(h=0, lty=3)
boxplot((data_short$skewness) ~ data_short$VariableFactor, ylim=c(-5,5), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype], add=T )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('Skewness', 2, 2)

#boxplots of shape
boxplot(data_short$shape ~ data_short$VariableFactor, ylab='', names=NA, boxwex=boxwex, col=colors[variabletype], pch=NA, ylim=c(-1.5,1.5) )
abline(h=0, lty=3)
boxplot(data_short$shape ~ data_short$VariableFactor, ylab='', names=NA, boxwex=boxwex, col=colors[variabletype], pch=NA, ylim=c(-1.5,1.5), add=T)
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('Variable', 1, 2.5)
mtext('EVD-shape', 2, 2)

dev.off()


# Boxplots of spatial variability across variables tau
png("Figures/spatialheterogeneitytauvariables.png", res=200, width=4.5,height=10, units="in")
par(mfrow=c(5,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

boxwex=0.6
colors<-c('blue', 'red', 'green')

#boxplots of CV
boxplot(data_tau$CV ~ data_tau$VariableFactor, ylim=c(0,1), ylab='', names=NA , boxwex=boxwex, col=colors[variabletype])
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('CV', 2, 2)

#boxplots of MADM over median
boxplot(data_tau$MADMOverMedian ~ data_tau$VariableFactor, ylim=c(0,.8), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('MADM over median', 2, 2)

#boxplots of QuartileDispersion
boxplot(data_tau$QuartileDispersion ~ data_tau$VariableFactor, ylim=c(0,.8), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('QuartileDispersion', 2, 2)

#boxplots of skewness
boxplot((data_tau$skewness) ~ data_tau$VariableFactor, ylim=c(-5,5), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
abline(h=0, lty=3)
boxplot((data_tau$skewness) ~ data_tau$VariableFactor, ylim=c(-5,5), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype], add=T)
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('Skewness', 2, 2)

#boxplots of shape
boxplot(data_tau$shape ~ data_tau$VariableFactor, ylab='', names=NA, boxwex=boxwex, col=colors[variabletype], pch=NA, ylim=c(-1.5,1.5) )
abline(h=0, lty=3)
boxplot(data_tau$shape ~ data_tau$VariableFactor, ylab='', names=NA, boxwex=boxwex, col=colors[variabletype], pch=NA, ylim=c(-1.5,1.5), add=T)
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('Variable', 1, 2.5)
mtext('EVD-shape', 2, 2)

dev.off()



# Violin plots of spatial variability across variables
png("Figures/spatialheterogeneityViolins.png", res=200, width=6,height=10, units="in")
par(mfrow=c(5,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

boxwex=0.6
varcolors<-c('blue', 'red', 'green')


p1 <- ggplot(data_short, aes(x=VariableShort, y=CV, fill=VariableShort, color=VariableShort)) + 
  scale_y_continuous(limits = c(0, 1.5)) + 
  labs(x = "", y='CV') + 
  geom_violin(alpha=0.2, trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

p2 <- ggplot(data_short, aes(x=VariableShort, y=MADMOverMedian, fill=VariableShort, color=VariableShort)) + 
  scale_y_continuous(limits = c(0,.8)) + 
  labs(x = "", y='MADMOverMedian') + 
  geom_violin(alpha=0.2,  trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

p3 <- ggplot(data_short, aes(x=VariableShort, y=QuartileDispersion, fill=VariableShort, color=VariableShort)) + 
  scale_y_continuous(limits = c(0,.8)) + 
  labs(x = "", y='QuartileDispersion') + 
  geom_violin(alpha=0.2,  trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

p4 <- ggplot(data_short, aes(x=VariableShort, y=skewness, fill=VariableShort, color=VariableShort)) + 
  scale_y_continuous(limits = c(-5,5)) + 
  labs(x = "", y='skewness') + 
  geom_violin(alpha=0.2, trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

p5 <- ggplot(data_short, aes(x=VariableShort, y=shape, fill=VariableShort, color=VariableShort)) + 
  scale_y_continuous(limits = c(-1.5,1.5)) + 
  labs(x = "Variable", y='EVD-shape') + 
  geom_violin(alpha=0.2,  trim=F) + 
  # geom_boxplot(width=0.5, color='black', notch=T) + 
  scale_fill_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  scale_color_manual(values=varcolors[c(1,1,1,1,2,2,2,2,3,3)]) + 
  # geom_jitter(width=0.02, size=0.5, alpha=1) +
  stat_summary(fun.y=median, geom="point", size=3, color='black', shape=18) +
  theme_bw() + 
  theme(legend.position="none")

grid.arrange(p1, p2, p3, p4,p5, ncol=1)

dev.off()




# ########################################
# Calculate ranges (max - min) for each variable/day and plot histograms
# ########################################

setwd("E:/Git_Repo/NHLDLakes")

merged_ranges<- merged_short %>%
  filter(Statistic =='range')

merged_IQR<- merged_short %>%
  filter(Statistic =='IQR')


units<-c()
units[1]<-c(expression(paste('Temp (', degree, 'C)')))
units[2]<-c(expression(paste('SPC (', mu, 'S cm'^'-1', ')')))
units[3]<-c(expression(paste('fDOM (RFU)')))
units[4]<-c(expression(paste('Turb (FNU)')))
units[5]<-c(expression(paste('pH')))
units[6]<-c(expression(paste('DO (mg L'^'-1', ')')))
units[7]<-c(expression(paste(CO[2], ' (', mu, 'M)')))
units[8]<-c(expression(paste(CH[4], ' (', mu, 'M)')))
units[9]<-c(expression(paste('Chl a (RFU)')))
units[10]<-c(expression(paste('BGA (RFU)')))

                      
# Boxplots of spatial variability across variables
png("Figures/HisotgramsofMaxminusMin.png", res=200, width=4.5,height=10, units="in")
par(mfrow=c(5,2))
par(mar=c(3,2,.5,.5), oma=c(.5,2,2,0))
par(mgp=c(2, .5, 0))
  
for (column in 4:13){
  hist(merged_ranges[,column], main='', xlab='', ylab='', col='lightgrey', breaks=10)
  # mtext(shortnames[column-3], 1, 2)
  mtext(units[column-3], 1, 2)
  box(which='plot')
}
mtext('Number of lakes', 2, .5, outer=T)
mtext('Within-lake ranges (Max - Min)', 3, 0, outer=T)

dev.off()
  


# Boxplots of spatial variability across variables
png("Figures/HisotgramsofIQR.png", res=200, width=4.5,height=10, units="in")
par(mfrow=c(5,2))
par(mar=c(3,2,.5,.5), oma=c(.5,2,2,0))
par(mgp=c(2, .5, 0))

for (column in 4:13){
  hist(merged_IQR[,column], main='', xlab='', ylab='', col='lightgrey', breaks=10)
  # mtext(shortnames[column-3], 1, 2)
  mtext(units[column-3], 1, 2)
  box(which='plot')
}
mtext('Number of lakes', 2, .5, outer=T)
mtext('Within-lake IQR', 3, 0, outer=T)

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


