# ===================================
# Compare spatial summaries across systems
# Load all NHLD pointsummaries data
# Luke Loken, Feb 2018
# ===================================

rm(list = ls())

library(gtools)
library(tidyr)
library(dplyr)

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

#Only use raw data values
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")
variabletype<-c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3)

data_short<-data_combine[data_combine$Variable %in% goodvars,]
data_short$VariableFactor = factor(data_short$Variable,goodvars)

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
boxplot(data_short$CV ~ data_short$VariableFactor, ylim=c(-1,1), ylab='', names=NA , boxwex=boxwex, col=colors[variabletype])
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('CV', 2, 2)

#boxplots of MADM over median
boxplot(data_short$MADMOverMedian ~ data_short$VariableFactor, ylim=c(-1,1), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('MADM over median', 2, 2)

#boxplots of QuartileDispersion
boxplot(data_short$QuartileDispersion ~ data_short$VariableFactor, ylim=c(-1,1), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('QuartileDispersion', 2, 2)

#boxplots of skewness
boxplot(abs(data_short$skewness) ~ data_short$VariableFactor, ylim=c(0,5), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('abs(skewness)', 2, 2)

#boxplots of shape
boxplot(abs(data_short$shape) ~ data_short$VariableFactor, ylim=c(0,2), ylab='', names=NA, boxwex=boxwex, col=colors[variabletype] )
axis(1, labels=shortnames, at=1:length(goodvars))
mtext('Variable', 1, 2.5)
mtext('abs(shape)', 2, 2)

dev.off()


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


