


# ########################################
# Plot histograms of (max - min) for each variable/day and plot histograms
# ########################################
setwd("E:/Git_Repo/NHLDLakes")

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

# From previous script, can delete later
#Subset or select which columns to use...
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")

goodvars_pixels<-paste(goodvars, 'pixels', sep='_')
goodvars_points<-paste(goodvars, 'points', sep='_')

SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')

# ##### End code to delete later
IQRstats <- c('MaxMinusMin', 'Q95MinusQ05', 'IQR')

# Make a vector of axis labels with units
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


# Loop through stats and variables and make histograms (pixels)
nu <- 1
for (nu in 1:length(IQRstats)){
  
  png(paste0("Figures/Hisotgramsof", IQRstats[nu], "Pixels.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(3,2,.5,.5), oma=c(.5,2,2,0))
  par(mgp=c(2, .5, 0))
  
  var <- 1
  for (var in 1:length(goodvars_pixels)){
    values<-j[paste(goodvars_pixels[var], IQRstats[nu], sep="_")]
    hist(values[,1], main='', xlab='', ylab='', col='lightgrey', breaks=10)
    
    mtext(units[var], 1, 2)
    box(which='plot')
    
  }
  mtext('Number of lakes', 2, .5, outer=T)
  mtext(paste0('Within-lake ', IQRstats[nu], ' (pixels)'), 3, 0, outer=T)
  
  dev.off()
}

# Loop through stats and variables and make histograms (points)
nu <- 1
for (nu in 1:length(IQRstats)){
  
  png(paste0("Figures/Hisotgramsof", IQRstats[nu], "Points.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(3,2,.5,.5), oma=c(.5,2,2,0))
  par(mgp=c(2, .5, 0))
  
  var <- 1
  for (var in 1:length(goodvars_points)){
    values<-j[paste(goodvars_points[var], IQRstats[nu], sep="_")]
    hist(values[,1], main='', xlab='', ylab='', col='lightgrey', breaks=10)
    
    mtext(units[var], 1, 2)
    box(which='plot')
    
  }
  mtext('Number of lakes', 2, .5, outer=T)
  mtext(paste0('Within-lake ', IQRstats[nu], ' (points)'), 3, 0, outer=T)
  
  dev.off()
}


