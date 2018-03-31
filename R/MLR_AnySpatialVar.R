
# Script to loop through standard deviations within lakes
# Pull out lake scale predictor variables. 
# Currently using bestglm and random forest
# Outputs a bunch of figures (scatterplots, model outputs)
# Still needs to be ruggedized to convert to CV and other metrics

# Not sure what all needs to be loaded
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(scales)
library(PerformanceAnalytics)
library(leaps)
library(MASS)
library(gridExtra)
library(bestglm)
library(randomForest)
library(viridis)
library(randomForestSRC)
library(ggRandomForests)
library(grid)

source('R/GLMcustom.R')

#Wide table
j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

#Color scheme to be used for all plotting
colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

#Spatial heterogeneiety stat to use
# VarStat <-'mad'
VarStat <-'sd'
# VarStat <-'SemiRange'
# VarStat <-'SemiRangeOverCutoff'


#Vectors of names
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

mediannames<-paste0(goodvars, 'Median')
goodvars_points<-paste(goodvars, 'points', sep='_')

sd_columns<-paste(goodvars, 'points', VarStat, sep='_')
# mean_columns<-paste(goodvars, 'points', 'Mean', sep='_')

sd_columns_pix<-paste(goodvars, 'pixels', VarStat, sep='_')
med_columns_pix<-paste(goodvars, 'pixels', 'Median', sep='_')

#Semivariance ranges use different var names
SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')
SemiRangeOverCutoff_columns<-paste(SemiVars, 'points', 'SemiRangeOverCutoff', sep='_')


# Which predictor variables to use from flame medians
# Omit fDOM and pH as these have multicollinearity issues
# Omit methane and CO2 since not available for all lakes
flamepredvars<-med_columns_pix[c(1,2,4,6,9,10)]

#only use lakes with lake area
k_full<-j

#Change lake connection to numeric (1 = higher landscape position)
connect_df<-data.frame(lakeconnection=c('Isolated', 'Headwater', 'DR_Stream', 'DR_LakeStream'), value=1:4)
k_full$lakeconn <- as.numeric(as.character(connect_df$value[match(k_full$lakeconnection, connect_df$lakeconnection)]))

#Omit Laura Lake as this lake was clearly two separated lakes
k_full<-k_full[which(k_full$Lake!='LauraLake'),]

# List of useful predictor variables
predvars_full<-c(names(k_full)[c(2,6, 10, 12,15,16,17:22)], 'lakeconn')

#Subset predictor variables to eliminate multicollinearity issues
predvars<-predvars_full[c(1:2, 4:7,10,12,13)]

# Tables of variabity stat (e.g., sd), flamepred vars, and morph predvars
if (VarStat=='SemiRange'){ 
  df_sd <- k_full[SemiRange_columns]
  sd_columns_pix<-SemiRange_columns
  k<-na.omit(k_full[c('Lake', predvars, flamepredvars, SemiRange_columns)])
  
} else if (VarStat=='SemiRangeOverCutoff'){
  df_sd <- k_full[SemiRangeOverCutoff_columns]
  sd_columns_pix<-SemiRangeOverCutoff_columns
  k<-na.omit(k_full[c('Lake', predvars, flamepredvars, SemiRangeOverCutoff_columns)])

} else { df_sd<-k_full[sd_columns_pix]
k<-na.omit(k_full[c('Lake', predvars, flamepredvars, sd_columns_pix)])}

df_median <-k_full[flamepredvars]
df_morph<-k_full[predvars]


# subset to only include lakes will all available variables
# Note that this table is not used for glm and random forest as these can handle missin values
# k<-na.omit(k_full[c('Lake', predvars, flamepredvars, sd_columns_pix)])



############ Simple linear model ###########
# Old way of doing this using all predictors

# set predictor variables (morphometry, and flame variables)
m1<-k[,predvars[1]]
m2<-k[,predvars[2]]
m3<-k[,predvars[3]]
m4<-k[,predvars[4]]
m5<-k[,predvars[length(predvars)]]
m6<-k[,predvars[6]] #lakeconn as numeric
m7<-k[,predvars[7]]
m8<-k[,predvars[8]]
m9<-k[,predvars[5]] #lakeconn as factor


f1<-k[,flamepredvars[1]]
f2<-k[,flamepredvars[2]]
f3<-k[,flamepredvars[3]]
f4<-k[,flamepredvars[4]]
f5<-k[,flamepredvars[5]]
f6<-k[,flamepredvars[6]]

# Loop through 10 y variables (sdTemp, sdSPC, etc.)
modellist<-list()
column<-1
for (column in 1:length(sd_columns_pix)) {
y<-k[,sd_columns_pix[column]]

fvars<-c(1:length(flamepredvars))

xvars<-paste(paste('m', c(1:8), sep='', collapse='+'), paste('f', fvars, sep='', collapse="+"), sep="+")

equation <- paste0('y~', xvars)

fullmodel<-lm(equation)
# vif(fullmodel)
summary(fullmodel)
anova(fullmodel)

stepmodel<-stepAIC(fullmodel, direction='both', k=3)
anova(stepmodel)
summary(stepmodel)

modellist[[column]] <-stepmodel

}

modellist

#print models
model<-1
for (model in 1:length(modellist)){
  print(sd_columns_pix[model])
  summary<-summary(modellist[[model]])
  print(summary)
}


############ Best GLM and RandomForest###########
# Better way of doing this using all predictors
# Code uses k_full which contains NAs
# loops through all response variables and generates a bestglm
# and a random forest model

#Create empty lists to populate
glmlist<-list()
glmloglist<-list()

forest_fit<-list()
forest_fit_log<-list()

rfsrc_fit<-list()
rfsrc_fit_log<-list()

#Start loop
column<-2
for (column in 1:ncol(df_sd)){
  
  name<-names(df_sd)[column]
  name_short<-paste0('Drivers of within-lake ', VarStat, ' of ', shortnames[column])
  df<-data.frame( df_median, df_morph[,-5], y=df_sd[,column])
  # df<-data.frame(df_morph[-which(k$Lake=='LauraLake'),-c(6,8,9,11)], y=df_sd[-which(k$Lake=='LauraLake'),column])
  # df2<-data.frame( df_morph[,-c(6,8,9,11)], y=df_sd[,column])

  names(df)<-c(shortnames[-c(3,5,7:8)], 'DOC', 'TP', 'LA', 'SDI', 'LC', 'WA', 'SLtoLA', 'WAtoLA',  'y')
  
  df_noNA<- na.omit(df)
  df_noNAlog<-df_noNA
  df_noNAlog$y<-log10(df_noNA$y)
  
  glm<-bestglm(df_noNA, family=gaussian, IC='BIC', nvmax=5)
  
  glmlog<-bestglm(df_noNAlog, family=gaussian, IC='BIC', nvmax=5)
  
  print(name)
  print(glm)
  print(summary(glm))
  glmlist[[column]]<-glm
  glmloglist[[column]]<-glmlog
  
  forest_fit[[column]]<-randomForest::randomForest(x=df_noNA[,which(names(df_noNA) != 'y')], y=df_noNA$y, importance=T)
  
  forest_fit_log[[column]]<-randomForest::randomForest(x=df_noNAlog[,which(names(df_noNAlog) != 'y')], y=df_noNAlog$y, importance=T)
  
  rfsrc_fit[[column]] <- rfsrc((y)~., data=df, na.action='na.impute')
  rfsrc_fit_log[[column]] <- rfsrc(log10(y)~., data=df, na.action='na.impute')
  
  png(paste0("Figures/RandomForest/", VarStat, "/RF_", VarStat, name,  ".png"), res=200, width=8,height=6, units="in")
  print(randomForest::varImpPlot(forest_fit[[column]], main=name_short))
  dev.off()
  
  png(paste0("Figures/RandomForest/", VarStat, "/RF_log_", VarStat, name,  ".png"), res=200, width=8,height=6, units="in")
  print(randomForest::varImpPlot(forest_fit_log[[column]], main=name_short))
  dev.off()
  
  png(paste0("Figures/RandomForest/", VarStat, "/GG_", VarStat, name,  ".png"), res=200, width=8,height=6, units="in")
  GG<- plot(gg_vimp(rfsrc_fit[[column]], nvar=10)) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(name_short) +
    labs(y='Variable importance')
  print(GG) 
  dev.off()
  
  png(paste0("Figures/RandomForest/", VarStat, "/GG_log_", VarStat, name,  ".png"), res=200, width=8,height=6, units="in")
  GGlog<- plot(gg_vimp(rfsrc_fit_log[[column]], nvar=10)) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(paste0(name_short, ' log')) +
    labs(y='Variable importance')
  print(GGlog) 
  dev.off()
  
}


#### GLM Scatterplots of all spatial stats ####
png(paste0("Figures/GLMModel/GLM_", VarStat, ".png"), res=200, width=4.5,height=10, units="in")

par(mar=c(2,2,.5,.5), oma=c(1.5,1.5,1.5,0))
par(mgp=c(2, .5, 0))
par(mfrow=c(5,2))
for (model in 1:length(glmlist)){
  
  glmplot(glmlist[[model]], pch=1, ylab='', xlab='')
  abline(0,1, lty=2)
  legend('top', inset=0.01, c(shortnames[model]), bty='n')
  
  #Check for null model and add adjusted r squared
  if (length(glmlist[[model]]$BestModel[[1]])>1){
    lm<-glm2lm(glmlist[[model]])
    r2<-round(summary(lm)$adj.r.squared,2)
    legend('bottomright', inset=0.01, c(paste0('r2=', (r2))), bty='n')
  } else {
    legend('bottomright', inset=0.01, c('Best model = NULL'), bty='n')
  }
  
  
}
mtext(paste0('Predicted ', VarStat), 2, 0, outer=T)
mtext(paste0('Observed ',VarStat), 1, 0, outer=T)
mtext(paste0('Best GLM for predicting within lake ', VarStat), 3, 0, outer=T)


dev.off()

#### GLM Scatterplots of all spatial stats LOG VERSION ####
png(paste0("Figures/GLMModel/GLMlog_", VarStat, ".png"), res=200, width=4.5,height=10, units="in")

par(mar=c(2,2,.5,.5), oma=c(1.5,1.5,1.5,0))
par(mgp=c(2, .5, 0))
par(mfrow=c(5,2))
for (model in 1:length(glmloglist)){
  
  glmplot(glmloglist[[model]], pch=1, ylab='', xlab='')
  abline(0,1, lty=2)
  legend('top', inset=0.01, c(shortnames[model]), bty='n')
  
  #Check for null model and add adjusted r squared
  if (length(glmloglist[[model]]$BestModel[[1]])>1){
    lm<-glm2lm(glmloglist[[model]])
    r2<-round(summary(lm)$adj.r.squared,2)
    legend('bottomright', inset=0.01, c(paste0('r2=', (r2))), bty='n')
  } else {
    legend('bottomright', inset=0.01, c('Best model = NULL'), bty='n')
  }
  
}
mtext(paste0('Predicted ', VarStat), 2, 0, outer=T)
mtext(paste0('Observed ',VarStat), 1, 0, outer=T)
mtext(paste0('Best GLM for predicting log ', VarStat, ' of'), 3, 0, outer=T)


dev.off()


#### Variable importance plots of all response variables ####
png(paste0("Figures/RandomForest/", VarStat, "/GG_", VarStat, "_AllVars.png"), res=200, width=8,height=10, units="in")

RFplots<-list()
for (column in 1:length(rfsrc_fit)){
  
  name_short<-shortnames[column]
  
RFplots[[column]]<- plot(gg_vimp(rfsrc_fit[[column]], nvar=8)) +
  theme_bw() +
  theme(legend.position="none") +
  theme(plot.margin=unit(c(0,0,0,0),"in")) + 
  ggtitle(name_short) +
  labs(y='') + 
  theme(plot.title = element_text(size = 10, hjust=0.5, vjust=0))
}

grid.arrange(RFplots[[1]], RFplots[[2]], RFplots[[3]],RFplots[[4]],RFplots[[5]],RFplots[[6]],RFplots[[7]],RFplots[[8]],RFplots[[9]],RFplots[[10]], 
             top=textGrob(paste0("Random forest top predictors of within lake ", VarStat, " of"),
                          gp=gpar(fontsize=10)),
             bottom=textGrob("Variable importance", gp=gpar(fontsize=10)),
             left=textGrob("Predictor variable", gp=gpar(fontsize=10), rot=90),
             ncol=2)

dev.off()

#### Variable importance plots of all response variables log version####
png(paste0("Figures/RandomForest/", VarStat, "/GG_log_", VarStat, "_AllVars.png"), res=200, width=8,height=10, units="in")

RFplotslog<-list()
for (column in 1:length(rfsrc_fit_log)){
  
  name_short<-shortnames[column]
  
  RFplotslog[[column]]<- plot(gg_vimp(rfsrc_fit_log[[column]], nvar=8)) +
    theme_bw() +
    theme(legend.position="none") +
    theme(plot.margin=unit(c(0,0,0,0),"in")) + 
    ggtitle(name_short) +
    labs(y='') + 
    theme(plot.title = element_text(size = 10, hjust=0.5, vjust=0))
}

grid.arrange(RFplotslog[[1]], RFplotslog[[2]], RFplotslog[[3]],RFplotslog[[4]],RFplotslog[[5]],RFplotslog[[6]],RFplotslog[[7]],RFplotslog[[8]],RFplotslog[[9]],RFplotslog[[10]], 
             top=textGrob(paste0("Random forest top predictors of within lake log ", VarStat, " of"),
                          gp=gpar(fontsize=10)),
             bottom=textGrob("Variable importance", gp=gpar(fontsize=10)),
             left=textGrob("Predictor variable", gp=gpar(fontsize=10), rot=90),
             ncol=2)

dev.off()



#### Boxplots of spatial stat by lake connection ####
if (VarStat %in% c('SemiRange', 'SemiRangeOverCutoff')==FALSE){
  png(paste0("Figures/Boxplots/", VarStat, "_lakeconnection_points.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(1.5,2,.5,.5), oma=c(1.5,2,1.5,0))
  par(mgp=c(2, .3, 0), tck=-0.02)
  
  var_nu<-1
  for (var_nu in 1:length(sd_columns)){
    y<-k_full[,sd_columns[var_nu]]
    
    boxplot(y ~ k_full$lakeconn, col=viridis(4), boxwex=0.5)
    legend('top', inset=0, shortnames[var_nu], bty='n', xjust=0.5, x.intersp=0)
  }
  mtext('Lake Connection', 1,0,outer=T)
  mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
  mtext('Points', 3,-.25,outer=T)
  
  dev.off()
}

png(paste0("Figures/Boxplots/", VarStat, "_lakeconnection_pixels.png"), res=200, width=4.5,height=10, units="in")

par(mfrow=c(5,2))
par(mar=c(1.5,2,.5,.5), oma=c(1.5,2,1.5,0))
par(mgp=c(2, .3, 0), tck=-0.02)

var_nu<-1
for (var_nu in 1:length(sd_columns_pix)){
  y<-k_full[,sd_columns_pix[var_nu]]
  
  boxplot(y ~ k_full$lakeconn, col=viridis(4), boxwex=0.5)
  legend('top', inset=0, shortnames[var_nu], bty='n', xjust=0.5, x.intersp=0)
}
mtext('Lake Connection', 1,0,outer=T)
mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
mtext('Pixels', 3,-.25,outer=T)

dev.off()


# Scatterplots of predictor vars vs all response vars. ####
# Loop through all predictor vars and plot scatterplot of within lake sd
xvars<-c(predvars,flamepredvars)
for (xvar in 1:length(xvars)){
  
  x<-k_full[,xvars[xvar]]
  if (class(x)=='numeric'){
  
    if (VarStat %in% c('SemiRange', 'SemiRangeOverCutoff')==FALSE){
    
  png(paste0("Figures/Scatterplots/", VarStat, "_by_predictor/", VarStat, "_", xvars[xvar], "_points.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(1.5,2,.5,.5), oma=c(1.5,2,1.5,0))
  par(mgp=c(2, .3, 0), tck=-0.02)
  
  var_nu<-1
  for (var_nu in 1:length(sd_columns)){
    y<-k_full[,sd_columns[var_nu]]
    ylim<-c(range(y, na.rm=T)[1],extendrange(y, f=0.14)[2]) 
    plot(y ~ x, bg=viridis(4)[k_full$lakeconn], pch=21, cex=1.5, xlab='', ylab='', ylim=ylim)
    # mtext(shortnames[var_nu], 3,0)
    legend('top', inset=0, shortnames[var_nu], bty='n', xjust=0.5, x.intersp=0)
  }
  mtext(xvars[xvar], 1,0,outer=T)
  mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
  mtext('Points', 3,-.25,outer=T)
  
  dev.off()
    }
  
  png(paste0("Figures/Scatterplots/", VarStat, "_by_predictor/", VarStat, "_", xvars[xvar], "_pixels.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(1.5,2,.5,.5), oma=c(1.5,2,1.5,0))
  par(mgp=c(2, .3, 0), tck=-0.02)
  
  var_nu<-1
  for (var_nu in 1:length(sd_columns_pix)){
    y<-k_full[,sd_columns_pix[var_nu]]
    ylim<-c(range(y, na.rm=T)[1],extendrange(y, f=0.14)[2]) 
    
    plot(y ~ x, bg=viridis(4)[k_full$lakeconn], pch=21, cex=1.5, ylim=ylim)
    legend('top', inset=0, shortnames[var_nu], bty='n', xjust=0.5, x.intersp=0)
    # mtext(shortnames[var_nu], 3,0)
  }
  mtext(xvars[xvar], 1,0,outer=T)
  mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
  mtext('Pixels', 3,-.25,outer=T)
  
  dev.off()
  }
  
}



# Loop through all response vars and plot scatterplot of within lake sd vs each predictor variable
xvars<-c(predvars,flamepredvars)
# df_2 <- data.frame(df_morph, df_median)

var_nu<-1
for (var_nu in 1:length(sd_columns_pix)){
  name<-shortnames[var_nu]
  if (VarStat %in% c('SemiRange', 'SemiRangeOverCutoff')==FALSE){
  y<-k_full[,sd_columns[var_nu]]
  
  
  png(paste0("Figures/Scatterplots/", VarStat, "_by_response/", VarStat, "_", name, "_points.png"), res=200, width=6.5,height=12, units="in")
  
  par(mfrow=c(5,3))
  par(mar=c(1.5,2,1.5,.5), oma=c(2,2,2,0))
  par(mgp=c(2, .3, 0), tck=-0.02)
  
  xvar<-1
  for (xvar in 1:length(xvars)){
    x<-k_full[,xvars[xvar]]
    if (class(x)=='numeric'){

      plot(y ~ x, bg=viridis(4)[k_full$lakeconn], pch=21, cex=1.5, xlab='', ylab='')
      mtext(xvars[xvar], 1,1.5)
    }
  }
  mtext(paste0('Within-lake ', VarStat, ' of ', name), 2,0,outer=T)
    mtext(paste0(name, ' Points'), 3,-1,outer=T)
    
    dev.off()
    
  }
    
    y2<-k_full[,sd_columns_pix[var_nu]]
    
    png(paste0("Figures/Scatterplots/", VarStat, "_by_response/", VarStat, "_", name, "_pixels.png"), res=200, width=6.5,height=12, units="in")
    
    par(mfrow=c(5,3))
    par(mar=c(1.5,2,1.5,.5), oma=c(2,2,2,0))
    par(mgp=c(2, .3, 0), tck=-0.02)
    
    for (xvar in 1:length(xvars)){
      x<-k_full[,xvars[xvar]]
      if (class(x)=='numeric'){
        
        plot(y2 ~ x, bg=viridis(4)[k_full$lakeconn], pch=21, cex=1.5, xlab='', ylab='')
        mtext(xvars[xvar], 1,1.5)
      }
    }
    mtext(paste0('Within-lake ', VarStat, ' of ', name), 2,0,outer=T)
    mtext(paste0(name, ' Pixels'), 3,-1,outer=T)
    
    dev.off()
}
    