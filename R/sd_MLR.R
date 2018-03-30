
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

#Wide table
j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

#Color scheme to be used for all plotting
colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

#Spatial heterogeneiety stat to use
VarStat <-'sd'

#Vectors of names
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

mediannames<-paste0(goodvars, 'Median')
goodvars_points<-paste(goodvars, 'points', sep='_')

sd_columns<-paste(goodvars, 'points', VarStat, sep='_')
mean_columns<-paste(goodvars, 'points', 'Mean', sep='_')

sd_columns_pix<-paste(goodvars, 'pixels', VarStat, sep='_')
med_columns_pix<-paste(goodvars, 'pixels', 'Median', sep='_')

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
predvars<-predvars_full[c(1:7,10,12,13)]

# Tables of variabity stat (e.g., sd), flamepred vars, and morph predvars
df_sd<-k_full[sd_columns_pix]
df_median <-k_full[flamepredvars]
df_morph<-k_full[predvars]

# subset to only include lakes will all available variables
# Note that this table is not used for glm and random forest as these can handle missin values
k<-na.omit(k_full[c('Lake', predvars, flamepredvars, sd_columns_pix)])



############ Simple linear model ###########
# Old way of doing this using all predictors

# set predictor variables (morphometry, and flame variables)
m1<-k[,predvars[1]]
m2<-k[,predvars[2]]
m3<-k[,predvars[3]]
m4<-k[,predvars[4]]
m5<-k[,predvars[5]]
m6<-k[,predvars[length(predvars)]] #lakeconn as numeric
m7<-k[,predvars[7]]
m8<-k[,predvars[8]]
m9<-k[,predvars[9]]
m10<-k[,predvars[6]] #lakeconn as factor


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

xvars<-paste(paste('m', c(1:9), sep='', collapse='+'), paste('f', fvars, sep='', collapse="+"), sep="+")

equation <- paste0('y~', xvars)

fullmodel<-lm(equation)
vif(fullmodel)
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
forest_fit<-list()
rfsrc_fit<-list()

#Start loop
column<-1
for (column in 1:ncol(df_sd)){
  
  name<-names(df_sd)[column]
  name_short<-paste0('Drivers of within-lake ', VarStat, ' of ', shortnames[column])
  df<-data.frame( df_median, df_morph[,-10], y=df_sd[,column])
  # df<-data.frame(df_morph[-which(k$Lake=='LauraLake'),-c(6,8,9,11)], y=df_sd[-which(k$Lake=='LauraLake'),column])
  # df2<-data.frame( df_morph[,-c(6,8,9,11)], y=df_sd[,column])

  df_noNA<- na.omit(df)
  names(df_noNA)<-c(shortnames[-c(3,5,7:8)], 'DOC', 'TP', 'Cl', 'LA', 'SDI', 'LC', 'WA', 'SLtoLA', 'WAtoLA',  'y')
  
  glm<-bestglm(df_noNA, family=gaussian, IC='BIC')
  
  print(name)
  print(glm)
  print(summary(glm))
  glmlist[[column]]<-glm
  
  forest_fit[[column]]<-randomForest::randomForest(x=df_noNA[,which(names(df_noNA) != 'y')], y=df_noNA$y, importance=T)
  
  rfsrc_fit[[column]] <- rfsrc(y~., data=df_noNA)
  
  png(paste0("Figures/RandomForest/", VarStat, "/RF_", VarStat, name,  ".png"), res=200, width=8,height=6, units="in")
  print(randomForest::varImpPlot(forest_fit[[column]], main=name_short))
  dev.off()
  
  png(paste0("Figures/RandomForest/", VarStat, "/GG_", VarStat, name,  ".png"), res=200, width=8,height=6, units="in")
  RF1<- plot(gg_vimp(rfsrc_fit[[column]], nvar=10)) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(name_short) +
    labs(y='Variable importance')
  print(RF1) 
  dev.off()
  
}

str(glmlist[[1]])

glm2lm<-function(glm){
  df<-glm[[1]]$model
  equation<-paste0(names(df)[1], '~', paste(names(df)[2:ncol(df)], collapse='+'))
  lm<-lm(equation, data=df)
  return(lm)
}


glmplot<-function (glm, ...){
  y_obs<-glm[[1]]$model[,1]
  y_pred<-predict(glm$BestModel, glm[[1]]$model)
  plotlim<-range(c(y_obs, y_pred), na.rm=T)
  plot(y_obs, y_pred, xlim=plotlim, ylim=plotlim,...)
}


lmlist<-lapply(glmlist, glm2lm)

lapply(lmlist, function(l) summary(l))
lapply(lmlist, function(l) anova(l))


png(paste0("Figures/GLMModel/GLM_", VarStat, ".png"), res=200, width=4.5,height=10, units="in")

par(mar=c(2,2,.5,.5), oma=c(1.5,1.5,1.5,0))
par(mgp=c(2, .5, 0))
par(mfrow=c(5,2))
for (model in 1:length(glmlist)){
  
glmplot(glmlist[[model]], pch=1, ylab='', xlab='')
abline(0,1, lty=2)
r2<-round(summary(lmlist[[model]])$adj.r.squared,2)

legend('top', inset=0.01, c(shortnames[model]), bty='n')
legend('bottomright', inset=0.01, c(paste0('r2=', (r2))), bty='n')

}
mtext('Predicted standard deviation', 2, 0, outer=T)
mtext('Observed standard deviation', 1, 0, outer=T)
mtext(paste0('Best GLM for predicting within lake ', VarStat), 3, 0, outer=T)


dev.off()



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


# plot(gg_minimal_vimp(rfsrc_fit[[column]]))

png(paste0("Figures/Boxplots/", VarStat, "_lakeconnection_points.png"), res=200, width=4.5,height=10, units="in")

par(mfrow=c(5,2))
par(mar=c(1.5,2,1.5,.5), oma=c(1.5,2,1,0))
par(mgp=c(2, .5, 0))
ymax<-c(1.1,3.2,2,2,.4,.8,20,1,.45,.25)

var_nu<-1
for (var_nu in 1:length(sd_columns)){
y<-k_full[,sd_columns[var_nu]]

boxplot(y ~ k_full$lakeconn, ylim=c(0,ymax[var_nu]), col=viridis(4), boxwex=0.5)
legend('top', inset=0, shortnames[var_nu], bty='n', xjust=0.5, x.intersp=0)
}
mtext('Lake Connection', 1,0,outer=T)
mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
mtext('Points', 3,-1,outer=T)

dev.off()

png(paste0("Figures/Boxplots/", VarStat, "_lakeconnection_pixels.png"), res=200, width=4.5,height=10, units="in")

par(mfrow=c(5,2))
par(mar=c(1.5,2,1.5,.5), oma=c(1.5,2,1,0))
par(mgp=c(2, .5, 0))
ymax<-c(.32,1,.7,.7,.2,.2,5,.4,.2,.05)

var_nu<-1
for (var_nu in 1:length(sd_columns_pix)){
  y<-k_full[,sd_columns_pix[var_nu]]
  
  boxplot(y ~ k_full$lakeconn, ylim=c(0,ymax[var_nu]), col=viridis(4), boxwex=0.5)
  legend('top', inset=0, shortnames[var_nu], bty='n', xjust=0.5, x.intersp=0)
}
mtext('Lake Connection', 1,0,outer=T)
mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
mtext('Pixels', 3,-1,outer=T)

dev.off()


# Loop through all predictor vars and plot scatterplot of within lake sd of each variable
for (xvar in 1:length(predvars)){
  
  x<-df_morph[,predvars[xvar]]
  if (class(x)=='numeric'){
  
  png(paste0("Figures/Scatterplots/", VarStat, "_by_predictor/", VarStat, "_", predvars[xvar], "_points.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(1.5,2,1.5,.5), oma=c(1.5,2,2,0))
  par(mgp=c(2, .3, 0), tck=-0.02)
  ymax<-c(1.1,3.2,2,2,.4,.8,20,1,.45,.25)
  
  var_nu<-1
  for (var_nu in 1:length(sd_columns)){
    y<-k_full[,sd_columns[var_nu]]
    
    plot(y ~ x, bg=viridis(4)[k_full$lakeconn], pch=21, cex=1.5, xlab='', ylab='')
    mtext(shortnames[var_nu], 3,0)
  }
  mtext(predvars[xvar], 1,0,outer=T)
  mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
  mtext('Points', 3,-1,outer=T)
  
  dev.off()
  
  png(paste0("Figures/Scatterplots/", VarStat, "_by_predictor/", VarStat, "_", predvars[xvar], "_pixels.png"), res=200, width=4.5,height=10, units="in")
  
  par(mfrow=c(5,2))
  par(mar=c(1.5,2,1.5,.5), oma=c(1.5,2,2,0))
  par(mgp=c(2, .3, 0), tck=-0.02)
  ymax<-c(1.1,3.2,2,2,.4,.8,20,1,.45,.25)
  
  var_nu<-1
  for (var_nu in 1:length(sd_columns_pix)){
    y<-k_full[,sd_columns_pix[var_nu]]
    
    plot(y ~ x, bg=viridis(4)[k_full$lakeconn], pch=21, cex=1.5)
    mtext(shortnames[var_nu], 3,0)
  }
  mtext(predvars[xvar], 1,0,outer=T)
  mtext(paste0('Within-lake ', VarStat), 2,0,outer=T)
  mtext('Pixels', 3,-1,outer=T)
  
  dev.off()
  }
  
}



# Loop through all response vars and plot scatterplot of within lake sd vs each predictor variable
xvars<-c(predvars,flamepredvars)
# df_2 <- data.frame(df_morph, df_median)

var_nu<-1
for (var_nu in 1:length(sd_columns)){
  y<-k_full[,sd_columns[var_nu]]
  name<-shortnames[var_nu]
  
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
    mtext(paste0('Within lake ', VarStat, ' of ', name), 2,0,outer=T)
    mtext(paste0(name, ' Points'), 3,-1,outer=T)
    
    dev.off()
    
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
    mtext(paste0('Within lake ', VarStat, ' of ', name), 2,0,outer=T)
    mtext(paste0(name, ' Pixels'), 3,-1,outer=T)
    
    dev.off()
}
    