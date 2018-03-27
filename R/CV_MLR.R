

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

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_points<-paste(goodvars, 'points', sep='_')


CV_columns<-paste(goodvars, 'points', 'CV', sep='_')
mean_columns<-paste(goodvars, 'points', 'Mean', sep='_')


#only use lakes with lake area
k<-j[which(!is.na(j$lake_area_ha)),]
#Change lake connection to numeric (1 = higher landscape position)
connect_df<-data.frame(lakeconnection=c('Isolated', 'Headwater', 'DR_Stream', 'DR_LakeStream'), value=1:4)
k$lakeconn <- as.numeric(as.character(connect_df$value[match(k$lakeconnection, connect_df$lakeconnection)]))

# predvars<-names(k)[c(2,4,6:17)]
predvars<-c(names(k)[c(2,6, 10, 12,15,16,17:22)], 'lakeconn')


df_CV<-k[CV_columns]
df_mean <-k[mean_columns]
df_morph<-k[predvars]


#set y value (Start with Temp and loop through all CV vars)
y<-k[,CV_columns[1]]

# set predictor variables (morphometry, and flame variables)
m1<-df_morph[,predvars[1]]
m2<-df_morph[,predvars[2]]
m3<-df_morph[,predvars[3]]
m4<-df_morph[,predvars[4]]
m5<-df_morph[,predvars[5]]
m6<-df_morph[,predvars[length(predvars)]]
m7<-df_morph[,predvars[7]]
m8<-df_morph[,predvars[8]]
m9<-df_morph[,predvars[9]]
m10<-df_morph[,predvars[10]]
m11<-df_morph[,predvars[11]]
m12<-df_morph[,predvars[12]]
m13<-df_morph[,predvars[6]]

f1<-df_mean[,mean_columns[1]]
f2<-df_mean[,mean_columns[2]]
f3<-df_mean[,mean_columns[3]]
f4<-df_mean[,mean_columns[4]]
f5<-df_mean[,mean_columns[5]]
f6<-df_mean[,mean_columns[6]]
f7<-df_mean[,mean_columns[7]]
f8<-df_mean[,mean_columns[8]]
f9<-df_mean[,mean_columns[9]]
f10<-df_mean[,mean_columns[10]]


fullmodel<-lm(y~m1+m2+m3+m4+m5+m6+f1+f2+f3+f4+f5+f6+f7+f8+f9+f10)
summary(fullmodel)
anova(fullmodel)

stepmodel<-stepAIC(fullmodel, direction='both')
anova(stepmodel)
summary(stepmodel)

