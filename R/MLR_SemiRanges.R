

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(scales)
library(PerformanceAnalytics)
library(leaps)
library(MASS)

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_points<-paste(goodvars, 'points', sep='_')

SemiVars <- c('TmpC_h', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_h', 'ODOmgL_h', 'CO2M_h', 'CH4M_h', 'ChlARFU_h', 'BGAPCRFU_h')

SemiRange_columns<-paste(SemiVars, 'points', 'SemiRange', sep='_')

df_semi<-j[SemiRange_columns]

#only use lakes with lake area
k<-j[which(!is.na(j$lake_area_ha)),]

# predvars<-names(k)[c(2,4,6:17)]
predvars<-names(k)[c(2,6, 10, 12,15,16)]


y<-k[,SemiRange_columns[9]]
x1<-k[,predvars[1]]
x2<-k[,predvars[2]]
x3<-k[,predvars[3]]
x4<-k[,predvars[4]]
x5<-k[,predvars[5]]
x6<-k[,predvars[6]]
# x7<-k[,predvars[7]]
# x8<-k[,predvars[8]]
# x9<-k[,predvars[9]]
# x10<-k[,predvars[10]]
# x11<-k[,predvars[11]]
# x12<-k[,predvars[12]]
# x13<-k[,predvars[13]]
fullmodel<-lm(y~x1+x2+x3+x4+x5+x6)
summary(fullmodel)
anova(fullmodel)

stepmodel<-stepAIC(fullmodel, direction='both')
anova(stepmodel)
summary(stepmodel)

bestglm<-bestglm(k[c(predvars, SemiRange_columns[9])])
summary(bestglm)

chla.pca <- prcomp(na.omit(k[predvars[c(1:11,13:14)]]), center = TRUE, scale. = TRUE) 

plot(chla.pca, type='l')
summary(chla.pca)


plot(k[,predvars[9]], y)
plot(k[,predvars[3]], y)
plot(k[,predvars[13]], y)

chla_range_model<-lm(y~k[,predvars[3]]+k[,predvars[9]]+k[,predvars[13]])
anova(chla_range_model)
summary(chla_range_model)

chla_predict<-predict(chla_range_model, k[, predvars[3]], k[, predvars[9]])


y2<-k[,SemiRange_columns[1]]/k[,SemiRange_columns[9]]

y2_model<-lm(y2~k[,predvars[3]]+k[,predvars[9]])

plot(y2~k[,predvars[9]])
plot(y2~k[,predvars[13]])


