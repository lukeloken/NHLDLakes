

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)

setwd("E:/Git_Repo/NHLDLakes")

#Load long dataframe of all lake observations
df <-readRDS(file='Data/FlameAllLakesBoundPoints.rds')
Lakes<-df$LakeDay
Lakes<-gsub('AllequashLake1', 'AllequashLake', Lakes)
Lakes<-gsub('AllequashLake2', 'AllequashLake', Lakes)

date<-ymd(gsub("[A-z]", '', Lakes))

str2<-gsub('[0-9]','',Lakes)
str3<-gsub('-', '', str2)
df$Lake<-gsub('_', '', str3)

            
# Load lagos/water chem data
# Pull Lake connection

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

df$lakeconn_f<-factor(j$lakeconn[match(df$Lake,j$Lake)], c('Isolated', 'Headwater', 'DR_Stream', 'DR_LakeStream'))
df$lakeconn_n <- match(df$lakeconn_f, levels(df$lakeconn_f))

df_short <- df[which(!is.na(df$lakeconn_f)),]
df_short2<-df_short

# df_short2<- df_short[sample(nrow(df_short), min(nrow(df_short), 80000)), ]

#Vectors of column names and shortnames
goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")

shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

Vars <- c('TmpC_hy', 'SPCScm_h', 'fDOMRFU_h', 'TrbFNU_h', 'pH_hyd', 'ODOmgL_h', 'CO2M_hy', 'CH4M_hy', 'ChlARFU_h', 'BGAPCRFU_h')




#Single linear model
model_list<-list()

model_list[[1]] <- lm(TmpC_hy ~ LakeDay, data = df)
model_list[[2]] <- lm(SPCScm_h ~ LakeDay, data = df)
model_list[[3]] <- lm(fDOMRFU_t ~ LakeDay, data = df)
model_list[[4]] <- lm(TrbFNU_h ~ LakeDay, data = df)
model_list[[5]] <- lm(pH_hyd ~ LakeDay, data = df)
model_list[[6]] <- lm(ODOmgL_h ~ LakeDay, data = df)
model_list[[7]] <- lm(XCO2Dppm_h ~ LakeDay, data = df)
model_list[[8]] <- lm(XCH4Dppm_h ~ LakeDay, data = df)
model_list[[9]] <- lm(ChlARFU_h ~ LakeDay, data = df)
model_list[[10]] <- lm(BGAPCRFU_h ~ LakeDay, data = df)

model<-model_list[[1]]
extractvar<-function(model){
  SSs<-anova(model)$'Sum Sq'
  PerVar<-SSs[1]/sum(SSs)
  return(PerVar)
}


PercentVar<-unlist(lapply(model_list, extractvar))
pervar_df<-data.frame(goodvars, PercentVar)

#Figure of percent variance within vs across lakes
png("Figures/VariancePartion.png", res=200, width=5,height=3, units="in", res=400)

par(mfrow=c(1,1))
par(mar=c(2.5,3,1,1))
par(mgp=c(3,.5,0), tck=-0.02)
barcolors<-c('grey20', 'grey80')

barplot(rep(1,10), las=1, axes=F, names.arg=shortnames, col=barcolors[1], yaxs='i', ylim=c(0,1), cex.names=.7, mgp=c(3,.1,0))
barplot(PercentVar, add=T, col=barcolors[2], axes=F)

axis(2,at=seq(0,1,.2), labels=seq(0,100,20), las=1)
mtext('Variance explained (%)',2,1.75)
mtext('Variable',1,1.5)

legend('bottomleft', inset=0.02, c('Within lakes', 'Across lakes'), fill=barcolors, x.intersp = 0.8)

box(which='plot')

dev.off()



# ###############################################
# Multiple level model. Include landscape position as a source of variance. 


model.var <- function(var, data) {
  return(lmer(var ~ 1 + (1|lakeconn_f) + (1|LakeDay), data = data, REML=TRUE))
}

lmer_list<-list()
lmer_summary<-list()
var_nu<-1
for (var_nu in 1:length(Vars)){
  var<-Vars[var_nu]
  lmer_list[[var_nu]]<-model.var(df_short2[,var], data=df_short2)
  print(var)
  lmer_summary[[var_nu]]<-data.frame(VarCorr(lmer_list[[var_nu]]))
  lmer_summary[[var_nu]]$Variable<-var
  print(lmer_summary[[var_nu]])
}

df_lmer <- ldply(lmer_summary, data.frame)
df_lmer$grp <-factor(df_lmer$grp, c( 'lakeconn_f', 'LakeDay', 'Residual'))
df_lmer$Variable <-factor(df_lmer$Variable, Vars)
df_lmer$VarShort <- factor(shortnames[match(df_lmer$Variable, Vars)], shortnames)

#Figure of percent variance within vs across lakes
png("Figures/VariancePartion3Level.png", width=4.5,height=3, units="in", res=400)

par(mfrow=c(1,1))
par(mar=c(2,3,1,1))
par(mgp=c(3,.5,0), tck=-0.02)

varbar<-ggplot(df_lmer[c(1,4,7)], aes(x=VarShort, y=vcov, fill=grp)) + 
  geom_bar(position = "fill",stat = "identity", colour='black', width=0.85, size=0.5) +
  labs(y='Variance explained (%)', x='Variable')  +
  scale_fill_manual("", values = c("grey75", "grey45",  "grey10"), labels=c("Among landscape positions", "Among lakes", "Within lakes")) + 
  theme_bw() + 
  theme(legend.position=c("bottom"), panel.border = element_rect(colour = "black", fill=NA, size=.5)) + 
  scale_y_continuous(labels = seq(0,100,25), breaks=seq(0,1,.25), expand=c(0,0)) 

print(varbar)

dev.off()




