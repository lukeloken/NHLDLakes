# ###########################################
# Code to produce across lake summaries of medians
# ##########################################
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)

setwd("E:/Git_Repo/NHLDLakes")
source('R/AddAlpha.R')

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

#Get Lagos data
# mylagos2<-readRDS(file='Data/MyLakesLagos.rds')
morphvars<-c("lake_area_ha", "lake_perim_meters", "ShorelineIndex", "maxdepth", "iws_ha", 'iwsarea_to_lakearea', "iws_streamdensity_streams_sum_lengthm", 'streamlength_to_lakearea', 'lakeconnection')
morphnames<-c('Lake area', 'Lake perimeter', 'Shoreline development index', 'Max depth', 'Watershed area', 'Watershed area to lake area', 'Total stream length', 'Total stream length to lake area', 'Lake connection')
morphabbr<-c('LA', 'LP', 'SDI', 'Z', 'WA', 'WA:LA', 'SL', 'SL:LA', 'LC')

morphvars<-c("lake_area_ha", "lake_perim_meters", "ShorelineIndex", "maxdepth", "iws_ha", 'iwsarea_to_lakearea', "iws_streamdensity_streams_sum_lengthm", 'streamlength_to_lakearea')
morphnames<-c('Lake area', 'Lake perimeter', 'Shoreline development index', 'Max depth', 'Watershed area', 'Watershed area to lake area', 'Total stream length', 'Total stream length to lake area')
morphabbr<-c('LA', 'LP', 'SDI', 'Z', 'WA', 'WA:LA', 'SL', 'SL:LA')

chemvars<-c('TotalPUF', "TotalNUF", "SRP", "NO3NO2", "NH4", 'DOC', 'Cl')
chemnames<-c('Total P', 'Total N', 'SRP', 'NO3-N', 'NH4-N', 'DOC', 'Chloride')
chemabbr<-c('TP', 'TN', 'SRP', 'NO3', 'NH4', 'DOC', 'Cl')


goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "pH", "TurbFNU", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM",  "pH", "Turb", "DO", "CO2", "CH4", "ChlA", "BGA")
longnames<-c('Temperature', 'Specific conductivity', 'Fluorescent dissolved organic matter', 'pH', 'Turbidity', 'Dissolved oxygen', 'Carbon dioxide', 'Methane', 'Chlorophyll A', 'Blue green algae/phycocyanin')
goodvars_points<-paste(goodvars, 'points', sep='_')

flamevars<-paste(goodvars, 'pixels', 'Median', sep='_')
flamevars_sd<-paste(goodvars, 'pixels', 'sd', sep='_')


table_subset <- j %>%
  group_by(Lake) %>%
  dplyr::summarize_at(c(morphvars, chemvars, flamevars), median, na.rm=T)

mins<- table_subset %>%
  dplyr::summarize_at(c(morphvars, chemvars, flamevars), min, na.rm=T)

maxs<- table_subset %>%
  dplyr::summarize_at(c(morphvars, chemvars, flamevars), max, na.rm=T)

medians<- table_subset %>%
  dplyr::summarize_at(c(morphvars, chemvars, flamevars), median, na.rm=T)

means<- table_subset %>%
  dplyr::summarize_at(c(morphvars, chemvars, flamevars), mean, na.rm=T)

sds<- table_subset %>%
  dplyr::summarize_at(c(morphvars, chemvars, flamevars), sd, na.rm=T)

table_subset_sd <- j %>%
  group_by(Lake) %>%
  dplyr::summarize_at(c(flamevars_sd), median, na.rm=T) %>%
  dplyr::summarize_at(c(flamevars_sd), median, na.rm=T)

table_bound <- bind_rows(medians, means, mins, maxs, sds)
table_bound$lake_perim_meters<-table_bound$lake_perim_meters/1000
table_bound$iws_streamdensity_streams_sum_lengthm<-table_bound$iws_streamdensity_streams_sum_lengthm/1000
names(table_bound)<-c(morphnames, chemnames, longnames)

table_out<-as.data.frame(t(table_bound))
names(table_out)<-c('Median', 'Mean', 'Min', 'Max', 'SD')
table_out$Variable<-row.names(table_out)
table_out$Abbr<-c(morphabbr, chemabbr, shortnames)

row.names(table_out)<-NULL

table_out[,1:5]<-apply(table_out[,1:5], 2, round, digits=2)
table_out<-table_out[,c(6:7,1:5)]


write.table(table_out, file='Data/AcrossLakeSummary.csv', row.names=F, sep=',')
saveRDS(table_out, file='Data/AcrossLakeSummary.rds')




sd_table<-data.frame(var1=as.numeric(sds[flamevars]), var2=as.numeric(table_subset_sd))
sd_table$percentamong<-sd_table$var1/(sd_table$var1+sd_table$var2)
sd_table$percentwithin<-sd_table$var2/(sd_table$var1+sd_table$var2)
sd_table$withinoveramong<-sd_table$var2/(sd_table$var1)

  
barcolors<-c('grey20', 'grey80')


png("Figures/sdAmongvsWithinlakes.png", width=5,height=3, units="in", res=400)

par(mfrow=c(1,1))
par(mar=c(2.5,3,1,1))
par(mgp=c(3,.5,0), tck=-0.02)

colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,1,2,2,2,2,2)]

# barplot(rep(1,10), las=1, axes=F, names.arg=shortnames, col=barcolors[2], yaxs='i', ylim=c(0,1), cex.names=.7, mgp=c(3,.1,0))
# barplot(sd_table$percentwithin, add=T, col=barcolors[1], axes=F)
# barplot(sd_table$withinoveramong, las=1, axes=F, names.arg=shortnames, col=barcolors[1], yaxs='i', ylim=c(0,1), cex.names=.7, mgp=c(3,.1,0))
barplot(sd_table$withinoveramong, las=1, axes=F, names.arg=shortnames, col=colorbyvar, yaxs='i', ylim=c(0,1), cex.names=.7, mgp=c(3,.1,0))



axis(2,at=seq(0,1,.2), labels=seq(0,1,.2), las=1)
mtext('SD within lakes : SD among lakes',2,1.75)
mtext('Variable',1,1.5)

# legend('topleft', inset=0.02, c('Among lakes*', 'Within lakes'), fill=barcolors[2:1], x.intersp = 0.8)

box(which='plot')

dev.off()


