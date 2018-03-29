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

chemvars<-c('TotalPUF', "TotalNUF", "SRP", "NO3NO2", "NH4", 'DOC', 'Cl')
chemnames<-c('Total P', 'Total N', 'SRP', 'NO3-N', 'NH4-N', 'DOC', 'Chloride')
chemabbr<-c('TP', 'TN', 'SRP', 'NO3', 'NH4', 'DOC', 'Cl')


goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")
longnames<-c('Temperature', 'Specific conductivity', 'Fluorescent dissolved organic matter', 'Turbidity', 'pH', 'Dissolved oxygen', 'Carbon dioxide', 'Methane', 'Chlorophyll A', 'Blue green algae/phycocyanin')
goodvars_points<-paste(goodvars, 'points', sep='_')

flamevars<-paste(goodvars, 'pixels', 'Median', sep='_')

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

table_bound <- bind_rows(medians, means, mins, maxs)
table_bound$lake_perim_meters<-table_bound$lake_perim_meters/1000
table_bound$iws_streamdensity_streams_sum_lengthm<-table_bound$iws_streamdensity_streams_sum_lengthm/1000
names(table_bound)<-c(morphnames, chemnames, longnames)

table_out<-as.data.frame(t(table_bound))
names(table_out)<-c('Median', 'Mean', 'Min', 'Max')
table_out$Variable<-row.names(table_out)
table_out$Abbr<-c(morphabbr, chemabbr, shortnames)

row.names(table_out)<-NULL

table_out[,1:4]<-apply(table_out[,1:4], 2, round, digits=2)
table_out<-table_out[,c(5:6,1:4)]


write.table(table_out, file='Data/AcrossLakeSummary.csv', row.names=F, sep=',')
saveRDS(table_out, file='Data/AcrossLakeSummary.rds')


