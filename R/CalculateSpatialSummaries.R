# ###########################################
# Code to make lots of carbon plots on lake Mendota 2016
# ##########################################
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
source('R/AddAlpha.R')

setwd("E:/Git_Repo/NHLDLakes")

#Get Lagos data
mylagos2<-readRDS(file='Data/MyLakesLagos.rds')
lagosvars<-c("lake_area_ha", "lake_perim_meters", "meandepth", "maxdepth", "iws_ha", "iws_perimkm", "iws_lakeareaha", "ShorelineIndex")

lagos_subset <- mylagos2 %>%
  dplyr::select(lagosvars)

mean_lagos <- lagos_subset %>%
  summarize_all(mean, na.rm=T)

median_lagos <- lagos_subset %>%
  summarize_all(median, na.rm=T)

min_lagos <- lagos_subset %>%
  summarize_all(min, na.rm=T)

max_lagos <- lagos_subset %>%
  summarize_all(max, na.rm=T)

Statistic<-c('Median', 'Mean', 'Min', 'Max')
lagos_summary<-data.frame(Statistic, as.data.frame(rbind(median_lagos, mean_lagos, min_lagos, max_lagos)))
lagos_summary

# Get table of spatial summaries for all flame runs on Lake Mendota
merged_summary<-readRDS('Data/FlameSpatialSummaries.rds')
vars<-names(merged_summary)
orderedvars<-c("TempC_t", "SPCScm_t",  "TrbFNU_t", "fDOMRFU_t",  "pH_tau", "ODOmgL_t", "ChlARFU_t", "ChlAgL_t", "BGAPCRFU_t", "BGAPCgL_t", "CO2uM_t", "CH4uM_t", "NITRATEM", "ABS254", "ABS350")
goodvars <- vars[which(vars %in% orderedvars)]


mean_table <- merged_summary %>%
  filter(Statistic == 'Mean') %>%
  summarize_at(goodvars, mean, na.rm=T)

median_table <- merged_summary %>%
  filter(Statistic == 'Median') %>%
  summarize_at(goodvars, median, na.rm=T)

min_table <- merged_summary %>%
  filter(Statistic == 'Mean') %>%
  summarize_at(goodvars, min, na.rm=T)

max_table <- merged_summary %>%
  filter(Statistic == 'Mean') %>%
  summarize_at(goodvars, max, na.rm=T)

range_mean <- merged_summary %>%
  filter(Statistic == 'range') %>%
  summarize_at(goodvars, mean, na.rm=T)

range_median <- merged_summary %>%
  filter(Statistic == 'range') %>%
  summarize_at(goodvars, median, na.rm=T)

range_min <- merged_summary %>%
  filter(Statistic == 'range') %>%
  summarize_at(goodvars, min, na.rm=T)

range_max <- merged_summary %>%
  filter(Statistic == 'range') %>%
  summarize_at(goodvars, max, na.rm=T)

chem_summary<-data.frame(Statistic, as.data.frame(rbind(median_table, mean_table, min_table, max_table)))

chem_summary<-chem_summary[,c('Statistic', orderedvars)]
chem_summary

chemlagos_summary<-left_join(chem_summary, lagos_summary, all=T)

t_summary<-data.frame(Variable=names(chemlagos_summary)[-1], as.data.frame(t(chemlagos_summary[,-1])))
names(t_summary)<-c('Variable', Statistic)
rownames(t_summary) <- c()
t_summary[,2:5]<-apply(t_summary[,2:5], 2, signif, digits=4)

write.table(t_summary, file='Data/AcrossLakeSummary.csv', row.names=F, sep=',')
saveRDS(t_summary, file='Data/AcrossLakeSummary.rds')
