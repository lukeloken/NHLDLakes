#Code to link range/correlation distance to spatial summaries

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(vioplot)
library(ggplot2)
library(scales)

setwd("E:/Git_Repo/NHLDLakes")

# spatial pixel summaries
merged_summary <- readRDS(file='Data/FlameSpatialSummaries.rds')
merged_summary$GeoType<-rep('pixels', nrow(merged_summary))

# spatial points summaries
merged_points <- readRDS(file='Data/FlamePointsSummaries.rds')
merged_points$GeoType<-rep('points', nrow(merged_points))

a <- full_join(merged_summary, merged_points)
lakedays<-unique(paste(a$Date, a$Lake, sep='_'))

b <- a %>%
  dplyr::select(-LakeDay) %>% 
  tidyr::gather(key=variable, value=value, -c(Lake, Date, Statistic, GeoType))
head(b)
  
# Semivariance and correlation outputs
# moran_ncf_alllakes <- readRDS( file='Data/moran_ncf_alllakes.rds')
semivar_alllakes <- readRDS(file='Data/semivar_alllakes.rds')
head(semivar_alllakes)
semivar_alllakes$range_best[semivar_alllakes$model_type=='Nug']<-NA
semivar_alllakes[which(semivar_alllakes[,c('range_best')]==Inf), c('range_best')]<-semivar_alllakes[which(semivar_alllakes[,c('range_best')]==Inf), c('cutoff')]

names(semivar_alllakes)[c(4,9,10)]<-c('PSill', 'SemiRange', 'SemiCutoff')

semivar_alllakes$SemiRangeOverCutoff <- semivar_alllakes$SemiRange/semivar_alllakes$SemiCutoff

c <- semivar_alllakes %>%
  filter(variable != 'NA', lake_day %in% lakedays) %>% 
  dplyr::select(variable, Date, Lake, PSill, SemiRange, SemiCutoff, SemiRangeOverCutoff, SillTotal, SillPercent) %>%
  gather(key=Statistic, value=value, -c(Lake, Date,  variable))

c$GeoType<-rep('points', nrow(c))

d<-full_join(b,c)

saveRDS(d, file='Data/FlameStatsAll.rds')

