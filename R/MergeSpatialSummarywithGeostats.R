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

b <- a %>%
  dplyr::select(-LakeDay) %>% 
  tidyr::gather(key=variable, value=value, -c(Lake, Date, Statistic, GeoType))
head(b)
  
# Semivariance and correlation outputs
# moran_ncf_alllakes <- readRDS( file='Data/moran_ncf_alllakes.rds')
semivar_alllakes <- readRDS(file='Data/semivar_alllakes.rds')
head(semivar_alllakes)
semivar_alllakes[which(semivar_alllakes[,c('range_best')]==Inf), c('range_best')]<-semivar_alllakes[which(semivar_alllakes[,c('range_best')]==Inf), c('cutoff')]

names(semivar_alllakes)[c(9,10)]<-c('SemiRange', 'SemiCutoff')

semivar_alllakes$SemiRangeOverCutoff <- semivar_alllakes$SemiRange/semivar_alllakes$SemiCutoff

c <- semivar_alllakes %>%
  dplyr::select(variable, Date, Lake, SemiRange, SemiCutoff, SemiRangeOverCutoff) %>%
  filter(variable != 'NA') %>%
  gather(key=Statistic, value=value, -c(Lake, Date, variable))

c$GeoType<-rep('points', nrow(c))

d<-full_join(b,c)

saveRDS(d, file='Data/FlameStatsAll.rds')

