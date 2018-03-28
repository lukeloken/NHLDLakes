# ===================================
# Variance aportioning using lme4 package
# Load all NHLD Flame data
# ===================================

rm(list = ls())

library(spatstat)
library(gstat)
library(sp)
library(rgdal)
library(geoR)
library(raster)
library(rgeos)
library(lme4)
library(plyr)


# Set working directory to root data directory
setwd("E:/Dropbox/FLAME_NHLDLakes/")
filenames <- list.files(path = paste(getwd(), "/Data", sep=""))

# subset filenames for new lakes
filenames<-filenames[-grep('2014', filenames)]
filenames<-filenames[-grep('TenderfootCreek', filenames)]
# filenames<-filenames[c(17,43,54)]


# What is the spatial tollerance of points? 
# Observations within this distance (m) are excluded. 
# Only the first obseration is retained. 
maxdist<-5

# Set UTM projection (Wisconsin Transvere Mercator for Regional Lakes)
projection = "+init=epsg:3071"
# projection = "+proj=utm +zone=15 ellps=WGS84"


# ######################################
# Loop through lakes and bind them
# ######################################

# Start loop for each filename
data_list<-list()
lake_day=filenames[1]
for (lake_day in filenames){
  day_number<-which(filenames==lake_day )
  setwd(paste(getwd(),'/Data/', lake_day, sep=""))
  # get correct shapefile
  shapenames<-list.files(path=paste(getwd(), '/shapefiles', sep=""))
  file_number<-grep(pattern='cleaned.shp', shapenames)
  exact_name<-shapenames[file_number]
  short_name<-sub('.shp', '', exact_name )
  data<-readOGR('shapefiles', short_name)
  
  # Transform to correct projection
  data<-spTransform(data, CRS(projection))
  data2<-remove.duplicates(data, zero=maxdist)
  data2$LakeDay<-lake_day
  
  data_list[[day_number]]<-data2@data
  setwd("..")
  setwd("..")
}

df <- ldply(data_list, data.frame)
names(df)

dim(df)

setwd("E:/Git_Repo/NHLDLakes")
saveRDS(df, file='Data/FlameAllLakesBoundPoints.rds')

