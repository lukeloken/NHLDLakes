# Code to loop through NHD datasets and identify lake ids.
# And download LAGOS information

library(devtools)
install_github('lawinslow/hydrolinks')
library(hydrolinks)
library(LAGOSNE)
library(dplyr)

#### Load Flame and WaterChem Data ####
FlameData <- readRDS(file='Data/NHLDLakes_WaterChemFLAME.rds')
str(FlameData)
lakenames <- unique(FlameData $LakeName)

#### Load NHD IDs ####
nhdIDs<-read.csv('E:/Dropbox/FLAME_NHLDLakes/Lake Information/NHD_IDs.csv', header=T, stringsAsFactors = F)
LakeIDs<-nhdIDs$Permanent.Identifier

#### LAGOS data using nhd ids ####

# lagos_all<-lagosne_get("1.087.1")
lagos<-lagosne_load("1.087.1")

# Subset Lagos using IDs and change class of columns
mylagos <- lagos$locus %>% filter(as.character(nhdid) %in% LakeIDs)
mylagos$nhdid<-as.character(mylagos$nhdid)
mylagos$gnis_name<-as.character(mylagos$gnis_name)

# Add additional tables to mylagos datatable
mylagos <- left_join(mylagos, lagos$lakes_limno)
mylagos <- left_join(mylagos, lagos$lakes.geo)
mylagos <- left_join(mylagos, lagos$iws)
mylagos <- left_join(mylagos, lagos$iws.conn)
mylagos <- left_join(mylagos, lagos$iws.lulc)
mylagos <- left_join(mylagos, lagos$buffer100m.lulc)
mylagos <- left_join(mylagos, lagos$buffer500m.lulc)

saveRDS(mylagos, file='Data/MyLakesLagos.rds')
