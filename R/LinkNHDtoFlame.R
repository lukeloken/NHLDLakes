# Code to loop through NHD datasets and identify lake ids.
# And download LAGOS information

library(devtools)
install_github('lawinslow/hydrolinks')
library(hydrolinks)
library(LAGOSNE)
library(dplyr)

setwd("E:/Git_Repo/NHLDLakes")

#### Load Flame and WaterChem Data ####
FlameData <- readRDS(file='Data/NHLDLakes_WaterChemFLAME.rds')
ChemData <- readRDS(file='Data/NHLDLakes_WaterChem_middle.rds')
str(FlameData)
lakenames <- unique(FlameData $LakeName)

#### Load NHD IDs ####
nhdIDs<-read.csv('E:/Dropbox/FLAME_NHLDLakes/Lake Information/NHD_IDs.csv', header=T, stringsAsFactors = F)
nhdIDs<-nhdIDs[nhdIDs$FlameName!='TurtleFlambeauFlowage',]
LakeIDs<-nhdIDs$Permanent.Identifier

#### LAGOS data using nhd ids ####

# lagos_all<-lagosne_get("1.087.1")
lagos<-lagosne_load("1.087.1")

# Subset Lagos using IDs and change class of columns
mylocus <- lagos$locus %>% filter(as.character(nhdid) %in% LakeIDs)
mylocus$nhdid<-as.character(mylocus$nhdid)
mylocus$gnis_name<-as.character(mylocus$gnis_name)
mylocus$flame_name<-nhdIDs[match(mylocus$nhdid,nhdIDs$Permanent.Identifier),2]

# Add additional tables to mylagos datatable
mylagos <- left_join(mylocus, lagos$lakes_limno)
mylagos <- left_join(mylagos, lagos$lakes.geo)
mylagos <- left_join(mylagos, lagos$iws)
mylagos <- left_join(mylagos, lagos$iws.conn)
mylagos <- left_join(mylagos, lagos$iws.lulc)
mylagos <- left_join(mylagos, lagos$buffer100m.lulc)
mylagos <- left_join(mylagos, lagos$buffer500m.lulc)

#Make a table of mean water quality (nutrients, secchi, chemistry)
mynuts <- left_join(mylocus, lagos$epi_nutr)
nuts_summary <-
  mynuts %>%
  group_by(lagoslakeid) %>%
  dplyr::select(chla, colora, colort, dkn, doc, nh4, no2, no2no3, srp, tdn, tdp, tkn, tn, toc, ton, tp) %>%
  summarise_all(funs(mean), na.rm=T)
str(nuts_summary)

mysecchi <- left_join(mylocus, lagos$secchi)
secchi_summary <-
  mysecchi %>%
  group_by(lagoslakeid) %>%
  dplyr::select(secchi) %>%
  summarise_all(funs(mean), na.rm=T)

wq_summary<-left_join(nuts_summary, secchi_summary)

#Merge water quality with lagosGeo tables
mylagos2<-left_join(wq_summary, mylagos)

setwd("E:/Git_Repo/NHLDLakes")
saveRDS(mylagos2, file='Data/MyLakesLagos.rds')

