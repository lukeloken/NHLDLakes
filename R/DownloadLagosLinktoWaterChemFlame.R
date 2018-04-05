# Code to download LAGOS information
# and link with WaterChemistry/Flame Middle

rm(list = ls())

# library(devtools)
# install_github('lawinslow/hydrolinks')
library(hydrolinks)
library(LAGOSNE)
library(dplyr)
library(sp)
library(rgdal)
library(raster)

setwd("E:/Git_Repo/NHLDLakes")

#### Load Flame and WaterChem Data ####
ChemData <- readRDS(file='Data/NHLDLakes_WaterChem_middle.rds')
ChemData$flame_name<-ChemData$LakeName

#### Load NHD IDs ####
nhdIDs<-read.csv('E:/Dropbox/FLAME_NHLDLakes/Lake Information/NHD_IDs.csv', header=T, stringsAsFactors = F)
nhdIDs<-nhdIDs[nhdIDs$FlameName!='TurtleFlambeauFlowage',]
LakeIDs<-nhdIDs$Permanent.Identifier

mylakes<-readOGR("Data/GISData", "regional_lakes_6", verbose=F)
mylakes<-spTransform(mylakes,  CRS("+init=epsg:4326"))
mylakes$WBIC<-as.numeric(as.character(mylakes$WBIC))
mylakes$SHAIDNAME<-as.character(mylakes$SHAIDNAME)

mylakes$WBIC[mylakes$SHAIDNAME=='Tomahawk Lake']<-1542700
mylakes$WBIC[mylakes$SHAIDNAME=='Rainbow Lake']<-2310800


#Get Lake Order from Latkza
Latzka<-read.csv('Data/AL_WIlakecharacteristics.csv', header=T)

mylakes$lake_order <- Latzka$landscapeposition[match(mylakes$WBIC, Latzka$WBIC)]


lakeomits<-c('Camp Lake', 'Stone Lake', 'Fawn Lake', 'Turtle Flambeau Flowage', 'Big Lake', 'Averill Lake', 'Presque Isle Lake')
mylakes<-mylakes[-which(mylakes$SHAIDNAME %in% lakeomits),]
coords<-coordinates(mylakes)
coords[which(mylakes$SHAIDNAME=='Tomahawk Lake'),] <- c(-89.67,45.83)
coords[which(mylakes$SHAIDNAME=='Little St Germain Lake'),] <- c(-89.445,45.927)
coords[which(mylakes$SHAIDNAME=='Day Lake'),] <- c(-89.7,46.063)

waterbodyid<-link_to_waterbodies(lats=coords[,2], lons=coords[,1], ids=1:length(mylakes), dataset='nhdh')

MissingLakerows<-setdiff( 1:length(mylakes), c(waterbodyid$MATCH_ID))
print(as.character(mylakes$SHAIDNAME[MissingLakerows]))

#Change Lake Tomahawk to Match lagos id
waterbodyid$permanent_[which(waterbodyid$gnis_name=='Tomahawk Lake')]<-nhdIDs$Permanent.Identifier[which(nhdIDs$FlameName=='LakeTomahawk')]


mylakedata<-mylakes@data

#Manually include lakes by lat/long and give them a name
#waterbodyid2<-link_to_waterbodies(c(45.83, 45.929, 46.063), c(-89.67, -89.44,-89.7), id=c('Tom', 'LSG', 'Day'))

mylakedata$Permanent.Identifier<-NA
mylakedata$Permanent.Identifier[waterbodyid$MATCH_ID]<-as.numeric(waterbodyid$permanent_)
mylakedata$waterbodyname[waterbodyid$MATCH_ID]<-waterbodyid$gnis_name

FullIds<-full_join(mylakedata, nhdIDs, by = "Permanent.Identifier")
FullIds$SDI<-FullIds$PERIMETER/(2*sqrt(pi*FullIds$AREA))
dim(FullIds)
# View(FullIds)



#### LAGOS data using nhd ids ####

# lagos_all<-lagosne_get("1.087.1")
lagos<-lagosne_load("1.087.1")


# Subset Lagos using IDs and change class of columns
# mylocus <- lagos$locus %>% filter(as.character(nhdid) %in% LakeIDs)
mylocus <- lagos$locus %>% filter(as.character(nhdid) %in% FullIds$Permanent.Identifier)
mylocus$nhdid<-as.character(mylocus$nhdid)
mylocus$gnis_name<-as.character(mylocus$gnis_name)
mylocus$flame_name<-nhdIDs[match(mylocus$nhdid,nhdIDs$Permanent.Identifier),2]

# mylocus$ShorelineIndex <- mylocus$lake_perim_meters/(2*sqrt(mylocus$lake_area_ha*10000))

#Replace area and perimeter with shapefile metrics and calculate shoreline development index
mylocus$lake_area_ha <- FullIds$AREA[match(mylocus$nhdid,FullIds$Permanent.Identifier)]/10000
mylocus$lake_perim_meters <- FullIds$PERIMETER[match(mylocus$nhdid,FullIds$Permanent.Identifier)]
mylocus$ShorelineIndex <- FullIds$SDI[match(mylocus$nhdid,FullIds$Permanent.Identifier)]
mylocus$LakeOrder <-  FullIds$lake_order[match(mylocus$nhdid,FullIds$Permanent.Identifier)]

# Add additional tables to mylagos datatable
mylagos <- left_join(mylocus, lagos$lakes_limno)
mylagos$lagosname1 <- NA
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
  summarise_all(funs(mean, median), na.rm=T)
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

saveRDS(mylagos2, file='Data/MyLakesLagos.rds')

# Merge lagos to LTER water chemistry

MyChemLagos <- full_join(ChemData, mylagos2)

saveRDS(MyChemLagos, file='Data/MyLakesLagosFlameChem.rds')

