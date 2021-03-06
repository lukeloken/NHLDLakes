

library(spatstat)
library(gstat)
library(sp)
library(rgdal)
library(geoR)
library(raster)
library(rgeos)
library(maptools)
library(riverdist)
library(gstat)

source('R/VariogramFunctions.R')

#Code to calcualte semivariance for CH4 in Wingra Creek

flame_dir<-"C:/Dropbox/FLAME_WisconsinRivers/Data/2017-10-06_WingraCreek"
flame_data<-readOGR("C:/Dropbox/FLAME_WisconsinRivers/Data/2017-10-06_WingraCreek/ProcessedData", "2017-10-06_WingraCreek_07_ShapefileCleaned" )

flame_data$DateTime<-as.POSIXct(flame_data$dt_tm, format='%Y-%m-%d %H:%M:%S', tz='UTC')
WingraCreekLine<-readOGR("C:/Dropbox/ArcGIS", "WingraCreekFlowLine")

# plot(WingraCreekLine)
# 
# plot(flame_data, add=T)

projection = "+init=epsg:3071"

# projection = "+proj=utm +zone=15 ellps=WGS84"

# Transform lakebase and outline into UTM's. This way distance is in meters (m)
WingraCreekLine_UTM<-spTransform(WingraCreekLine, CRS(projection))
flame_data_UTM<-spTransform(flame_data, CRS(projection))
# 
# flame_snapped<-snapPointsToLines(flame_data_UTM, WingraCreekLine_UTM, maxDist = 100, withAttrs=T)
# 
# plot(flame_snapped)
# plot(WingraCreekLine_UTM, add=T, col='blue', lwd=3)
# plot(flame_data_UTM, col='red', pch=5, add=T, cex=0.5)


# convert to linear network. 
# Use 1 m as the resolution!
WingraNetwork<-line2network("C:/Dropbox/ArcGIS", "WingraCreekFlowLine", reproject=projection)
WingraNetwork_clean<-cleanup(WingraNetwork)


plot(WingraNetwork_clean)
str(WingraNetwork_clean)

flame_snapped<-xy2segvert(x=coordinates(flame_data_UTM)[,1], y=coordinates(flame_data_UTM)[,2], rivers=WingraNetwork_clean)

str(flame_snapped)

hist(flame_snapped$snapdist)

flame_data_UTM$Dist<-flame_snapped$vert

plot(flame_data_UTM$Dist, flame_data_UTM$CH4)



#Calculate Semivariance

data1<-flame_data_UTM@data
data1$x<-data1$Dist
data1$y<-rep(1, nrow(data1))

coordinates(data1) <- ~x+y
str(data1)

#Use only first half of data
TurnTime<-as.POSIXct("2017-10-06 17:05:00", format='%Y-%m-%d %H:%M:%S', tz='UTC')
data2<-data1[which(data1$DateTime<=TurnTime),]

#Variogram parameters
# cutoff=3000
width=10
subset = 10
diff   <- diff(range(data2$Dist, na.rm=T))
# window <-diff/100
window=10
cutoff <- diff/2

#CH4 column

column<-53
for (column in 1:ncol(data2@data)){
  if (is.numeric(data2@data[,column])==TRUE){
    var<-names(data2@data)[column]

    data3<-data2[is.na(data2@data[,column])==FALSE,]
    if (nrow(data3@data)>1){
    data4<-data3[sample(nrow(data3), nrow(data3)/subset), ]


# data4<-data1[is.na(data1@data[,column])==FALSE,]

v = variogram(data4@data[,column]~1, data4, cutoff=cutoff, width=window)

# Guess sill, range, and nugget
# These help the fit.variogram function figure out the best model
est_sill<-median(v$gamma)
est_range<-2000/4
est_nugget<-v$gamma[1]

#fit model to variogram
v.fit <- fit.variogram(v, vgm(est_sill=est_sill, c("Nug", "Lin", "Sph", "Gau", "Exp"), est_range=est_range, nugget=est_nugget), fit.method = 1)
# v.fit <- fit.variogram(v, vgm(est_sill=est_sill, c("Sph"), est_range=est_range, nugget=est_nugget), fit.method = 1)
v.fit_Sph <- fit.variogram(v, vgm(est_sill=est_sill, c("Sph"), est_range=est_range, nugget=est_nugget), fit.method = 1)


# v.fit <- fit.variogram(v, vgm(c("Nug", "Lin", "Sph", "Gau", "Exp")), fit.method = 1)

if(min(v.fit$range, na.rm=T)>=0){
# v.fit <- fit.variogram(v, vgm(c('Lin')), fit.method = 1)

#Ouput model information
model_type    <- as.character(tail(v.fit[,1],1))

#Both of these become NA if nug model
model_psill <- v.fit$psill[2]
model_range <- v.fit$range[2]

model_nug <- v.fit$psill[1]
model_nugrange <- v.fit$range[1]
range_best <- model_range

if(model_type %in% c("Lin", "Sph")){
  range95 <- EffectiveRange(v.fit, window)
  if (range95>=cutoff){range95 <- Inf}
  if (range_best>=cutoff){range_best<-Inf}
} else if (model_type %in% c("Nug")) {
  range95 <- NA
range_best <- model_nugrange
} else if (model_type %in% c("Exp", "Gau")){
  range95 <- EffectiveRange(v.fit, window)
  range_best<-range95
  if (range_best>=cutoff){range_best<-Inf}
}



#Semivariogram
png(paste0(flame_dir, "/semivariograms/", var,".png"), res=200, width=5,height=4, units="in")
par(mar=c(3.5,3.5,0.5,0.5), tck=0.02)
par(mgp=c(2,.4,0))

vplot(v, v.fit, col='cornflowerblue', pch=16, lwd=2, xlab='', ylab='', xlim=c(0,cutoff))

mtext('Distance (m)', 1, 2)
mtext('Semivariance', 2, 2)
legend('top', inset=0.01, names(data4)[column], bty='n')
legend('bottomright', inset=0.01, c(as.character(paste0("Model = ", model_type)), paste0('Range = ', signif(range_best, 3))), bty='n')

dev.off()

}

}

  }
}
