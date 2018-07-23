# ############################################################################
# Correlelogram modelling
# Load all NHLD Flame data and evaluate spatial correlation
# Luke Loken, Feb 2018

rm(list = ls())

library(sp)
library(ncf)
library(rgdal)
library(gstat)
library(geoR)
library(lubridate)
library(gtools)

setwd("E:/Git_Repo/NHLDLakes")
source('R/VariogramFunctions.R')

# Set working directory to basemap location
setwd("E:/Dropbox/FLAME/basemaps")
lakes_Base_UTM<-readOGR(getwd(), "regional_lakes_6", stringsAsFactors = F)

# Set working directory to root data directory
setwd("E:/Dropbox/FLAME_NHLDLakes/")
filenames <- list.files(path = paste(getwd(), "/Data", sep=""))

# subset filenames for new lakes
filenames <- filenames[-grep('2014', filenames)]
filenames <- filenames[-which(filenames=="2015-08-12_RainbowLake")]


# Set UTM projection (Wisconsin Transvere Mercator for Regional Lakes)
projection <- "+init=epsg:3071"
# projection = "+proj=utm +zone=15 ellps=WGS84"

# Set percentage of total data you want to use for correlogram analysis
maxobs <- 1200

# Set window size and resample number for which to compute correlation coefficients
window <- 10
resamp <- 25

#Set alpha for moran i testing
p_threshold <- 0.05

# Transform lakebase and outline into UTM's. This way distance is in meters (m)
lakes_Base<-spTransform(lakes_Base_UTM, CRS(projection))

# ######################################
# Loop through lakes and calculate spatial correlation
# ######################################

# Lists to populate with correlogram and semivariogram stats
moran_ncf_alllakes<-list()
semivar_alllakes<-data.frame(matrix(nrow=0, ncol=10))
names(semivar_alllakes)<-c('lake_day', 'variable', 'model_type', 'model_psill', 'model_range', 'model_nug', 'model_nugrange', 'range95', 'range_best', 'cutoff')

# add data to existing files 
# moran_ncf_alllakes<-readRDS(file='SpatialOutputs/moran_ncf_alllakes.rds')
# semivar_alllakes<-readRDS(file='SpatialOutputs/semivar_alllakes.rds')
# donelakes <- unique(semivar_alllakes$lake_day)
# filenames <- filenames[-which(filenames %in% donelakes)]

# Start loop for each filename
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
  
  # Set list of variables to run through (omit time which is column 1)
  variables_all <- names(data)[2:length(names(data))]
  variables <- variables_all[grep('_h', variables_all)]
  badvars <- variables[which(variables %in% c("XCO2Dppm_h", "XCH4Dppm_h", "CH4St_h", "CO2St_h"))]
  if (length(badvars)>0){variables <- variables[-which(variables %in% badvars)]}
  variable_names<-paste0(sub('\\_h.*', '', variables), '_h')
  
  #add SUNA vars if they exist
  sunavars<-variables_all[which(variables_all %in% c('ABS254', 'ABS350', 'NITRATEM', 'NITRATEU'))]
  if (length(sunavars)>0){
    variables<-c(variables, sunavars)
    variable_names<-c(variable_names, sunavars)
  }
  
  # subset (%) of the data # pgirmess cannot handle dataset more than ~1000 obs)
  data2<-data[sample(nrow(data@data), min(nrow(data@data), maxobs)), ]
  
  #Determine extent and set window size so bins are every 3 meters
  coords = cbind(data2@coords[ ,1], data2@coords[ ,2])
  coordsmatrix <- as.matrix(coords)
  dist   <- dist(coordsmatrix)
  range  <- range(dist)
  diff   <-range[2]-range[1]
  window <-diff/100
  cutoff <- diff/2

  # create subfolder 'correlograms' if it does not already exist
  folders<-list.files(path = paste(getwd(), "", sep=""))
  if(length(folders[folders=="correlograms"])==0){
    dir.create(paste(getwd(), "/correlograms", sep=""))}
  if(length(folders[folders=="semivariograms"])==0){
    dir.create(paste(getwd(), "/semivariograms", sep=""))}
  
  # ==========================================
  # Start of loop to run through each variable  
  # ==========================================
  
  #make lists for correlation
  moran_ncf<-list()
  semivar<-data.frame(matrix(ncol=10, nrow=length(variables)))
  names(semivar)<-c('lake_day', 'variable', 'model_type', 'model_psill', 'model_range', 'model_nug', 'model_nugrange', 'range95', 'range_best', 'cutoff')
  semivar$lake_day<-lake_day
  
  # Loop through variables
  var=variables[1]
  for (var in variables){
    var_number<-which(variables==var)
    
    #Identify column in data2 that contains variable
    column<-which(names(data2)==var) 
    
    # Remove NAs
    data3<-data2[is.na(data2@data[,column])==FALSE,]
    
    if (nrow(data3@data)>0){
      if(identical(round(min(data3@data[,column]), 3), round(max(data3@data[,column]),3))==FALSE){
        # spplot(data3[var], cuts=99, colorkey=TRUE, sp.layout = list(lakes_Base['Lake_Name']) )
        
        # ##########################
        # calculate correlalogram
        # ncf::correlog 
        # ########################
        print(variable_names[var_number])
        fit_ncf <- ncf::correlog(x = data3@coords[,1], y = data3@coords[,2], z = data3@data[,column], increment = window, resamp=resamp, na.rm=T)
        
        # outputs of correlogram
        ncf_class     <- fit_ncf $mean.of.class
        ncf_corr      <- fit_ncf $correlation
        ncf_p         <- fit_ncf $p
        
        moran_ncf[[var_number]] <- data.frame(ncf_class, ncf_corr, ncf_p)
        names(moran_ncf)[[var_number]]<-variable_names[var_number]
        
        # ##########################
        # Calculate semivariogram
        # variogram, fit.variogram
        # ########################
        
        v = variogram(data3@data[,column]~1, data3, cutoff=cutoff, width=window)
        
        # Guess sill, range, and nugget
        # These help the fit.variogram function figure out the best model
        est_sill<-median(v$gamma)
        est_range<-cutoff/4
        est_nugget<-v$gamma[1]
        
        #fit model to variogram
        v.fit <- fit.variogram(v, vgm(est_sill=est_sill, c("Nug", "Lin", "Sph"), est_range=est_range, nugget=est_nugget), fit.method = 2)
        
        # v.fit <- fit.variogram(v, vgm(c('Lin')), fit.method = 1)

        #Ouput model information
        model_type    <- as.character(tail(v.fit[,1],1))
        
        #Both of these become NA if nug model
        model_psill <- v.fit$psill[2]
        model_range <- v.fit$range[2]
        
        model_nug <- v.fit$psill[1]
        model_nugrange <- v.fit$range[1]
        range_best <- model_range
        
        if(model_type != 'Nug'){
          range95 <- EffectiveRange(v.fit, window)
          if (range95>=cutoff){range95 <- Inf}
          if (range_best>=cutoff){range_best<-Inf}
        } else {range95 <- NA
        range_best <- model_nugrange}
        
        
        semivar[var_number,2:10] <- c(variable_names[var_number], model_type, model_psill, model_range, model_nug, model_nugrange, range95, range_best, cutoff)
        
        # ###################################
        # Plot correlogram and semivariogram
        # ###################################
        
        #Correlogram
        png(paste0(getwd(), "/correlograms/", var,".png"), res=200, width=5,height=4, units="in")
        par(mar=c(2.5,2.5,0.5,0.5))
        par(mgp=c(2,.2,0))
        colors<-c('darkseagreen4', 'cornflowerblue')
        
        plot(ncf_class, ncf_corr, type="o", ylim = c(-1, 1), xlim=c(0, cutoff), ylab="", xlab="", axes=FALSE, col=colors[1])
        points(ncf_class[which(ncf_p<p_threshold)], ncf_corr[which(ncf_p<p_threshold)], type="p", pch=16, col=colors[1])
        
        abline(h=0, lty=3)
        axis(1, tck = .02)
        axis(2, tck = .02, las=1)
        mtext("Distance (m)", 1, 1.5)
        mtext("Moran I", 2, 1.5)
        legend('top', inset=0.01, variable_names[var_number], bty='n')
        box(which='plot')
        
        dev.off()
        
        #Semivariogram
        png(paste0(getwd(), "/semivariograms/", var,".png"), res=200, width=5,height=4, units="in")
        par(mar=c(3.5,3.5,0.5,0.5), tck=0.02)
        par(mgp=c(2,.4,0))
        
        vplot(v, v.fit, col=colors[2], pch=16, lwd=2, xlab='', ylab='', xlim=c(0,cutoff))
        
        mtext('Distance (m)', 1, 2)
        mtext('Semivariance', 2, 2)
        legend('top', inset=0.01, variable_names[var_number], bty='n')
        legend('bottomright', inset=0.01, c(as.character(paste0("Model = ", model_type)), paste0('Range = ', signif(range_best, 3))), bty='n')
        
        dev.off()
      }
    }
  }
  
  #output data
  moran_ncf_alllakes[[day_number]]<-moran_ncf
  names(moran_ncf_alllakes)[[day_number]]<-lake_day
  
  if(nrow(semivar_alllakes)==0){semivar_alllakes<-semivar
  } else {semivar_alllakes<-smartbind(semivar_alllakes, semivar)}

  setwd("E:/Dropbox/FLAME_NHLDLakes/")
  print(lake_day)
  
}
  
semivar_alllakes[,4:10] <- apply(semivar_alllakes[,4:10], 2, as.numeric)
semivar_alllakes$Date <- ymd(substr(semivar_alllakes$lake_day, start=1, stop=10))
semivar_alllakes$Lake <- gsub(".*_","",semivar_alllakes$lake_day)
semivar_alllakes$Lake <- gsub("[0-9]","",semivar_alllakes$Lake )

setwd("E:/Dropbox/FLAME_NHLDLakes/")
#Export Data
saveRDS(moran_ncf_alllakes, file='SpatialOutputs/moran_ncf_alllakes.rds')
saveRDS(semivar_alllakes, file='SpatialOutputs/semivar_alllakes.rds')

#Export Data to git
setwd("E:/Git_Repo/NHLDLakes")
saveRDS(moran_ncf_alllakes, file='Data/moran_ncf_alllakes.rds')
saveRDS(semivar_alllakes, file='Data/semivar_alllakes.rds')

