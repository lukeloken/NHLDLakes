# ===================================
# Spatial stats on raw flame data (hydro corrected)
# Load all NHLD Flame data
# Export a Summary table of spatial stats
# Luke Loken, Feb 2018
# ===================================

rm(list = ls())

library(spatstat)
library(gstat)
library(sp)
library(rgdal)
library(geoR)
library(raster)
library(rgeos)
library(moments)
library(evd)

# Set working directory to basemap location
setwd("E:/Dropbox/FLAME/basemaps")
lakes_Base_UTM<-readOGR(getwd(), "regional_lakes_6", stringsAsFactors = F)

# Set working directory to root data directory
setwd("E:/Dropbox/FLAME_NHLDLakes/")
filenames <- list.files(path = paste(getwd(), "/Data", sep=""))

# subset filenames for new lakes
filenames<-filenames[-grep('2014', filenames)]
filenames<-filenames[-grep('TenderfootCreek', filenames)]
# filenames<-filenames[c(17,43,54)]

# How much data to subset (data/subset)
# subset = 1; keep all data
# subset = 10: keep 1/10 of data
# subset = 20; keep 1/20 of data
# subset = 10

# Set UTM projection (Wisconsin Transvere Mercator for Regional Lakes)
projection = "+init=epsg:3071"
# projection = "+proj=utm +zone=15 ellps=WGS84"

#Table of minimums to add to all values to get ride of negatives
minvars<-data.frame(variable=c("ChlARFU", "ChlAugL", "BGAPCRFU", "BGAPCgL", "TurbFNU", "fDOMRFU", "fDOMQSU"), minimum=c(.12, .84, .21, .19, .42, .62, 1.5)*(-1))

# Transform lakebase and outline into UTM's. This way distance is in meters (m)
lakes_Base<-spTransform(lakes_Base_UTM, CRS(projection))

# ######################################
# Loop through lakes and calculate stats
# ######################################

# Create empty vector for identifying lake polygons
polygonmatch<-c(rep(NA, length(filenames)))

# Start loop for each filename
lake_day=filenames[7]
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
  variables <- names(data)[2:length(names(data))]
  # Can also pick variable names(data)
  # variables <- c('CO2St_t', 'CO2uM_t', 'CH4St_t', 'CH4uM_t', 'TempC_t')
  
  
  # ========================
  # Find appropraite lake polygon
  # ========================
  
  # Grab 5 random points in dataset
  points_random<-data[sample(nrow(data), 5), ]
  # Find the names of the polygons that those points fall within
  polygonnames <- as.character(over(points_random, lakes_Base)$Lake_Name)
  # Find the most common polygon name
  polygonname <- names(sort(table(polygonnames),decreasing=TRUE)[1])
  
  # If no polygon found, end code and return to top
  if (length(polygonname)<1){
    polygonmatch[day_number]<-c("No")
    print("No Polygon Match")
    setwd('..')
    setwd('..')
  }
  else {
    polygonmatch[day_number]<-c("Yes")
    # Set lake polygon
    lake_polygon<-lakes_Base[lakes_Base$Lake_Name==polygonname,]
    # Display name of lake (regional lakes only!)
    print(as.character(lake_polygon$Lake_Name))
    
    # Plot data and new 'lake polygon' object
    spplot(data[3], cuts=99, colorkey=TRUE, sp.layout = list(lake_polygon['Lake_Name']) )
    
    # Make an empty summary table for each filename
    # This will be populated with summary stats for each variable
    summary_lake<-as.data.frame(matrix(nrow=length(variables), ncol=22))
    names(summary_lake)<-c('Min', 'Q25', 'Median', 'Mean', 'Q75', 'Max', 'Q05', 'Q10', 'Q90', 'Q95', 'sd', 'SDL', 'n', 'mad', 'MADM', 'skewness', 'loc', 'scale', 'shape', 'CV', 'QuartileDispersion', 'MADMOverMedian')
    
    #Subset data so observations within 5 m of each other are removed. Mainly this gets rid of the duplicates generated when the boat is stationary. 
    data2<-remove.duplicates(data, zero=5)
    
    # ==========================================
    # Start of loop to run through each variable  
    # ==========================================
    var=variables[1]
    for (var in variables){
      var_number<-which(variables==var)
      
      #Identify column that contains variable
      values<-data@data[,which(names(data)==var)]
      values<-values[which(is.na(values)==F)]
      
      # Skip variable if all NAs
      if (length(values)>0){
        
        if (var %in% minvars[,1]){
          minimum<-minvars[which(var==minvars[,1]),2]
          values_plus<-values-minimum+0.1
        } else {
          values_plus<-values
        }
        
        # Create summary stats for variable
        basic_stats<-summary(values_plus)
        quantiles<-quantile(values_plus, probs = c(0.05, .1, .9, 0.95),  na.rm = TRUE)
        
        summary_var<-c(basic_stats, quantiles, sd=sd(values_plus, na.rm=T), SDL=sd(log10(values_plus), na.rm=T), n=length(values_plus), mad=mad(values_plus), MADM=median(abs(values_plus-median(values_plus))), skewness=skewness(values_plus, na.rm = T))

        
        # Save summary info to summary table
        summary_lake[var_number,1:16]<-summary_var
        
        #If zero heterogeneity skip evd and histogram
        if (identical(round(min(values_plus), 3), round(max(values_plus,3)))==FALSE){
          # hist(values_plus,breaks=20, xlab=var, main="", col="grey")
          evd<-fgev(values_plus, std.err=F)
          evd$estimate
          summary_lake[var_number,17:19]<-evd$estimate
        }
      }
    }
    
    summary_lake$CV<-summary_lake$sd/summary_lake$Mean
    summary_lake$QuartileDispersion<-(summary_lake$Q75 - summary_lake$Q25)/ (summary_lake$Q75 + summary_lake$Q25)
    summary_lake$MADMOverMedian<-(summary_lake$MADM)/ (summary_lake$Median)
    summary_lake$MaxMinusMin<-(summary_lake$Max) - (summary_lake$Min)
    summary_lake$IQR<-(summary_lake$Q75) - (summary_lake$Q25)
    summary_lake$Q95MinusQ05<-(summary_lake$Q95) - (summary_lake$Q05)
    
    summary_lake$Variable<-variables
    
    #Return to root directory
    setwd('..')
    setwd('..')
    
    #Write summary to file
    write.table(summary_lake, file = as.character(paste('pointsummaries/', short_name, ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
    rm(summary_lake)
    
  }
}

RunFrame<-data.frame(filenames, polygonmatch)
print(RunFrame)



