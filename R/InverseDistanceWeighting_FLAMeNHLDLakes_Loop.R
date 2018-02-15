# ===================================
# Start Inverse Distance Modelling HERE
# Load all NHLD Flame data and predict each variable across each lake
# Export a Summary table and spatial objects of predictions
# Use lake polygon and extent of data to inform prediction window and resolution
# Luke Loken, Feb 2016
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

#Load NHLD outline
setwd("E:/Git_Repo/NHLDLakes")
NHLD_Outline<-readOGR("Data/GISData", "nhld_boundary")

# Set working directory to basemap location
setwd("E:/Dropbox/FLAME/basemaps")
lakes_Base_UTM<-readOGR(getwd(), "regional_lakes_6", stringsAsFactors = F)

# Set working directory to root data directory
setwd("E:/Dropbox/FLAME_NHLDLakes/")
filenames <- list.files(path = paste(getwd(), "/Data", sep=""))

# subset filenames for new lakes
filenames<-filenames[-grep('2014', filenames)]

# How much data to subset (data/subset)
# subset = 1; keep all data
# subset = 10: keep 1/10 of data
# subset = 20; keep 1/20 of data
subset = 10

# Set UTM projection (Zone 15 for Northern Wisconsin Regional Lakes)

projection = "+init=epsg:3071"

# projection = "+proj=utm +zone=15 ellps=WGS84"

# Transform lakebase and outline into UTM's. This way distance is in meters (m)
lakes_Base<-spTransform(lakes_Base_UTM, CRS(projection))
NHLD_Outline<-spTransform(NHLD_Outline, CRS(projection))

# ============================================
# Loop to run through all file names
# use inverse distance weight to predict each variable
# ============================================

# Subset filenames for partial runs
# filenames<-filenames[2:100]

# Create empty vector for assessing each lake day
polygonmatch<-c(rep(NA, length(filenames)))

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
    
    # Identify relative size of system
    # This will be used to build grid for predictions
    brdydim<-lake_polygon@bbox
    xdiff<-diff(brdydim[1,])
    ydiff<-diff(brdydim[2,])
    extent<-min(xdiff,ydiff)
    pixelsize<-round(extent/50) # meters (if projection == UTM)
    pixelarea<-pixelsize^2 # meters squared (if projection == UTM)
    
    # Generate 'data cloud' based on observations
    bdry<-ripras(coordinates(data))
    
    # Convert data cloud to spatial polygon
    bdry_df<-data.frame(bdry[[4]][[1]]$x, bdry[[4]][[1]]$y)
    bdry_df[nrow(bdry_df)+1,]<-bdry_df[1,]
    bdry_poly<-Polygon(bdry_df)
    bdry_poly2 = Polygons(list(bdry_poly), "s1")
    bdry_poly_sp<-SpatialPolygons(list(bdry_poly2), proj4string=CRS(as.character(projection)))
    
    # Make Buffer around data cloud polygon 
    # width = distance in meters; currently using two pixel distances
    buffered<-gBuffer(bdry_poly_sp, width=pixelsize*2)
    
    # Make prediction area as intersection of buffered area and lake polygon
    Area<-gIntersection(buffered, lake_polygon)
    
    # Check Area and Confirm
    plot(Area)
    plot(data, add=TRUE)
    plot(bdry_poly_sp, add=TRUE)
    plot(buffered, add=TRUE, lwd=4)
    plot(lakes_Base, add=TRUE)
    
    # Make polygrid - This is each location to make predictions
    data1.grid<-polygrid(seq(brdydim[1,1], brdydim[1,2], pixelsize), seq(brdydim[2,1], brdydim[2,2], pixelsize), borders=Area@polygons[[1]]@Polygons[[1]]@coords)
    # Set names of coordinates and match projection
    coordinates(data1.grid)<-~x+y
    proj4string(data1.grid) <- CRS(projection)
    # Convert to gridded (pixels) rather than points
    gridded(data1.grid) <- TRUE
    
    # Remove pixels from islands and other polygon 'holes'
    pts_in=over(SpatialPoints(data1.grid), SpatialPolygons(lake_polygon@polygons), fn=NULL)
    data2.grid<-data1.grid[!is.na(pts_in)]
    
    # plot data grid, boundary, and observations
    plot(data2.grid, col="grey")
    plot(lake_polygon, add=TRUE, col=NA, lwd=3)
    plot(Area, add=TRUE, lwd=2, col=NA, border="blue")
    plot(data, add=TRUE, col="red", cex=0.2)
    
    # Make spatial object to save surface predictions
    grid_withData<-SpatialPixelsDataFrame(data2.grid, data=data.frame(matrix(ncol=length(variables), nrow=length(data2.grid))))
    names(grid_withData@data)<-variables
    
    # Make an empty summary table for each filename
    # This will be populated with summary stats for each variable
    summary_lake<-as.data.frame(matrix(nrow=length(variables), ncol=17))
    names(summary_lake)<-c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'sd', 'n', 'mad', 'MADM', 'skewness', 'loc', 'scale', 'shape', 'CV', 'QuartileDispersion', 'MADMOverMedian')
    
    # ==========================================
    # Start of loop to run through each variable  
    # ==========================================
    var=variables[1]
    for (var in variables){
      var_number<-which(variables==var)
      
      #Identify column that contains variable
      column1<-which(names(data)==var) 
      
      # Remove Time Variable
      data2<-data[,-1]
      
      #Identify column in data2 that contains variable
      column<-which(names(data2)==var) 
      
      # Remove NAs
      data2<-data2[is.na(data2@data[,column])==FALSE,]
      
      # Skip variable if all NAs
      if (length(data2)>0){
        
        # Plot Timeseries of variable
        # Make sure data seem reasonable
        plot(data@data[,column1], type="p")
        
        #Transform data into UTM's. This way distance is in meters (m)
        data2<-spTransform(data2, CRS(projection))
        
        #Plot heat map atop lake base polygon
        # spplot(data2[var], cuts=99, colorkey=TRUE, sp.layout = list(lakes_Base['Lake_Name']) )
        
        # subset (%) of the data. Take random percent of points
        # Depending on the analysis and size of data, R cannot handle entire dataset
        data3<-data2[sample(nrow(data2), nrow(data2)/subset), ]
        colnames(data3@coords)<-c("x", "y")
        
        # =========================
        # Using Inverse Distance Weighting predict values at each grid cell
        # idp = denominator exponent. 
        # idp = 1: 1/distance
        # idp = 2: 1/(distance squared)
        # =========================
        
        predict <- gstat::idw(data3@data[,column]~1, data3, data2.grid, idp=1)
        names(predict)<-c(paste(var, sep=""), paste(var, "_v", sep=""))
        
        # par(mfrow=c(1,1))
        # par(mar=c(4,4,4,4), oma=c(1,1,1,1))
        # spplot(predict, names(predict)[1], colorkey=TRUE, cuts=99, sp.layout=list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F) , main=paste(var, "_prediction_inverse_distance_weight", sep=""), xlim=bbox(lake_polygon)[1,], ylim=bbox(lake_polygon)[2,])
        
        # Create summary stats for variable
        summary(predict)
        values<-predict@data[,1]
        
        #if zero heterogeneity exists, skip evd and plotting
        if (identical(round(min(values),3), round(max(values), 3))){
          
          summary_var<-c(summary(values), sd=sd(values), n=length(values), mad=mad(values), MADM=median(abs(values-median(values))), skewness=skewness(values, na.rm = T), rep(NA,3))
          
          # Save summary info to summary table
          summary_lake[var_number,1:14]<-summary_var
        } else {
          
          evd<-fgev(values, std.err=F)
          
          summary_var<-c(summary(values), sd=sd(values), n=length(values), mad=mad(values), MADM=median(abs(values-median(values))), skewness=skewness(values, na.rm = T), evd$estimate)
          hist(predict@data[,1],breaks=20, xlab=var, main="", col="grey")
          
          # Save summary info to summary table
          summary_lake[var_number,1:14]<-summary_var
          
          # Save spatial data to spatial object
          grid_withData@data[var_number]<-predict@data[1]
          
          # reate subfolder 'maps_idw' if it does not already exist
          folders<-list.files(path = paste(getwd(), "", sep=""))
          if(length(folders[folders=="maps_idw"])==0){
            dir.create(paste(getwd(), "/maps_idw", sep=""))}
          
          # Plot Spatial data
          png(paste(getwd(), "/maps_idw/", var,lake_day, ".png", sep=""), res=200, width=6,height=6, units="in")
          xdist <- diff(bbox(lake_polygon)[1,1:2])
          scale <- signif(xdist/10, digits=1)
          # polyx<-c(bbox(lake_polygon)[1,1]+scale*(c(0.2,1.2)))
          # polyy<-c(bbox(lake_polygon)[2,1]+scale*c(.2,.4))
          # coords<-data.frame(x=c(polyx, rev(polyx)), y=c(rep(polyy[1], 2), rep(polyy[2], 2)))
          # poly_box<-Polygon(coords)
          # poly_box2<-Polygons(list(poly_box), "s1")
          # poly_box_sp<-SpatialPolygons(list(poly_box2), proj4string=CRS(as.character(projection)))
          # 
          polyx<-c(bbox(lake_polygon)[1,1]+scale*(c(0.2,1.2)))
          polyy<-c(bbox(lake_polygon)[2,1]+scale*c(.2,.4))
          coords<-data.frame(x=c(rep(polyx[1], 2), rep(polyx[2], 2)), y=c(rev(polyy), polyy))
          poly_line<-Line((coords))
          S1 = Lines(list(poly_line), ID="a")
          poly_line_sp<- SpatialLines(list(S1))
          
          l1 = list(lake_polygon['Lake_Name'], col=1, fill=0, lwd=3, lty=1, first=F)
          l2 = list("SpatialPolygonsRescale", layout.north.arrow(type=1), offset = 
                      c(bbox(lake_polygon)[1,1]+.25*scale, bbox(lake_polygon)[2,2]-1.1*scale),
                      scale = scale*1, first=FALSE) 
          l3<- list(poly_line_sp, fill=NA, lwd=2, lty=1, first=F)
          # l3<- list(poly_box_sp, fill=NA, lwd=2, lty=1, first=F)
# mean(polyx), mean(polyy)
          # l3 = list("SpatialPolygonsRescale", layout.scale.bar(height=scale/1000), offset = 
          #             c(bbox(lake_polygon)[1,1]+0.5*scale,bbox(lake_polygon)[2,1]+scale),
          #             scale = scale, fill=c('black'), lwd=1, first=FALSE) 
          l4 = list("sp.text", c(mean(polyx), polyy[1]),
                    paste0(scale, "m"), cex=0.6, first=FALSE, pos=3) 
          
          print(spplot(grid_withData, zcol=var, colorkey=TRUE, cuts=99, sp.layout=list(l1, l2, l3, l4) , main=paste(var, "_prediction_inverse_distance_weight", sep=""), xlim=bbox(lake_polygon)[1,], ylim=bbox(lake_polygon)[2,]))
          dev.off()
          closeAllConnections()
        }
      }  
    }
    
    # Add variable names to summary table
    summary_lake$CV<-summary_lake$sd/summary_lake$Mean
    summary_lake$QuartileDispersion<-(summary_lake$Q3 - summary_lake$Q1)/ (summary_lake$Q3 + summary_lake$Q1)
    summary_lake$MADMOverMedian<-(summary_lake$MADM)/ (summary_lake$Median)
    
    summary_lake$Variable<-variables
    
    
    # Save shapefile of interpolated surface (spatial pixels data frame)
    writeOGR(grid_withData, dsn=paste(getwd(), "/shapefiles_idw", sep="") ,layer=short_name, driver="ESRI Shapefile",  verbose=F, overwrite=T)
    
    # Convert spatialpixesldataframe to raster
    raster_withData <- stack(grid_withData)
    
    # Save raster of interpolated surface (stacked raster)
    # Note - ArcMap cannot read this type of file
    writeRaster(raster_withData, paste(getwd(), "/shapefiles_idw/" ,short_name, "raster", sep=""), format='raster', overwrite=TRUE)
    
    #Return to root directory
    setwd('..')
    setwd('..')
    
    #Write summary to file
    write.table(summary_lake, file = as.character(paste('pixelsummaries/', short_name, ".csv", sep="")), col.names=TRUE,row.names=F, sep=",")
    rm(summary_lake)
  }
  
}

# Combine filenames and polygon matches
# Files with "no" did not find a lake polygon
RunFrame<-data.frame(filenames, polygonmatch)
print(RunFrame)

# write.table(RunFrame, file = as.character(paste('AllLakeSummary.csv', sep="")), col.names=TRUE,row.names=F, sep=",")

# ==============================
# End spatial interpolation code
# ==============================

