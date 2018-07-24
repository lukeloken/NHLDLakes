
library(tidyr)
library(dplyr)
library(sp)
library(rgdal, verbose=F)
library(raster,verbose = T)
library(geoknife)
library(FedData)
library(prettymapr)
library(maps)

small_nlcd<-get_nlcd(template=nhld, label='nhld')

mylakes<-readOGR("Data/GISData", "regional_lakes_6", verbose=F)
mylakes<-spTransform(mylakes,  crs(small_nlcd))

mylakes<-mylakes[-which(mylakes$Lake_Name %in% c('Turtle Flambeau Flowage', 'Stone Lake', 'Fawn Lake', 'Camp Lake', 'Big Lake', 'Round Lake', 'Papoose Lake', 'Averill Lake', 'Presque Isle Lake', 'Newman Lake', 'Midge Lake', 'Tuesday Lake', 'Peter Lake', 'Paul Lake')),]


nhld<-readOGR("Data/GISData", "nhld_boundary", verbose=F)
nhld<-spTransform(nhld,  crs(small_nlcd))

states<-readOGR("Data/GISData", "StateOutlines", verbose=F)
states<-spTransform(states,  crs(small_nlcd))
WI<-states[states$NAME=='Wisconsin',]
MI<-states[states$NAME=='Michigan',]

#Subset UP from Lake Michigan polygon
UP_Poly<-MI@polygons[[1]]@Polygons[[6]]
UP_Poly2<-Polygons(list(UP_Poly), "s1")
UP<-SpatialPolygons(list(UP_Poly2), proj4string=crs(MI))

# plot(small_nlcd)
# plot(nhld, add=T, col=NA, border='magenta', lwd=3)

#mask raster by nhld polygon
nlcd_masked <- mask(small_nlcd, nhld, inverse=F)

#Save first color of raster
firstcolor<-nlcd_masked@legend@colortable[1]
nlcd_masked@legend@colortable[1]<-firstcolor

#Make a short legend color table
short_colors<-c('#476BA0', "#BAD8EA", "#68AA63", '#ED0000', "#DBD83D", "#CCBA7C")
short_labels<-c('Open Water', 'Wetland', 'Forest', 'Developed', 'Agriculture', 'Other')
short_legend<-data.frame(short_labels, short_colors, stringsAsFactors = F)

#Change colors of raster
nlcd_masked@legend@colortable[1]<-"#ffffff"
nlcd_masked@legend@colortable[1]<-NA
nlcd_masked@legend@colortable[11+1]<-short_colors[1]
nlcd_masked@legend@colortable[21:24+1]<-short_colors[4]
nlcd_masked@legend@colortable[41:43+1]<-short_colors[3]
nlcd_masked@legend@colortable[c(31,52,71)+1]<-short_colors[6]
nlcd_masked@legend@colortable[81:82+1]<-short_colors[5]
nlcd_masked@legend@colortable[c(90,95)+1]<-short_colors[2]


# plot(nlcd_masked)
# plot(nhld, add=T, col=NA, border='black', lwd=3)


#Make legend for plotting
classes<-unique(as.character(nlcd_masked@data@attributes[[1]]$NLCD.2011.Land.Cover.Class))
classes<-classes[-which(classes %in% c('Unclassified', '', "Perennial Snow/Ice"))]

IDs<-nlcd_masked@data@attributes[[1]]$ID[match(classes, nlcd_masked@data@attributes[[1]]$NLCD.2011.Land.Cover.Class)]

colors<-nlcd_masked@legend@colortable[IDs+1]

legendtable<-data.frame(IDs, classes, colors, stringsAsFactors=F)



#Generate percent of each land cover
Vals_nlcdmasked = as.data.frame(freq(nlcd_masked))

# Total counts
total = Vals_nlcdmasked %>% dplyr::filter(value >= 1) %>%
  summarise_at('count',sum)

# Total forest counts
forest = Vals_nlcdmasked %>% dplyr::filter(value >= 41 & value <= 43) %>%
  summarise_at('count',sum)
  
# Total forest counts
wetland = Vals_nlcdmasked %>% dplyr::filter(value >= 90 & value <= 95) %>%
              summarise_at('count',sum)

# Total water counts
water = Vals_nlcdmasked %>% dplyr::filter(value == 11) %>%
  summarise_at('count',sum)

perWater = water/total
perForest = forest/total
perWetland = wetland/total
perOther = 1-c(water+forest+wetland)/total

#Map of NHLD lakes with Wisconsin inset
png("Figures/NHLDmap.png", res=200, width=6.5,height=5, units="in")

par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
par(mgp=c(3,.5,0), tck=0)
lakecolor<-'mediumpurple4'

plot(nhld, col=NA, border='black', lwd=4)
plot(WI, add=T, lwd=2, lty=1)
plot(nlcd_masked, add=T)
plot(mylakes, add=T, col=lakecolor, border='black', lwd=1)

addnortharrow(pos='bottomleft', padin=c(1.6,.5), scale=.6)
addscalebar(plotunit = 'm', widthhint=.2, htin=0.1, pos='bottomleft', padin=c(1.2,.15), style='ticks')

legend('bottomleft', inset=0.01, ncol=1, c('Sampled Lakes', short_legend$short_labels), col=c('black', short_legend$short_colors), pt.bg = lakecolor, cex=0.8, pt.cex=2, pch=c(22, rep(15, length(short_legend$short_labels))), y.intersp=1.2, bty='n')

legend('topleft', inset=-.01, ncol=2, c("Wisconsin", "Michigan"), bty='n', cex=0.8, x.intersp=0)


# map("state", region="wisconsin", fill=T, add=T, col='lightgrey', projection=crs(nhld))

usr<-par('usr')
xdiff<-diff(usr[1:2])
ydiff<-diff(usr[3:4])
scaler=12
xshift<-(-1280000)
yshift<-(720000)
# Inset map
# par(usr=c(-132, -86, 42, 64))
# par(usr=c(137064.5,  759036.3, 2022391.0, 3114389.4))
par(usr=c(xshift+usr[1]-xdiff*scaler, xshift+usr[2]+xdiff*scaler, yshift+usr[3]-ydiff*scaler,yshift+usr[4]+ydiff*scaler))
# rect(xleft =-100,ybottom = 23.8,xright = -65.5,ytop = 50.6,col = "white")
plot(WI, add=T,  col='lightgrey')
plot(UP, add=T, col='lightgrey')
plot(nhld, add=T, border='black', lwd=3)

box(which='plot')

dev.off()
