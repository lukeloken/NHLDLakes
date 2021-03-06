
#Wide Table (one row = one lake)
MyChemLagos <- readRDS('Data/MyLakesLagosFlameChem.rds') 
names(MyChemLagos)[23]<-'Lake'

#For lakes without LTER water chemistry use lagos median
MyChemLagos$DOC[which(is.na(MyChemLagos$DOC))]<-MyChemLagos$doc_median[which(is.na(MyChemLagos$DOC))]
MyChemLagos$TotalPUF[which(is.na(MyChemLagos$TotalPUF))]<-MyChemLagos$tp_median[which(is.na(MyChemLagos$TotalPUF))]

#Without Lake Order
e<-MyChemLagos[,c(2:3,23, 14:22, 57, 62:63, 79, 75, 103, 105,191,192)]
#With Lake Order
# e<-MyChemLagos[,c(23, 14:22, 57, 62:63, 79, 75, 104, 106,192,193,76)]

#Fill in missing or eroneous lagos data
e$lakeconnection[e$Lake=='BallardLake']<-'DR_LakeStream'

e$iws_ha[e$Lake=='MidgeLake']<-10
e$iws_streamdensity_streams_sum_lengthm[e$Lake=='MidgeLake']<-0
e$iws_streamdensity_streams_density_mperha[e$Lake=='MidgeLake']<-0

#Create additional metrics describing watershed/stream attributes compared to lake size
e$streamlength_to_lakearea<-e$iws_streamdensity_streams_sum_lengthm/e$lake_area_ha
e$streamdensity_to_lakearea<-e$iws_streamdensity_streams_density_mperha/e$lake_area_ha
e$iwsarea_to_lakearea <- e$iws_ha/e$lake_area_ha


#Long Table (one row = one value), values are by lake, date, Geotype, variable, stat
d<- readRDS(file='Data/FlameStatsAll.rds')

f<-as.data.frame(right_join(e,d))

g <- f %>%
  filter(variable!='NA' | Statistic != 'NA')

h <- g[which(!is.na(g$value) & g$Date>='2015-06-08' & g$Lake !='MidgeLake' & g$Lake !='NewmanLake'),]

# h<- h[-which(h$Lake=='BigMuskellungeLake' & h$Date =='2015-08-03'),]

i <- h %>% 
  unite(col, variable, GeoType, Statistic, sep='_') 

j <- i %>%
  tidyr::spread(key=col, value=value)






#Make table for supplementary material
S1vars<-c('Lake', 'Latitude', 'Longitude', 'lake_area_ha', 'lake_perim_meters', 'ShorelineIndex', 'maxdepth', 'iws_ha', 'iws_streamdensity_streams_sum_lengthm', "lakeconnection")



goodlakes<-unique(j$Lake)

TableS1<-as.data.frame(e[S1vars])

TableS1$'lakeconnection'<-as.character(TableS1$'lakeconnection')
TableS1$'lakeconnection'[which(TableS1$'lakeconnection'=='DR_LakeStream')]<-'DrainageLake'
TableS1$'lakeconnection'[which(TableS1$'lakeconnection'=='DR_Stream')]<-'Drainage'


names(TableS1)<-c('Lake', 'Latitude', 'Longitude', 'Lake area', 'Perimeter', 'Shoreline development index', 'Max depth', 'Watershed area', 'Total stream length', 'Lake connection')

TableS1[,2:3]<-round(TableS1[,2:3], 4)
TableS1[,c(4:5,8:9)]<-round(TableS1[,c(4:5,8:9)], 0)
TableS1[,6]<-round(TableS1[,6], 2)
TableS1[,7]<-round(TableS1[,7], 1)


TableS1_out<-TableS1[which(TableS1$Lake %in% goodlakes),]

TableS1_out$Lake<-sub('Lake', '', TableS1_out$Lake)

TableS1_out<-TableS1_out[order(TableS1_out$Lake),]



write.table(TableS1_out, file='Data/NHLDLakesMorph.csv', row.names=F, sep=',')

#Drop Lat//Long to keep variables in same order for future analysis
j<-j[,-which(names(j) %in% c('Latitude', 'Longitude'))]

saveRDS(j, file='Data/FlameStatsLagosChemAllWide.rds')


