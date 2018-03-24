
#Wide Table (one row = one lake)
MyChemLagos <- readRDS('Data/MyLakesLagosFlameChem.rds') 
names(MyChemLagos)[1]<-'Lake'

e<-MyChemLagos[,c(1, 14:22, 57, 62:63, 79, 75, 103, 105,191,192)]

#Create additional metrics describing watershed/stream attributes compared to lake size
e$iws_ha[e$Lake=='MidgeLake']<-10
e$iws_streamdensity_streams_sum_lengthm[e$Lake=='MidgeLake']<-0
e$iws_streamdensity_streams_density_mperha[e$Lake=='MidgeLake']<-0


e$streamlength_to_lakearea<-e$iws_streamdensity_streams_sum_lengthm/e$lake_area_ha
e$streamdensity_to_lakearea<-e$iws_streamdensity_streams_density_mperha/e$lake_area_ha

e$iwsarea_to_lakearea <- e$iws_ha/e$lake_area_ha


#Long Table (one row = one value), values are by lake, date, Geotype, variable, stat
d<- readRDS(file='Data/FlameStatsAll.rds')

f<-as.data.frame(full_join(e,d))

g <- f %>%
  filter(variable!='NA' | Statistic != 'NA')

h <- g[which(!is.na(g$value) & g$Date!='2015-05-27'),]

i <- h %>% 
  unite(col, variable, GeoType, Statistic, sep='_') 

j <- i %>%
  tidyr::spread(key=col, value=value)

saveRDS(j, file='Data/FlameStatsLagosChemAllWide.rds')
