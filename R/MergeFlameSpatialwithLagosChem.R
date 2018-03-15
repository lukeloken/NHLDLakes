
#Wide Table (one row = one lake)
MyChemLagos <- readRDS('Data/MyLakesLagosFlameChem.rds') 
names(MyChemLagos)[1]<-'Lake'

e<-MyChemLagos[,c(1, 14:22, 57, 62:63, 78, 102:103, 105)]

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
