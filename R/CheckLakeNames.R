
rm(list = ls())

setwd("E:/Git_Repo/NHLDLakes")
mylagos2<-readRDS(file='Data/MyLakesLagos.rds')

setwd("E:/Dropbox/FLAME_NHLDLakes/")
semivar_alllakes<-readRDS(file='SpatialOutputs/semivar_alllakes.rds')

SampledLakes<-unique(semivar_alllakes$Lake)
# mylagos2$flame_name
LagosLakes<-unique(mylagos2$flame_name)
# LagosLakes<-gsub(' ', '', LagosLakes)
# LagosLakes<-gsub('Saint', 'St', LagosLakes)
# LagosLakes<-gsub('EscanabaLake', 'LakeEscanaba', LagosLakes)

setdiff(LagosLakes, SampledLakes)
setdiff(SampledLakes, LagosLakes)

