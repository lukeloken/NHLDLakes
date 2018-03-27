
#Script to create a pca of all predictor variables. Inputs are spatial medians (flame), lagos, and water chemistry


library(RColorBrewer)
library(viridis)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(corrplot)

j <- readRDS(file='Data/FlameStatsLagosChemAllWide.rds')

colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_points<-paste(goodvars, 'points', sep='_')
goodvars_pixels<-paste(goodvars, 'pixels', sep='_')

# mean_columns<-paste(goodvars, 'points', 'Mean', sep='_')
med_columns_pix<-paste(goodvars, 'pixels', 'Median', sep='_')

#only use lakes with lake area
k<-j[which(!is.na(j$lake_area_ha)),]
#Change lake connection to numeric (1 = higher landscape position)
connect_df<-data.frame(lakeconnection=c('Isolated', 'Headwater', 'DR_Stream', 'DR_LakeStream'), value=1:4)
k$lakeconn <- as.numeric(as.character(connect_df$value[match(k$lakeconnection, connect_df$lakeconnection)]))

# predvars<-names(k)[c(2,4,6:17)]
predvars<-c(names(k)[c(2,6, 10, 12,15,16,17:22)], 'lakeconn')


#PCA

pca_df<-k[c(predvars,med_columns_pix)]
names(pca_df)<-c(predvars, shortnames)
pca_df<-na.omit(pca_df)
removevars<-c(6,8,9,11)
pca_df2<-pca_df[-c(6,8,9,11)]



pca <- prcomp(pca_df2, center = TRUE, scale. = TRUE, rank=5) 


plot(pca, type='l')
abline(h=1)
summary(pca)
pca
PerVar<-(pca$sdev^2)/sum((pca$sdev^2))*100
CumVar<-cumsum(PerVar)
plot(CumVar, ylab='Cumulative variance (%)', type='o', ylim=c(0,100))

png("Figures/PCA/PCASummary.png", res=200, width=9,height=5, units="in")
par(mfrow=c(1,2))
par(mar=c(2.5,3.5,.5,0), oma=c(.5,0,0,0))
par(mgp=c(2, .5, 0))

plot(pca, type='l', main='')
abline(h=1, lty=3)
box(which='plot')
mtext('PCA #',1,2)

corrplot(pca$rotation, is.corr=FALSE, mar=c(0,0,0,1.5), oma=c(0,0,0,0), tl.col='black', cl.pos='r', cl.ratio=0.5, col=brewer.pal(10, 'RdYlBu'), cl.lim=c(-.75, .75))

mtext('Correlation', 4, -1.5)
legend(-7,par('usr')[4], c('Predictor variable'), bty='n')

dev.off()


png("Figures/PCA/PredictorVarsPCA_v1.png", res=200, width=5,height=5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_1v2<-autoplot(pca, x=2, y=1, data=pca_df, colour = 'lakeconnection', size=3, 
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=rev(viridis(4))) + 
  theme_bw() + 
  theme(legend.position=c(0.83,0.15), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(p_1v2)  

dev.off()

png("Figures/PCA/PredictorVarsPCA_v2.png", res=200, width=5,height=5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))


p_3v2<-autoplot(pca, x=3, y=1, data=pca_df, colour = 'lakeconnection', size=3, 
                loadings = TRUE, loadings.colour = 'grey', 
                loadings.label = TRUE, loadings.label.colour='black', 
                loadings.label.size = 3) + 
  scale_color_manual(values=rev(viridis(4))) + 
  theme_bw() + 
  theme(legend.position='none', panel.grid.major = element_blank(), panel.grid.minor = element_blank())

print(p_3v2)  

dev.off()

png("Figures/PCA/PredictorVarsPCA_v3.png", res=200, width=10,height=5, units="in")
par(mfrow=c(1,1))
par(mar=c(1.5,3,.5,.5), oma=c(2.5,1,0,0))
par(mgp=c(2, .5, 0))

grid.arrange(p_1v2, p_3v2, ncol=2)

dev.off()

