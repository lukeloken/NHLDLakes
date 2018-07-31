library(rpart)
library(rpart.plot)


l <- readRDS(file='Data/WideTableAllNoNA.rds')

colors<-c('#377eb8', '#e41a1c', '#4daf4a')
colorbyvar<-colors[c(1,1,1,1,2,2,2,2,3,3)]

goodvars<-c("TempC", "SPCuScm", "fDOMRFU", "TurbFNU", "pH", "ODOmgL",  "CO2uM", "CH4uM", "ChlARFU", "BGAPCRFU")
shortnames<-c("Temp", "SPC", "fDOM", "Turb", "pH", "DO", "CO2", "CH4", "ChlA", "BGA")

goodvars_points<-paste(goodvars, 'points', sep='_')

CV_columns<-paste(goodvars, 'points', 'CV', sep='_')
mean_columns<-paste(goodvars, 'points', 'Mean', sep='_')

CV_labels<- paste0(goodvars, ' CV')

#####################################
## Cart model for ChlA CV
#####################################

column<-1
for (column in 1:length(CV_columns)){
  yname<-CV_columns[column]
  
  #remove zero values 
  l_small = l[l[yname]>0,]
  
  yvar<- l_small[yname]
  
  #create equation for cart model
  equation = yname ~ names(l_small)[1] + names(l_small)[2]  
  equation = paste0(yname, " ~ DOC + TotalPUF + Cl + lake_area_ha + lakeconnection + ShorelineIndex + lakeconn + Temp + SPC + fDOM  + Turb + pH + DO + CO2 + CH4 + ChlA + BGA")
  
  png(paste0("Figures/Carts/", yname, ".png"), height=5, width=7, units="in", res=300)
  
  lake_cart = rpart(equation, method = "poisson", data = l_small, control = rpart.control(cp=0.0001))
  prp(lake_cart, faclen=0, cex = 0.8, extra = 1, type=3)
  mtext(CV_labels[column], 1,3)
  
  dev.off()
  
}


#create a an empty vector for var nsplit
nsplit = c()

#create 1000 trees using a random 90% subset of the data
#to ensure that model pruning is not dependent on observations
#used
i=1
for (i in 1:10) {
lake.90 = l_small[sample(1:nrow(l_small), .9*nrow(l_small)), ]
lake_cart = rpart(equation, method = "anova", data = lake.90, control = rpart.control(cp=0.0001))
#lake_cart = rpart(equation, method = "anova", data = l_small, control = rpart.control(cp=0.0001))
xerror <- lake_cart$cptable[, 4]
xstd <- lake_cart$cptable[, 5]
bestcp = lake_cart$cptable[min(seq(along = xerror)[xerror <= min(xerror) + xstd]),"CP"]
lake_cart.pruned = prune(lake_cart, cp = bestcp)
nsplit[i]=lake_cart$cptable[min(seq(along = xerror)[xerror <= min(xerror) + xstd]),"nsplit"]
}
#nsplit=lake_cart.pruned$cptable[min(seq(along = xerror)[xerror <= min(xerror) + xstd]),"nsplit"]

#which split number occurs most frequently in the 1000 runs?
hist(nsplit, breaks = 30)
nsplit = as.factor(nsplit)
summary(nsplit)
#27 splits occurs most frequently in randomly subsetted data
#37 splits occurs most frequently in full dataset

lake_cart = rpart(equation, method = "anova", data = conc, control = rpart.control(cp=0.0001))  
nsplit <- lake_cart$cptable[, 2]
bestcp = lake_cart$cptable[nsplit==27, "CP"]
lake_cart.pruned = prune(lake_cart, cp = bestcp)
prp(lake_cart.pruned,type = 4, faclen=0, cex = 0.8, extra = 1)
rsq.rpart(lake_cart.pruned)
plot(rsq.rpart(lake_cart))
  
#plot smaller tree so it is readable
bestcp = lake_cart$cptable[nsplit==10, "CP"]
lake_cart.pruned.small = prune(lake_cart, cp = bestcp)
prp(lake_cart.pruned.small,type = 4, faclen=0, cex = 0.6, extra = 1)


#create 20 trees, prune them according to the 1-SE rule, and 
#save trees as PDFs

for (i in 1:20) {
  #conc.90 = conc[sample(1:nrow(conc), .9*nrow(conc)), ]
  lake_cart = rpart(equation, method = "anova", data = conc, control = rpart.control(cp=0.0001))
  xerror <- lake_cart$cptable[, 4]
  xstd <- lake_cart$cptable[, 5]
  bestcp = lake_cart$cptable[min(seq(along = xerror)[xerror <= min(xerror) + xstd]),"CP"]
  lake_cart.pruned = prune(lake_cart, cp = bestcp)
  png(print(paste("Figures/Carts/log_lake_cart", i, ".pdf", sep = "")), height = 5, width = 7, res=200)
  prp(lake_cart.pruned, faclen=0, cex = 0.8, extra = 1)
  dev.off()
}

