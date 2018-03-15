rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyr)
library(gtools)

setwd("E:/Git_Repo/NHLDLakes")

# ###########################
# nhdID Data
# ###########################

nhdIDs<-read.csv('E:/Dropbox/FLAME_NHLDLakes/Lake Information/NHD_IDs.csv', header=T, stringsAsFactors = F)
nhdIDs$lakename<- sub(' ', '', nhdIDs$LagosName)
nhdIDs$lakename<- sub(' ', '', nhdIDs$lakename)
nhdIDs$lakename<- sub(' ', '', nhdIDs$lakename)
nhdIDs$lakename<- sub('Saint', 'St', nhdIDs$lakename)
nhdIDs$lakename<- sub('LakeHelen', 'HelenLake', nhdIDs$lakename)
nhdIDs$lakename<- sub('LakeLaura', 'LauraLake', nhdIDs$lakename)
nhdIDs$lakename<- sub('PresqueIsleRiverFlooding', 'PresqueIsleLake', nhdIDs$lakename)


# ########################
# Prepare water chemistry data
# ########################
WaterChem_raw<-read.csv('Data/LTER_WaterChemistry_2018Jan12.csv', header=T, stringsAsFactors = F)
str(WaterChem_raw)

#Omit bad flags and -99 values
BADFLAGS<-LETTERS[c(1:2,6:13, 15:16)]
WaterChem_raw$Value[WaterChem_raw$Flags %in% BADFLAGS]<-NA
WaterChem_raw$Value[WaterChem_raw$Value == (-99)]<-NA

WaterChem_raw$Value[WaterChem_raw$Sample.ID == 14656 & WaterChem_raw$Test=='Total P (UF)']<-NA

#omit weird columns and spread datatable
WaterChem_slim<-dplyr::select(WaterChem_raw, Event, Location, Sample.Name, Date, Sample.ID, Test, Value)

WaterChem_wide<-spread(WaterChem_slim, key=Test, value=Value)

str(WaterChem_wide)
summary(WaterChem_wide)


# ########################
# Prepare NHLD flame data
# ########################

directory<-'E:/Dropbox/FLAME_NHLDLakes'
# setwd(directory)

folders<-list.files(paste(directory, '/Data', sep=""))

lakes <- gsub(".*_","",folders)
lakes <- gsub("[0-9]","",lakes)

# Make table of sample occurences
table<-table(lakes)

df<-as.data.frame(table, stringsAsFactors = F)
df[nrow(df)+1,]<-c('PeterLakeEWI', 19)
df[nrow(df)+1,]<-c('PaulLakeEWI', 19)
df[nrow(df)+1,]<-c('TuesdayLakeEWI', 16)
df[nrow(df)+1,]<-c('SparklingLake48Hours', 19)
df[nrow(df)+1,]<-c('PeterLake48Hours', 17)
df[nrow(df)+1,]<-c('PaulLake48Hours', 17)

# ###########################
# Prepare Lake Sample Data
# ###########################

directories<-list.files(paste0(directory, '/Data'))
directories_2015<-directories[grep('2015', directories)]

Lake_data<-as.data.frame(matrix(nrow=0, ncol=0))

dir<-directories_2015[17]
for (dir in directories_2015){
  subdir<-paste0(directory, '/Data/',dir)
  subdir_files<-list.files(subdir)
  
  file<-subdir_files[grep('_Samples.csv', subdir_files)]
  lakename<-tail(unlist(strsplit(dir, "_")), 1)
  
  if (length(file)==1){
    data1<-read.csv(paste(subdir, file, sep="/"), header=T, stringsAsFactors = F)
    data1$LakeName<-rep(lakename, nrow(data1))
    data1$LakeDay<-rep(dir, nrow(data1))
    if (nrow(Lake_data)==0){
      Lake_data<-data1}
    else {
      Lake_data<-smartbind(Lake_data, data1, fill=NA)}
  }
}

str(Lake_data)


# ###################
# Merge Flame with LTER
# still workin below
# ###################

AllMerged<- merge(Lake_data, WaterChem_wide, by.x='Sample.Number', by.y='Sample.ID', all.x=T)
head(AllMerged)

names(AllMerged)<-gsub(' ', '', as.character(names(AllMerged)))
names(AllMerged)<-sub('\\(', '', as.character(names(AllMerged)))
names(AllMerged)<-sub(")", '',as.character(names(AllMerged)))

str(AllMerged)
samplenames<-unique(AllMerged$Sample.Notes)
middlenames<-c('deep hole', 'Deep Hole', 'LTER float', 'Central Bay', 'LTER float', 'deephole', 'lter bouy', 'pelagic', 'southern basin(pelagic)', 'middle of lake, pelagic', "Pelagic, middle of the lake", "Deep hole", "center", "South Basin- Deep Hole LTER site", "DNR buoy, middle of the lake", 'south basin', NA)

saveRDS(AllMerged, file='Data/NHLDLakes_WaterChemFLAME.rds')

AllMiddle<-subset(AllMerged, Sample.Notes %in% middlenames)

# Summarize by lake day

lakes.waterchem = 
  AllMerged %>%
  group_by(LakeName) %>%
  dplyr::select(DOC, TotalNF, TotalNUF, TotalPF, TotalPUF, NH4, NO3NO2, SRP, Cl) %>%
  summarise(
    n = n(),
    DOC_AVG = mean(DOC, na.rm=T),
    TotalNF_AVG = mean(TotalNF, na.rm=T),
    TotalNUF_AVG = mean(TotalNUF, na.rm=T),
    TotalPF_AVG = mean(TotalPF, na.rm=T),
    TotalPUF_AVG = mean(TotalPUF, na.rm=T),
    NH4_AVG = mean(NH4, na.rm=T),
    NO3NO2_AVG = mean(NO3NO2, na.rm=T),
    SRP_AVG = mean(SRP, na.rm=T),
    Cl_AVG = mean(Cl, na.rm=T)
    )
    
lakes.waterchem
lakes_df<-merge(df, lakes.waterchem, by.x='lakes', by.y='LakeName', all=T)

#Middle only
lakes.waterchem.middle = 
  AllMerged %>%
  filter(Sample.Notes %in% middlenames | LakeName %in% c('LauraLake', 'SpiderLake')) %>%
  group_by(LakeName) %>%
  dplyr::select(DOC, TotalNF, TotalNUF, TotalPF, TotalPUF, NH4, NO3NO2, SRP, Cl) %>%
  summarise(
    n = n(),
    DOC_AVG = mean(DOC, na.rm=T),
    TotalNF_AVG = mean(TotalNF, na.rm=T),
    TotalNUF_AVG = mean(TotalNUF, na.rm=T),
    TotalPF_AVG = mean(TotalPF, na.rm=T),
    TotalPUF_AVG = mean(TotalPUF, na.rm=T),
    NH4_AVG = mean(NH4, na.rm=T),
    NO3NO2_AVG = mean(NO3NO2, na.rm=T),
    SRP_AVG = mean(SRP, na.rm=T),
    Cl_AVG = mean(Cl, na.rm=T)
  )

lakes.waterchem.middle
lakes_df_middle<-merge(df, lakes.waterchem.middle, by.x='lakes', by.y='LakeName', all=T)

nomiddlelakes<-setdiff(lakes.waterchem$LakeName, lakes.waterchem.middle$LakeName)
AllMerged[AllMerged$LakeName %in% nomiddlelakes,c(2, 6, 65)]

saveRDS(lakes_df, file='Data/NHLDLakes_WaterChem.rds')
write.table(lakes_df, file='Data/NHLDLakes_WaterChem.csv', sep=',', row.names=F)

saveRDS(lakes_df_middle, file='Data/NHLDLakes_WaterChem_middle.rds')
write.table(lakes_df_middle, file='Data/NHLDLakes_WaterChem_middle.csv', sep=',', row.names=F)



middlenames<-names(lakes_df_middle)
lakechemsummary<-lakes_df_middle %>%
  summarize_at(middlenames[3:11], c('mean', 'median', 'min', 'max', 'sd'), na.rm=T)

lakechemsummary<-as.data.frame(summary(lakes_df_middle[,3:11]))
