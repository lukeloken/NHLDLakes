library(lubridate)
library(dplyr)
library(tidyr)
library(gtools)


# ########################
# Prepare water chemistry data
# ########################
WaterChem_raw<-read.csv('Data/LTER_WaterChemistry_2018Jan12.csv', header=T, stringsAsFactors = F)
str(WaterChem_raw)

#Omit bad flags and -99 values
BADFLAGS<-LETTERS[c(1:2,6:13, 15:16)]
WaterChem_raw$Value[WaterChem_raw$Flags %in% BADFLAGS]<-NA
WaterChem_raw$Value[WaterChem_raw$Value == (-99)]<-NA

#omit weird columns and spread datatable
WaterChem_slim<-select(WaterChem_raw, Event, Location, Sample.Name, Date, Sample.ID, Test, Value)

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

# write.table(df, file='LakeNames_SampleCount.csv', sep=",", row.names=F)



# ###########################
# Next step
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
    if (nrow(Lake_data)==0){
      Lake_data<-data1}
    else {
      Lake_data<-smartbind(Lake_data, data1, fill=NA)}
  }
}

str(Lake_data)


# ####################
# old code below
# #####################


# Convert long LTER datatable into a wide table
LTERdata<-read.csv(paste(directory, '/LTER_WaterChemistry_2017Mar20.csv', sep=""), header=T, stringsAsFactors = F)


# sample tests (e.g., DOC, NO3)
# sample ids (LTER barcodes)
tests<-unique(LTERdata$Test)
samplesID<-unique(LTERdata$Sample.ID)

# Convert long datatable into a wide table
chemtable<-data.frame(matrix(ncol=1, nrow=length(samplesID)))
names(chemtable)[1]<-c('Sample.ID')
chemtable$Sample.ID<-samplesID

for (test in tests){
  testtable<-subset(LTERdata,Test==test)
  smalltable<-testtable[,c(6,12)]
  names(smalltable)[2]<-test
  chemtable<-merge(chemtable, smalltable, by='Sample.ID', all.x=T)
}

str(chemtable)
summary(chemtable)
chemtable[chemtable==-99]<-NA


# ###################
# And merge with LTER
# ###################

AllMerged<- merge(All_data, chemtable, by.x='Sample.Number', by.y='Sample.ID', all.x=T)
head(AllMerged)

splitnames2<-strsplit(AllMerged$folder, '_')
str(splitnames2)
vector2<-unlist(splitnames2)
lakenames<-vector2[seq(2,length(vector2), 2)]
dates<-as.Date(vector2[seq(1,length(vector2), 2)])

AllMerged$LakeName<-lakenames
AllMerged$Date<-dates

names(AllMerged)<-sub(' ', '', as.character(names(AllMerged)))
names(AllMerged)<-sub('\\(', '', as.character(names(AllMerged)))
names(AllMerged)<-sub(")", '',as.character(names(AllMerged)))

# Summarize by lake day

lakes.waterchem = 
  AllMerged %>%
  group_by(LakeName) %>%
  select(DOC, TotalNF, TotalNUF, TotalPF, TotalPUF, NH4, NO3NO2, SRP, Cl) %>%
  summarise(
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


nhdIDs<-read.csv('E:/Dropbox/FLAME_NHLDLakes/Lake Information/NHD_IDs.csv', header=T, stringsAsFactors = F)
nhdIDs$lakename<- sub(' ', '', nhdIDs$Name)
nhdIDs$lakename<- sub(' ', '', nhdIDs$lakename)
nhdIDs$lakename<- sub(' ', '', nhdIDs$lakename)
nhdIDs$lakename<- sub('Saint', 'St', nhdIDs$lakename)
nhdIDs$lakename<- sub('LakeHelen', 'HelenLake', nhdIDs$lakename)
nhdIDs$lakename<- sub('LakeLaura', 'LauraLake', nhdIDs$lakename)
nhdIDs$lakename<- sub('PresqueIsleRiverFlooding', 'PresqueIsleLake', nhdIDs$lakename)

intersect(df$lakes, nhdIDs$lakename)
nhdIDs$lakename[!is.element(nhdIDs$lakename, df$lakes)]

   
lagos_data<-read.csv('E:/Dropbox/FLAME_NHLDLakes/Lake Information/lake_attributes.csv', header=T, stringsAsFactors = F)

lagosMerge<-merge(nhdIDs, lagos_data, by.x='Permanent.Identifier', by.y='nhdid', all=T)
names(lagosMerge)[1]<-'nhdid'

flamechemlagosmerge<-merge(lakes_df, lagosMerge, by.x='lakes', by.y='lakename', all=T)


write.table(flamechemlagosmerge, file='FLAMENHLDLakes_WaterChem_Lagos.csv', sep=",", row.names=F)

