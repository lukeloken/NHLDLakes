# =============================================
# Code to Extract Flame Data from spatial summaries
# Output rdata file of all spatial summaries
# =============================================

# Load packages
library(gtools)
library(lubridate)
setwd("E:/Git_Repo/NHLDLakes")

#### Merge all spatial summaries ####

data_dir<-"E:/Dropbox/FLAME_NHLDlakes/pixelsummaries"
point_dir<-"E:/Dropbox/FLAME_NHLDlakes/pointsummaries"

spatial_summaries<-list.files(data_dir)
spatial_summaries<-spatial_summaries[-which(
  spatial_summaries %in% c("TenderfootCreek2016-09-05cleaned.csv",
                       "AllequashLake2015-05-27cleaned.csv",
                       "AllequashLake2015-06-24cleaned.csv",
                       "BigMuskellungeLake2015-06-02cleaned.csv",
                       "BigMuskellungeLake2015-08-03cleaned.csv",
                       "BigStGermainLake2015-06-10cleaned.csv",
                       "BigStGermainLake2015-07-07cleaned.csv",
                       "BlackOakLake2016-09-05cleaned.csv", 
                       "DiamondLake2015-06-24cleaned.csv",
                       "DiamondLake2015-08-05cleaned.csv",
                       "DogLake2015-07-28cleaned.csv",
                       "LakeEscanaba2015-06-30cleaned.csv",
                       "LakeNebish2015-06-30cleaned.csv",
                       "LittleArborVitaeLake2015-07-07cleaned.csv",
                       "LittleArborVitaeLake2015-08-03cleaned.csv",
                       "LittleArborVitaeLake2016-09-04cleaned.csv",
                       "PalmerLake2016-09-05cleaned.csv",
                       "RainbowLake2015-08-12cleaned.csv",
                       "SparklingLake2015-05-26cleaned.csv",
                       "SparklingLake2015-07-13cleaned.csv",
                       "SparklingLake2015-08-24cleaned.csv",
                       "SpiderLake2015-07-20cleaned.csv",
                       "VandercookLake2015-07-13cleaned.csv"))]

point_summaries<-list.files(point_dir)
point_summaries<-point_summaries[-which(
  point_summaries %in% c("TenderfootCreek2016-09-05cleaned.csv",
                           "AllequashLake2015-05-27cleaned.csv",
                           "AllequashLake2015-06-24cleaned.csv",
                           "BigMuskellungeLake2015-06-02cleaned.csv",
                           "BigMuskellungeLake2015-08-03cleaned.csv",
                           "BigStGermainLake2015-06-10cleaned.csv",
                           "BigStGermainLake2015-07-07cleaned.csv",
                           "BlackOakLake2016-09-05cleaned.csv", 
                           "DiamondLake2015-06-24cleaned.csv",
                           "DiamondLake2015-08-05cleaned.csv",
                           "DogLake2015-07-28cleaned.csv",
                           "LakeEscanaba2015-06-30cleaned.csv",
                           "LakeNebish2015-06-30cleaned.csv",
                           "LittleArborVitaeLake2015-07-07cleaned.csv",
                           "LittleArborVitaeLake2015-08-03cleaned.csv",
                           "LittleArborVitaeLake2016-09-04cleaned.csv",
                           "PalmerLake2016-09-05cleaned.csv",
                           "RainbowLake2015-08-12cleaned.csv",
                           "SparklingLake2015-05-26cleaned.csv",
                           "SparklingLake2015-07-13cleaned.csv",
                           "SparklingLake2015-08-24cleaned.csv",
                           "SpiderLake2015-07-20cleaned.csv",
                           "VandercookLake2015-07-13cleaned.csv"))]



merged_summary<-as.data.frame(matrix(nrow=0, ncol=0))
merged_points<-merged_summary

summary_file<-spatial_summaries[1]
for (summary_file in spatial_summaries){
  
  
  str1<-sub("cleaned.csv", "", summary_file)
  date<-ymd(gsub("[A-z]", '', str1))
  str2<-gsub('[0-9]','',str1)
  lake<-gsub('-', '', str2)
  lakeday<-paste(lake, date, sep="_")
  
  summary<-read.csv(paste(data_dir, summary_file, sep="/"), header=T, stringsAsFactors = F)
  rownames(summary)<-summary$Variable
  transposed_summary<-as.data.frame(t(subset(summary, select = -c(Variable))))
  transposed_summary$Date<-as.Date(rep(date, nrow(transposed_summary)))
  transposed_summary$Statistic<-rownames(transposed_summary)
  transposed_summary$Lake<-rep(lake, nrow(transposed_summary))
  transposed_summary$LakeDay<-rep(lakeday, nrow(transposed_summary))
  
  if (nrow(merged_summary)==0){
    merged_summary<-transposed_summary
  }
  else {
    merged_summary<-smartbind(merged_summary, transposed_summary, fill=NA)
  }
  
  points<-read.csv(paste(point_dir, summary_file, sep="/"), header=T, stringsAsFactors = F)
  rownames(points)<-points$Variable
  transposed_points<-as.data.frame(t(subset(points, select = -c(Variable))))
  transposed_points$Date<-as.Date(rep(date, nrow(transposed_points)))
  transposed_points$Statistic<-rownames(transposed_points)
  transposed_points$Lake<-rep(lake, nrow(transposed_points))
  transposed_points$LakeDay<-rep(lakeday, nrow(transposed_points))
  
  if (nrow(merged_points)==0){
    merged_points<-transposed_points
  }
  else {
    merged_points<-smartbind(merged_points, transposed_points, fill=NA)
  }
}

merged_summary$Date<-as.Date(merged_summary$Date)
rownames(merged_summary)<-c()

merged_points$Date<-as.Date(merged_points$Date)
rownames(merged_points)<-c()


# Save to Git folder
saveRDS(merged_summary, file='Data/FlameSpatialSummaries.rds')
write.table(merged_summary, file='Data/FlameSpatialSummaries.csv')

# Save to Git folder
saveRDS(merged_points, file='Data/FlamePointsSummaries.rds')
write.table(merged_points, file='Data/FlamePointsSummaries.csv')

