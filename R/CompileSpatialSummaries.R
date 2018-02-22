# =============================================
# Code to Extract Flame Data from spatial summaries
# Output rdata file of all spatial summaries
# =============================================

# Load packages
library(gtools)

#### Merge all spatial summaries ####

data_dir<-"E:/Dropbox/FLAME_NHLDlakes/pixelsummaries"

spatial_summaries<-list.files(data_dir)

merged_summary<-as.data.frame(matrix(nrow=0, ncol=0))

summary_file<-spatial_summaries[1]
for (summary_file in spatial_summaries){
  
  str1<-sub("cleaned.csv", "", summary_file)
  date<-ymd(gsub("[A-z]", '', str1))
  str2<-gsub('[0-9]','',str1)
  lake<-gsub('-', '', str2)
  lakeday<-paste(lake, date, sep="_")
  
  summary<-read.csv(paste(data_dir, summary_file, sep="/"), header=T, stringsAsFactors = F)
  summary$range<-summary$Max-summary$Min
  # str(summary)
  # head(summary)
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
}

merged_summary$Date<-as.Date(merged_summary$Date)
rownames(merged_summary)<-c()


# Save to Git folder
saveRDS(merged_summary, file='Data/FlameSpatialSummaries.rds')
write.table(merged_summary, file='Data/FlameSpatialSummaries.csv')

