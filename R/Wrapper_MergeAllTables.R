#Wrapper function to call multiple scripts for combining datatables

# Things to do to convert sensor measurements into flame data
# Need to do this first and if you update any datasource('R/RUN_FLAME_NHLDLoop.R')

# source('R/RUN_FLAME_NHLDLoop.R')
# source('R/InverseDistanceWeighting_FLAMeNHLDLakes_Loop.R')
# source('R/SpatialStatsPoints_Loop.R')
# source('R/BindSpatialPoints.R')



############# Combine multiple data types ####


#Combine LTER Water Chemistry and Flame samples from lake centers
source('R/AggregateWaterChemistryMiddle.R')

#Combine previous with Lagos geo, waterquality
source('R/DownloadLagosLinktoWaterChemFlame.R')
# MyChemLagos == table with lagos,flame_middle,waterchem
# saved at: 'Data/MyLakesLagosFlameChem.rds' 

#Compile pixel and spatial summaries (mean, median, CV, SDL, etc)
source('R/CompileSpatialSummaries.R')
# Two tables
# merged_summary at 'Data/FlameSpatialSummaries.rds' = pixel summaries
# merged_points at 'Data/FlamePointsSummaries.rds' = point summaries

# Merge semivariance ranges
source('R/MergeSpatialSummarywithGeostats.R')
# One long table with all flame spatial summaries
# d at 'Data/FlameStatsAll.rds' = pixel and point summaries and Semivar ranges

# Combine spatials stats with Lagos and Chemistry data. 
source('R/MergeFlameSpatialwithLagosChem.R')
# j at 'Data/FlameStatsLagosChemAllWide.rds' = wide table of all variables, stats


############# Plotting ####
# Map Figure (This takes some time due to masking nlc raster)
# source('PlotLandCoverNHLD.R')

#Plot boxplots of spatial heterogeneity among variable types
source('R/CompareSpatialHeterogeneity_v2.R')

#Histograms of maxminusmin and IQR across lakes and variables
source('R/PlotHistRangeIQRAmongVariableTypes.R')

#Scatterplots of semivariance ranges
source('R/PlotSemivarRangeScatterplots.R')

#PCA analysis of predictor variables
source('R/PCAanalysis.R')


#Variance partitioning 
source('R/VariancePartioning.R')


