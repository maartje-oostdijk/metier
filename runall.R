#Elzbieta Baranowska MFRI 2024

setwd("/home/sjor/maartje/final_metier/")
# Running scripts to generate métiers
only_figs <- TRUE #note, can only be set to true if stocks_wide_zcomp4.txt is in main folder
atlantis_logb <- FALSE# part of code to run logbook analysis based on Atlantis functional groups


#when database is updated mar needs to be re-installed (this contains confidential logbook data and can only be used after an agreement with the MFRI)
 #devtools::install_git(
  # "https://gitlab.hafogvatn.is/dev/mar.git",
  # dependencies = FALSE)
#install packages if they are not installed (not yet complete!)
if(!require("tidyverse")) {install.packages("tidyverse")}
if(!require("mapdata")) {install.packages("mapdata")}
if(!require("geo")) {install.packages("/home/sjor/maartje/final_metier/geo_1.4-3.tar.gz", repos = NULL, type = "source")}


### --- load data - creates dataDEM object or load first the logbook_data----
source('R/1_load_data.R')

### --- filter out tows near port as the dataDEM is generated in load_data before ----
source('R/2_near_port_filter.R')

### ---- add 4 columns with spatial and temporal information, creates extra_col object and data1 ----
source('R/3_cyclical_adjustment.R')

### ---- do terminal_run ----
# deals with 0s, logit transformation, creates data2 object - need to run in terminal (maybe not) after cyclical_adjustment.
# source('R/4_terminal_run.R')

#if there is no need for running in terminal then enough just to get the previously produced matrix and logit transform here

stocks_wide_zcomp <- read.delim("stocks_wide_zcomp_NEW22016_2019.txt")
apply(stocks_wide_zcomp, MARGIN = 2, FUN = boot::logit) -> catchdata1
rm(stocks_wide_zcomp)
data2 <- as.data.frame(catchdata1) %>% rownames_to_column(var="haul_id") 

data_veg <- 
  data2 %>% 
  inner_join(extra_col) %>% #join species composition data with location and season data
  column_to_rownames(var="haul_id")

print(dim(data_veg))


if(!only_figs){
  ### ---- DO THIS STEP BY STEP IN TERMINAL - distance matrix and performs clustering use: nohup Rscript runall.R
  # outputs 'hc1_ward2.rds' file for later uploading
  
   #data_veg <- test_data1 %>% 
     #inner_join(extra_col) %>% 
     #column_to_rownames(var="sub_vessel") # 5% removed 
  
  #RUN IN TERMINAL
 #distmat <- vegan::vegdist(data_veg, method = "euclidean")#
    
  #hc1_ward2 <- fastcluster::hclust(distmat, method = "ward.D2")#!NOTE! sometimes this does not work right after the distance matrix is created if it stalls, run matrix.R in terminal by running nohup Rscript matrix.R
  #rm(distmat)
  #hc1_complete <- fastcluster::hclust(distmat, method = "complete")
  #hc1_average <- fastcluster::hclust(distmat, method = "average")
  
#  print(dim(hc1_ward2))
 # saveRDS(hc1_ward2, 'hc1_ward2.rds')}
  #saveRDS(hc1_complete, 'hc1_complete.rds')
  #saveRDS(hc1_average, 'hc1_average.rds')


####-----RUN THIS TO ONLY MAKE FIGURES --- ####

### ---- Make figures from cluster analyses - takes catchdata object and hc1_ward2
# also uses original data object dataDEM_clean
# creates objects clustering and stockshares_table


## THIS RDS FILE CAN BE FOUND IN THE TEAMS FILES:
# Files / Documents / Fishing into the Future / WP1 / Metiér analysis


}

hc1_ward2 <- readRDS('hc1_ward2.rds')

#----- set filepath for saving plots-----
fs::dir_create("plots")
folder.pic <- file.path("plots")

catchdata <- data_veg 

#JACOB: WHAT YOU WANT ARE THE OBJECTS:
#'clustering', 'stockshares_table'

n_cluster <- 7

clustering <- 
  catchdata %>% 
  as.data.frame()%>% 
  rownames_to_column(var = "haul_id")%>%
  as_tibble() %>% 
  dplyr::mutate(cluster_num = cutree(hc1_ward2 , k=n_cluster),
      cluster = cluster_num %>% 
      paste('cluster', .) %>% 
      factor(., levels = paste('cluster', 1:max(cluster_num)))) %>% 
  dplyr::select(-cluster_num) %>% 
  dplyr::mutate_at('haul_id', as.double)

dataDEM_clust <- left_join(dataDEM, clustering)

dataDEM_clust_size <- 
  dataDEM_clust %>% 
  dplyr::select(year,cluster,catch) %>% 
  group_by(cluster) %>% 
  dplyr::summarise(catch_clust=sum(catch))

dataDEM_clust <- left_join(dataDEM_clust, dataDEM_clust_size, by=c( "cluster")) 

#---- optimal number of clusters---- 
#source('R/9_optimal_clusters.R')

### ---- to generate most of the plots ----
source('R/5_before_plotting.R')

### ---- cuttree 
source('R/6_treeplot.R')

### ---- Structure plot ---- (Figure 1)
source('R/7_structure_plot.R')

### ---- Spatial plot----
source('R/8_spatialM.R')


