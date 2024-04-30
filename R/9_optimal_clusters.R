#Maartje Oostdijk LBHI 2024


#before running do install: 
devtools::install_git('https://github.com/ESulanke/FleetSegmentation')

library(FleetSegmentation)
library(ggpubr)

#function to run numberclust_table over each year in the data
process_yearly_data <- function(y, data_veg, dataDEM_clust) {
  
  # y =2017#for testing
  year_data <- dataDEM_clust%>%
    dplyr::select(year, haul_id) %>%
    distinct() %>%
    filter(year == y)
  
  #select data for year
  catchdata_fun <- data_veg %>%
    dplyr::filter(rownames(data_veg) %in% year_data$haul_id)
  

  pl=numberclust_plot(catchdata = catchdata_fun, distance="euclidean", method="ward.D2", dend_method="range")
  pl=annotate_figure(pl, top = text_grob(paste0("Year ", y), face = "bold", size = 14))
  pl
  
  
  ggsave(file = paste0(folder.pic,"numberclust_",y,".jpg"), path = folder.pic, width = 12, height = 8, dpi=400)
  
  #
  # Perform numberclust_table calculation
  numberclust_result <- numberclust_table(catchdata = catchdata_fun, max_clusternumber = 27, distance="euclidean", method="ward.D2")
  numberclust_result$year =y
  # # Return the result table
  return(numberclust_result)
  
  return(res)
}

# Example usage for each year from 2016 to 2019
result_table_2016 <- process_yearly_data(2016, data_veg, dataDEM_clust)
result_table_2017 <- process_yearly_data(2017, data_veg, dataDEM_clust)
result_table_2018 <- process_yearly_data(2018, data_veg, dataDEM_clust)
result_table_2019 <- process_yearly_data(2019, data_veg, dataDEM_clust)

result_table=bind_rows(result_table_2016, result_table_2017, result_table_2018, result_table_2019)
write.csv(result_table, file="~/final_metier/data_out/result_table_FS_dataveg.csv")#for fleet segmentation outcomes

