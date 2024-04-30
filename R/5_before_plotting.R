#Elzbieta Baranowska MFRI 2024

## before plotting the structure plots
yr <- '2016_2019'

plot_data <-
  dataDEM_clust %>% 
  filter(!haul_id=='-893641') %>% #MO_needtoremove
  dplyr::select(haul_id, cluster, season, year, doy, doy_end, species, species_id, catch, 
                depth, vessel_id, vessel_length,towtime, production_year, landing_date) %>% 
  group_by(haul_id) %>% 
  mutate(catch_haul = sum(catch), ratio = (catch/catch_haul)) %>% 
  ungroup()

# breaks for the structure plots 
breaks = c(0, 300, 600) #optional to do other depth variations
breaks <- sort(breaks)
b_labs <- paste(breaks[-length(breaks)], breaks[-1], sep = "-")
depth_bin <- tibble::tibble(depth = 0:3000) %>% 
  dplyr::mutate(depth_class = as.character(cut(depth,breaks, labels = b_labs, include.lowest = TRUE)), 
                depth_class = ifelse(is.na(depth_class), paste0(max(breaks), "+"), depth_class))
depth_bin <- 
  depth_bin %>% 
  mutate(depth_class = factor(depth_class, ordered = TRUE, levels = depth_bin %>% 
                                dplyr::select(depth_class) %>% distinct() %>% unlist))

struct_plot <- 
  plot_data %>% 
  dplyr::mutate(depth = round(depth)) %>%  
  dplyr::left_join(depth_bin, by = "depth")

sp_order <- 
  struct_plot %>% #sample_n(10000) %>% 
  #filter(cluster== 'cluster 1') %>% 
  dplyr::select(cluster, ratio, species, haul_id, depth_class) %>% 
  group_by(cluster, depth_class, species) %>% 
  dplyr::summarise(sum = sum(ratio)) 

catch_sum <-  
  dataDEM %>% dplyr::select(haul_id,species,catch,year) %>% 
  dplyr::group_by(year,haul_id,species) %>% 
  dplyr::summarise(tot_catch=sum(catch)) %>% ungroup() %>% 
  dplyr::select(!year, haul_id, species) %>% 
  mutate(haul_id=as.character(haul_id))

box_col <- c("#0000FF","#009FFF","#00FFBE","#005300","#FF00B6","#E31A1C","#FFD300")

