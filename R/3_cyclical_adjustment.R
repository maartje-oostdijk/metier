#Elzbieta Baranowska MFRI 2024

# START WITH LOADING THE DATASET 
# Here we are using demersal trawl

head(dataDEM) ## dataDEM output coming from near_port_filter.R 

m_lat = mean(dataDEM$lat)
m_lon = mean(dataDEM$lon)

demersals1 <-
  dataDEM %>%
  dplyr::mutate(x2 = lon - m_lon, 
                y2 = lat - m_lat,
                r = sqrt(x2^2 + y2^2),
                t = atan2(y2,x2),
                #t1=atan(y2/x2), 
                rtrans = (r-min(r))/(max(r)-min(r)),
                ttrans = (t-min(t))/(max(t)-min(t))) %>% 
  dplyr::mutate(doy_sin = sin((doy / 365) * 2*pi), 
                doy_cos = cos((doy / 365) * 2*pi))

demersals1 %>% dplyr::select(haul_id, rtrans, ttrans, doy_sin, doy_cos) %>% distinct(haul_id, .keep_all = TRUE) -> extra_col

extra_col = extra_col %>%
  mutate(haul_id = as.character(haul_id))

# This is the data that goes in the catch_transformation function in catchdata_adjusted.R and run in putty.
data1 <- dataDEM %>% dplyr::select(haul_id,species,catch) %>% 
  pivot_wider(names_from = species, values_from = catch, values_fill = 0) %>% 
  remove_rownames %>% column_to_rownames(var="haul_id") %>% as.data.frame() %>%  
  rownames_to_column(var="haul_id") %>% as_tibble() %>% 
  pivot_longer(names_to = 'species', values_to = 'catch', 2:(length(unique(dataDEM$species))+1))
