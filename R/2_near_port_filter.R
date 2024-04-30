#Sandra Rybicky, MFRI 2024
if(!require("maps")) {install.packages("maps")}
if(!require("FRK")) {install.packages("FRK")}
if(!require("raster")) {install.packages("raster")}

#install.packages("remotes")
#remotes::install_github("SEEG-Oxford/seegSDM")
#---------------------------------------------------------------------------------------
source('/home/sjor/maartje/final_metier/R/nearestland.R')

## Distance between ports (landing_port) and hauls around land, ignoring land masses

# Example: https://rpubs.com/bearedo/AOTTP-Seaway-Distances

# rename df
cod_df_M <- dataDEM


#-------------- create transition matrix

#----- Use rasterized map as basis for transition matrix

shp2 <- map_data("world","Iceland")

names(shp2)[names(shp2)== "lat"] <- "y"
names(shp2)[names(shp2)== "long"] <- "x"
shp2 <- FRK::df_to_SpatialPolygons(shp2, "group",c("x","y"),sp::CRS())
# plot(shp2)
r2 <- raster::raster(nrow=1440, ncol=1440) # This can be increased or decreased

shp <- shp2
r <- r2

r <-raster::rasterize(shp, r, silent=TRUE)
plot(r)



# cut raster to extend of study areas (i.e. Iceland)
# Note: extend is large extent, but keep it like this for creating the raster layer and rather
# cut the extent when plotting!!
bb <- raster::extent(-40, 10, 60, 75)  
r <- raster::crop(r, bb) 
plot(r)
# points(ports5$landing_lon2, ports5$landing_lat, pch=19, col="blue", cex=0.2) # harbor points assigned/edited later

#----- Make landmasses impassable

# also make "throat" of the head a bit thicker --> i.e. add a "sea" raster cell to land
r_test <- r

# assign values to the cells
# # ncell(r) # --> ncell(r)- # na (summary(r_test))--> # cells of land --> 6720 - 6102 = 618

n_land <- raster::ncell(r_test==1)-raster::ncell(r_test[is.na(r_test)])
n_sea <- raster::ncell(r_test[is.na(r_test)])+1
r_test[is.na(r_test)] <- 2:n_sea
r_test[r_test==1] <- -1:-n_land

#identify cell/s of interest and extract value of it
test_cell1 <- raster::xyFromCell(r_test, 14069)
plot(r_test)
points(test_cell1)
test_val1 <- raster::extract(r_test,test_cell1)
# should be ocean for isafjördur

test_cell2 <- raster::xyFromCell(r_test, 14286)
plot(r_test)
points(test_cell2)
test_val2 <- raster::extract(r_test,test_cell2)
# should be ocean close to siglufjoerdur

test_cell3 <- raster::xyFromCell(r_test, 14488)
plot(r_test)
points(test_cell3)
test_val3 <- raster::extract(r_test,test_cell3)
# should be ocean for Akureyri

test_cell4 <- raster::xyFromCell(r_test, 16068)
plot(r_test)
points(test_cell4)
test_val4 <- raster::extract(r_test,test_cell4)
# should be land for stronger boundary at snaefellsnes

test_cell5 <- raster::xyFromCell(r_test, 14101)  
plot(r_test)
points(test_cell5)
test_val5 <- raster::extract(r_test,test_cell5)
# make tail a bit thicker

test_cell6 <- raster::xyFromCell(r_test, 14487)
plot(r_test)
points(test_cell6)
test_val6 <- raster::extract(r_test,test_cell6)
# should be ocean for Akureyri

test_cell7 <- raster::xyFromCell(r_test, 16267)
plot(r_test)
points(test_cell7)
test_val7 <- raster::extract(r_test,test_cell7)
# should be land for stronger boundary at snaefellsnes

test_cell8 <- raster::xyFromCell(r_test, 15273)
plot(r_test)
points(test_cell8)
test_val8 <- raster::extract(r_test,test_cell8)
# make the throat a bit thicker


# make all land cells and this additional sea cell as NA 
r_test[r_test==test_val1] <- n_sea+1
r_test[r_test==test_val2] <- n_sea+2
r_test[r_test==test_val3] <- n_sea+2
r_test[r_test==test_val4] <- NA
r_test[r_test==test_val5] <- NA
r_test[r_test==test_val6] <- n_sea+3
r_test[r_test==test_val7] <- NA
r_test[r_test==test_val8] <- NA
# r_test[r_test==test_val9] <- NA

r_test[r_test<0] <- NA
# make all sea cells = -999
r_test[r_test>=1] <- -999
# assign unit cost to all grid cells in water
r_test[r_test==-999] <- 1

# check
plot(r_test)
# points(x = ports5a$landing_lon2, y = ports5a$landing_lat)

r <- r_test
r_df <- raster::as.data.frame(r, xy=T, optional=T,na.rm=T)

# plot(r)

#--------------------------------------------------------------

# adjust harbor points (to be situated slightly outside land mass)

#--------------------------------------------------------------

# because some are situated too far in land and land was previously defined as NA
# therefore the distance cannot be calculated

## get port info from mfdb, but have no names
# library(mfdb)
# mdb <- mfdb('Iceland', db_params = list(host='mfdb.hafro.is'));
# # ports <- mar::tbl_mar(mdb$db,'"iceland"."port"') %>% as.data.frame()
# # no actual names included here though (would be good to add later)
# # and there are NAs in lat/lon in here...

ports <- tbl_mar(mar, 'channel.port_v')

ports2 <- 
  ports %>% 
  dplyr::rename(landing_lon = longitude, landing_lat = latitude, landing_port = port_no) %>% 
  dplyr::mutate(
    landing_port = as.character(landing_port)
  ) %>% 
  dplyr::select(-c(branch_no)) %>% 
  as_tibble() %>% 
  dplyr::mutate(landing_port = as.numeric(landing_port))

ports2_na <- 
  ports2 %>% 
  dplyr::filter(is.na(landing_lat)) 

ports3 <- 
  cod_df_M %>%  
  distinct(landing_port) %>% 
  as_tibble() %>% 
  dplyr::mutate(IDnum = 1:n())%>%
  dplyr::mutate(landing_port = as.numeric(landing_port))


ports5a <- 
  left_join(ports3, ports2) %>%
  dplyr::rename(port_name = name) %>% 
  dplyr::select(-c(IDnum)) %>% 
  dplyr::mutate(landing_port = as.numeric(landing_port))%>% #, 
  arrange(landing_port) %>% 
  dplyr::filter(!is.na(landing_lat)) 

xy <- ports5a %>% dplyr::select(x = landing_lon, y = landing_lat)

ports_sp <-
  sp::SpatialPointsDataFrame(coords = xy, data = ports5a, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# Error is ok, still works, only means that the port_name has weird characters (cannot identify Icelandic letters)

# find ports which fall in NA areas on the raster (i.e. on land) 
vals <- raster::extract(r_test, ports_sp) 
outside_mask <- is.na(vals)
outside_pts <- ports_sp[outside_mask, ]

# plot these in blue
plot(r_test, xlim = c(-26, -12), ylim = c(63,67))
points(outside_pts, pch = 16, col = 'blue')



pts=outside_pts %>% 
  as.data.frame() %>% 
  dplyr::select(x,y) %>% 
  as.matrix()
# find the nearest raster cell with value (i.e. ocean)
ocean <- nearestLand(pts, r_test, 30638)   # may need to adjust max_distance if NA is still there

# check & plot where they moved to
points(ocean, pch = 16, col = 'dark green')

arrows(outside_pts %>% as.data.frame() %>% dplyr::select(x) %>% as.matrix(),
       outside_pts %>% as.data.frame() %>% dplyr::select(y) %>% as.matrix(),
       ocean[, 1],
       ocean[, 2], length = 0.1)

points(ports5a$landing_lon, ports5a$landing_lat, pch = 16, col = 'black')

ports_new <-
  outside_pts %>% 
  as.data.frame() %>% 
  cbind(ocean %>% 
          as.data.frame() %>% 
          dplyr::rename(x_new = x, y_new = y)) %>% 
  right_join(ports5a) %>% 
  dplyr::rename(landing_lat_old = landing_lat, landing_lon_old = landing_lon, x_old = x, y_old = y) %>% 
  dplyr::mutate(
    landing_lon = ifelse(is.na(x_new), landing_lon_old, x_new),
    landing_lat = ifelse(is.na(y_new), landing_lat_old, y_new),
    landing_port = as.character(landing_port)
  ) %>% as_tibble()%>%
  dplyr::select(landing_port, port_name, landing_lon, landing_lat)

points(ports_new$landing_lon, ports_new$landing_lat, pch = 16, col = 'red')

# join new positions to actual df
cod_wPorts <- 
  cod_df_M %>% 
  dplyr::mutate(landing_port = as.character(landing_port)) %>% 
  dplyr::select(-landing_lon, -landing_lat) %>% 
  left_join(ports_new) 

#################################

# check ports that appeared with NA
cod_wPorts %>% dplyr::filter(is.na(landing_lat)) %>% distinct(landing_port) 
#ports2 %>% View(., title="ports")
# no lat/lon for those 5 ports 

cod_wPorts %>% dplyr::filter(is.na(landing_lat))  
# 42 cases in this sample data 

# --> remove for now, check with Bjarki later?

cod_wPorts_noNA <- 
  cod_wPorts %>% 
  dplyr::filter(!is.na(landing_lat))  

#################################

# same with hauls

#--------------------------------------------------------------

# some hauls in raster cell on land (in NA) --> filter those out

# hauls <- test_error
hauls <- cod_wPorts_noNA

xy <- hauls %>% dplyr::select(x = lon, y = lat)

hauls_sp <-
  sp::SpatialPointsDataFrame(coords = xy, data = hauls, proj4string =  sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

vals2 <- raster::extract(r_test, hauls_sp) 
outside_mask2 <- is.na(vals2)
outside_pts2 <- hauls_sp[outside_mask2, ]

# plot these in blue
plot(r_test, xlim = c(-33, -5), ylim = c(60,73))
points(outside_pts2, pch = 16, col = 'blue')

DF <- as.data.frame(outside_pts2)

## use this as in sub-sample no hauls were "on land"
haul_distances <- cod_wPorts_noNA%>%
  dplyr::anti_join(DF)
#-------------------------

#------- create Transition Layer matrix




#filter out hauls that are too close to harbour
haul_distances <- haul_distances%>%
  #dplyr::select( y_lat, x_lon, landing_port, landing_lon, landing_lat)%>%
  distinct()%>%
  #filter(!(abs(y_lat-landing_lat)<0.1 & abs(x_lon-landing_lon)<0.1))%>%
  mutate(in_harbour = ifelse((abs(lat-landing_lat)<0.15 & abs(lon-landing_lon)<0.15), 1,0))%>%
  mutate(row_id=row_number())


#What to do, add haul id back, and filter hauls that are too close to harbour? check also how many points those are by not filtering, but a mutate, ifelse in harbour yes, otherwise no
#also use dataDEM clean or something, not the final clustering output so that you can make this part of the run all script

print(paste0(length(unique(haul_distances$haul_id[haul_distances$in_harbour==1]))," close to harbour and ",length(unique(cod_wPorts$haul_id))-length(unique(haul_distances$haul_id))," no lat and lon and ",length(unique(DF$haul_id)), " originally on land - hauls will be filtered"))

dataDEM=haul_distances%>%
  filter(in_harbour==0)


# rm(cod_wPorts_noNA)
# rm(cod_df_M)
# rm(cod_wPorts)
