#Elzbieta Baranowska MFRI 2024

require(mar)
require(geo)

logbook_data= readRDS("~/final_metier/logbook_data.rds") #MO_needtocensor

mar <- connect_mar()
tyr <- lubridate::year(Sys.Date())

source('R/krafl.R')
source('R/sending.R')
source('R/find_area.R')
#source('R/Regions.R')

# Logbook data clean up ---------------------------------------------------------------------------------------
begyear <- 2016 #2000 ###remember to have the same beginning year here as in filtering years in cyclical_adjustment.R where I produce the dummy dataframe extra_col otherwise it will not fit with catchdata1 
endyear <- 2019


# logbook_data <- flat_logbook(mar) %>% 
#   dplyr::filter(year %in% c(local(begyear):local(endyear))) %>% #, lat < 70, lon < -5) %>% #lat lon filtering for demersal and years, changed 3 into 13 just for temp.
#   dplyr::filter(gear_id %in% c(1,2,3,5,6,7,9,10,12,14,15,26,38,39,40)) %>% 
#   dplyr::rename(lat=haul_lat, lon=haul_lon, production_year=smidaar)%>%
#   dplyr::select(haul_id,vessel_id,gear_id,date,month,year,species,species_id,catch,gridcell,lat,lon,vessel_length, depth, landing_date,landing_port, landing_lon, landing_lat, towtime, production_year)  %>%
#   dplyr::filter(!is.na(haul_id),!is.na(vessel_id),!is.na(gear_id),!is.na(month),!is.na(year),
#                 !is.na(species),!is.na(species_id),!is.na(catch),!is.na(gridcell),!is.na(lat),!is.na(lon), !is.na(landing_date)) %>% 
#   dplyr::mutate(season = ifelse(month %in% c(1,2,3), '1', 
#                                 ifelse(month %in% c(4,5,6), '2',
#                                        ifelse(month %in% c (7,8,9), '3', '4')))) %>% 
#   as.data.frame() %>% 
#   dplyr::mutate(gridcell = d2sr(lat,lon)) %>% ##producing gridcells by using lat/lon from geo package function. 
#   dplyr::left_join(tbl_bc4, by=c("gridcell")) %>%     # tbl_bc4 is produced in "Regions.R"
#   dplyr::filter(!is.na(bormicon.area)) %>%
#   dplyr::mutate(doy = lubridate::yday(date)) %>% 
#   dplyr::mutate(doy_end = lubridate::yday(landing_date)) %>% 
#   dplyr::mutate_at('bormicon.area', as.numeric)
#   
# glimpse(logbook_data)

# Trip duration ------------------------------------------------------------------------------------
# Need to rewrite this
# logbook_data %>% dplyr::select(haul_id,vessel_id,species_id,doy,doy_end,year, gear_id) %>% 
#   group_by(doy_end,vessel_id) %>% mutate(trip=doy_end-doy) %>% ungroup() %>% distinct(haul_id, trip, vessel_id, year, gear_id, doy, doy_end) -> trip_days

# Datasets by fleet---------------------------------------------------------------------------------

# Demersal trawls==================================================================================

dataDEM <-
  logbook_data %>% 
  filter(gear_id == 6 & lat < 70 & lat > 40 & lon > -35 & lon < 15, !vessel_id %in% c(1611,2430), depth > 9, depth < 600 ) %>% #this needs to be adjusted to the Iceandic EEZ area for demersal trawls
  mutate(oddYes = ifelse(species_id %in% c(10,11,16,17,29,30,31,33,34,36,39,41,43,44,45,46,48,49,55,62,93,95,97,99,130,143,155,161,191,199,689,830),'1','0')) %>% #here we take out "odd" species or species that mess up out
  group_by(haul_id)%>%
  filter(!any(species == 'Norway lobster'))%>%
  dplyr::filter(!haul_id %in% c('-1031057', '-1092370','-973306','-1110911')) %>% #very large hauls and '-893641' is a longliner #MO_needtocensor
  mutate(sum_tot = sum(catch))%>%
  group_by(haul_id, oddYes)%>%
  mutate(sum_odd_n = sum(catch))%>%
  mutate(perc_odd = sum_odd_n/sum_tot)%>%
  group_by(haul_id)%>%
  filter(!any(perc_odd>0.3 &oddYes==1))%>%
  filter(oddYes=="0")%>%
  ungroup()

removals <- 
  afli_stofn(mar) %>% 
  inner_join(les_stod(mar) %>% 
               left_join(les_syni(mar)) %>% 
               filter(synaflokkur_nr %in% c(30, 35)) %>% 
               dplyr::select(skip_nr,leidangur, synaflokkur_nr) %>% 
               distinct() %>% 
               left_join(les_leidangur(mar)), by = c('skipnr'='skip_nr') ) %>% 
  filter(between(vedags,brottfor,koma)) %>% 
  dplyr::select(haul_id = visir) %>% 
  collect(n=Inf)

dataDEM <-
  dataDEM %>% 
  dplyr::anti_join(removals)

glimpse(dataDEM)

rm(removals)

#check percentage catch "odd" species & filter those hauls that have more than 30% "odd" species


#dataDEM <-
# logbook_data %>% filter(gear_id == 6 & lat < 70 & lat > 40 & lon > -35 & lon < 15) %>%
# mutate(oddYes = ifelse(species_id %in% c(10,11,16,17,19,29,30,31,33,34,36,39,41,43,44,45,46,48,49,55,60,62,93,95,97,99,130,143,155,161,191,199,689,830),'1','0')) %>% #here we take out "odd" species or species that mess up out
# filter(oddYes == "0")  ## bottom trawl
# 
# 
# if(atlantis_logb){
#   print("run atlantis logs")
#   atlants = read.csv("~/metier/metier-analysis/predictor_data/atlantis_groups.csv")%>%
#     dplyr::select(-note)%>%
#     dplyr::filter(!(species==""|is.na(species)))
#   
#   dataDEM <-
#     logbook_data %>% filter(gear_id == 6 & lat < 70 & lat > 40 & lon > -35 & lon < 15)%>%
#     dplyr::left_join(atlants)
#   
#   dataDEM_NA=dataDEM%>%
#     dplyr::filter(is.na(group))
#   
#   print(unique(dataDEM_NA$species))#species that do not yet have a matching Atlantis group
#   
#   dataDEM <-dataDEM%>%
#     dplyr::filter(!is.na(group))%>%
#     dplyr::group_by(haul_id, group)%>%
#     dplyr::mutate(s_catch = sum(catch))%>%
#     dplyr::select(-catch, -species, -species_code, -species_id)%>%
#     dplyr::rename(catch= s_catch, species=group)%>%
#     dplyr::distinct()%>%
#     dplyr::group_by(species)%>%
#     dplyr::filter(n()>2)%>%
#     dplyr::ungroup()
#   
# }
# Demersal seine==================================================================================
# 
# dataDSE <-
#   logbook_data %>% filter(gear_id == 5 & lat < 70 & lat > 40 & lon > -35 & lon < 15) %>% 
#   mutate(oddYes = ifelse(species_id %in% c(55,10,29,45,40,41,36,16,191,31,44,46),'1','0')) 
# 
# # Longline==========================================================================================
# 
# logbook_data %>% filter(gear_id == 1 & lat < 70 & lat > 40 & lon > -35 & lon < 15) -> dataHOOK ##line
# 
# # Handline-Jiggers==================================================================================
# 
# logbook_data %>% filter(gear_id == 3 & lat < 70 & lat > 40 & lon > -35 & lon < 15) -> dataHAND ##line
# 
# # Gillnets==========================================================================================
# 
# logbook_data %>% filter(gear_id==2 & lat < 70 & lat > 40 & lon > -35 & lon < 15) -> dataGIL ##gillnet

# Dredges==========================================================================================
# 
# logbook_data %>% filter(gear_id %in% c(15,38,40)) -> dataDRE ##scallop dredge, cyprine dregde, sea urchins dredge

# Pelagic===========================================================================================

logbook_data %>% dplyr::filter(gear_id %in% c(7,10,12))  %>% 
  dplyr::mutate(oddYes = ifelse(species_id %in% c(25,26,41,49,97,191,199,15,28,29,47,48,8,4,12,23,24,173),'1','0')) %>% collect %>% 
  dplyr::filter(oddYes == "0")  %>% 
  dplyr::filter(!species_id==830) -> dataPEL2 ##mid-water trawl, herring seine, capelin seine



