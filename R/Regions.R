
# from Ella to get the overall 5 areas from the bormicon region and plot
# use the usual data (includes bormicon regions anyways)

# logbook_data2 <- logbook_data
# logbook_data2 %>% left_join(data.frame(bormicon.area=1:15, quart=c(2,3,3,3,4,4,5,5,5,1,6,7,8,9,10))) -> logbook_data3

# use geo package
#library(geo)

#table with lat, lon and bormicon stuff 
bc <-
  reg.bc %>%
  bind_rows(.id='area') %>%
  dplyr::mutate(area = area,order=1:n()) %>%
  dplyr::left_join(tibble(bc=1:10,       # define number of bormicon regions 
                       area=names(reg.bc)[bc],
                       quart=as.factor(c(2,3,3,3,4,4,5,5,5,1)))) %>%     # starts west and goes to north and then towards south
  arrange(order)

# bc is overall table 

bc2 <-
  reg.bc %>%
  bind_rows(.id='area') %>%
  dplyr::mutate(area = area,order=1:n()) %>%
  dplyr::left_join(tibble(bc=1:15,       # define number of bormicon regions 
                          area=names(reg.bc)[bc],
                       quart=as.factor(c(2,3,3,3,4,4,5,5,5,1,9,10,6,7,8)))) %>%     # starts west and goes to north and then towards south
  arrange(order)

#library(plyr)

bc3 = bc2%>%
  dplyr::group_by(area, bc, quart)%>%
  dplyr::summarise(lat = mean(lat), lon=mean(lon))


#bc3 <- plyr::ddply(bc2, .(area, bc, quart), function(x) c(lat = mean(x$lat), lon = mean(x$lon)))

# rearrange labels - group together the bormicon area numbers under the area number as we are grouping 10 bormicon into 5 areas. 
# 10  
bc3[16,4] <- 67
bc3[16,5] <- -28.5

#9
bc3[8,4] <- 61
bc3[8,5] <- -28.5

#8
bc3[11,4] <- 61.5
bc3[11,5] <- -16

#7
bc3[3,4] <- 67
bc3[3,5] <- -10

#6
bc3[4,4] <- 68
bc3[4,5] <- -22

#5
bc3[12,4] <- 63.7
bc3[12,5] <- -15.73123

#5
bc3[10,4] <- 64.25
bc3[10,5] <- -13.6

#4
bc3[1,4] <- 65.5
bc3[1,5] <- -12.7

#4
bc3[6,4] <- 66.8
bc3[6,5] <- -15.5

#3
bc3[2,4] <- 66.2
bc3[2,5] <- -19.6

#3
bc3[5,4] <- 67
bc3[5,5] <- -19.6

#2
bc3[15,4] <- 64.3
bc3[15,5] <- -24.82087

#1
bc3[13,4] <- 63.5
bc3[13,5] <- -21.54735

# plot
# library(mapproj)

col <- c("1"="#ffffd9", "2"="#edf8b1", "3"="#c7e9b4","4"="#7fcdbb","5"="#41b6c4","6"="#3690c0","7"="#0570b0","8"="#045a8d","9"="#88419d","10"="#8c6bb1")

# eff.plot <-
  ggplot(data.frame(lon=NA,lat=NA),aes(lon,lat)) +
  #tmp <- ggplot(subset(catch,as.character(year)> '2000'),aes
  geom_polygon(alpha=1,data=gbdypi.100,col='gray90') +
  geom_polygon(data=gbdypi.200,alpha=1,col='gray90') +
  # geom_polygon(data=bc,aes(group=area,fill=quart),alpha=1,col='black') +
  # geom_polygon(data=bc,aes(group=area,fill=quart),alpha=1,col='black') +
  geom_polygon(data=bc2,aes(group=area,fill=quart),alpha=1,col='black') +
  geom_text(data=bc3, aes(x=lon, y=lat, label=quart)) +
  # geom_text(aes(x=-25,y=64,label='W'),col='black',size=3) +
  # geom_text(aes(x=-23,y=66.8,label='NW'),col='black',size=3) +
  # #geom_text(aes(x=-19,y=66.3,label='4'),col='black',size=3) +
  # #geom_text(aes(x=-20,y=67,label='3'),col='black',size=3) +
  # #geom_text(aes(x=-15,y=67,label='5'),col='black',size=3) +
  # geom_text(aes(x=-14,y=66,label='NE'),col='black',size=3) +
  # geom_text(aes(x=-22.5,y=63.5,label='SW'),col='black',size=3) +
  # geom_text(aes(x=-15,y=63.7,label='SE'),col='black',size=3) +
  # #geom_text(aes(x=-17.5,y=63.5,label='9'),col='black',size=3) +
  #  geom_path(aes(-lon,lat),col='green', data = effort) +
  #  geom_path(aes(-lon,lat),col='green',data = effortMaxsea) +
  #  geom_path(aes(-lon,lat,fill=block),data=areal.block,col='green')) +
  geom_polygon(data=island,col='grey90') +
  #  geom_point() +
  # coord_map('mercator', xlim=c(-28,-11),ylim=c(62.5,68)) +
  coord_map('mercator', xlim=c(-30,-8.5),ylim=c(60,68.5)) +
  # scale_fill_brewer(palette="YlOrRd", direction = -1)+
  scale_fill_manual(values=col)+
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none")

# ggsave(file = paste0("/home/haf/ella/Doktor/Rcode/metier-analysis/plots/FS_package/15bormicon_overview.png"), width = 12, height = 8, dpi=400)
  

# #---------------------------------------------------------------------------------------------------
# 
# # get areas from Gadget models
# # https://gadget-framework.github.io/gadget-course/processing-input-data-with-mfdb.html#importing-data
# 
# # devtools::install_github('mareframe/mfdb', ref = '6.x')
# # library(mfdb)
# 
# 
# mdb <- mfdb('Iceland',db_params=list(host='mfdb.hafro.is')) #HAFGEIMUR GONE
# 
# 
# mfdb:::mfdb_taxonomy_tables
# 
# 
# geo::d2sr(lat = 66, lon = 20) 
# 
reitmapping <-
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)
# 
# names(reitmapping)[names(reitmapping)=="GRIDCELL"] <- "gridcell"
# 
# nrow(dataDEM2)
# #[1] 5500028
# nrow(reitmapping)
# #[1] 2992
# 
# regions <- left_join(dataDEM2, reitmapping)
# nrow(regions)
# # [1] 5500028

#---------------------------------------------------------------------

# install.packages('getPass')
# library(getPass)
# install.packages('git2r')
# library(git2r)

# devtools::install_git(
#   url="https://gitlab.hafogvatn.is/demersal-division/tidypax.git",
#   credentials = git2r::cred_user_pass("username", getPass::getPass()))

library(tidypax)
# library(statnet.common)

bormicon.area.map <- function(d){
  d %>%
    dplyr::mutate(division = nvl(division,0)) %>%
    dplyr::mutate(bormicon.area = case_when(division %in% c(101) ~ '2',  # W
                              division %in% c(102,103) ~ '3', # NW
                              division %in% c(104,105) ~ '4', # NE
                              division %in% c(107, 106) ~ '5', # SE
                              division %in% c(108) ~ '1',  # SW
                              division %in% c(109) ~ 'GR', # needs to be partitioned for 7 & 8
                              # division %in% c(110) ~ 'GR1',
                              division %in% c(111) ~ '9', # NW deep
                              # division %in% c(112) ~ 'GR3',
                              division %in% c(113) ~ '10', # NE deep
                              division %in% c(114) ~ '6', # SE/SW deep
                              # division %in% c(115) ~ 'FO2',
                              source == 'FO' ~ 'FO',
                              source == 'GR' ~ 'GR',
                              TRUE ~ 'Other'))
}


tbl_bc <- tbl_mar(mar,'ops$bthe."reitmapping"') %>%
  dplyr:: mutate(source = 'IS') %>%
  bormicon.area.map() %>%
  dplyr::collect(n=Inf) %>%
  #mutate(bormicon.area = forcats::fct_relevel(bormicon.area,'SE','SW','W','NW','NE','Other')) %>%
  #mutate(bormicon.area = as.factor(bormicon.area)) %>%
  #filter(bormicon.area!='Other') %>%
  arrange(bormicon.area) %>% data.frame()


# tbl_bc_W <- subset(tbl_bc, bormicon.area=="2")
# min_lat_W <- min(tbl_bc_W$lat)  # 62.875

bormicon.area_GR <- subset(tbl_bc, bormicon.area=="GR") 
bormicon.area_7 <- subset(bormicon.area_GR, lat < 62.875) 
bormicon.area_7$bormicon.area <- "7"

bormicon.area_8 <- subset(bormicon.area_GR, lat >= 62.875) 
bormicon.area_8 <- subset(bormicon.area_8, lon >= -30 ) 
bormicon.area_8$bormicon.area <- "8"

tbl_bc2 <- subset(tbl_bc, !bormicon.area=="GR")

tbl_bc3 <- rbind(tbl_bc2,bormicon.area_7,bormicon.area_8)
tbl_bc3 <- subset(tbl_bc3, !bormicon.area=="Other")

region_labels <-
  list(tibble(x=-25,y=65,label='W'),
       tibble(x=-25,y=66,label='NW'),
       tibble(x=-14,y=66,label='NE'),
       tibble(x=-22.5,y=63.5,label='SW'),
       tibble(x=-15,y=63.7,label='SE')) %>%
  bind_rows()

cols <- c("1"= "#999999","2"= "#E69F00","3"= "#56B4E9","4"= "#009E73","5"= "#F0E442", 
          "6"= "#756bb1","7"= "#bcbddc", 
          "8"="#dd1c77","9"='darkblue',"10"="red")


# bormicon.area.plot <-
# tbl_mar(mar,'ops$bthe."reitmapping"') %>%
#   mutate(source = 'IS') %>%
#   bormicon.area() %>%
#   collect(n=Inf) %>%
#   #mutate(bormicon.area = forcats::fct_relevel(bormicon.area,'SE','SW','W','NW','NE','Other')) %>%
#   #mutate(bormicon.area = as.factor(bormicon.area)) %>%
#   #filter(bormicon.area!='Other') %>%
#   arrange(bormicon.area) %>%
  tbl_bc3 %>% arrange(bormicon.area) %>%
  ggplot() +
  geom_raster(aes(lon,lat,fill=bormicon.area)) + 
  geom_polygon(data=map_data('worldHires','iceland'),aes(long,lat,group=group),col='black',fill='khaki') +
  geom_polygon(data=map_data('worldHires','greenland'),aes(long,lat,group=group),col='black',fill='khaki') +
  geom_polygon(data = geo::faeroes, aes(lon, lat), fill = 'khaki',col='black',lwd=0.1) +
  coord_quickmap(ylim = c(62,68), xlim = c(-45,-5)) +
  theme_light() +
  theme(legend.position = 'none',
        axis.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'),
        panel.spacing = unit(0,'lines')) +
  geom_label(data=region_labels,aes(x,y,label=label),col='black',size=3) +
  labs(y='',x='') +
  scale_fill_manual(values=cols)

tbl_bc4 <- tbl_bc3[,c('bormicon.area','gridcell')]
# str(tbl_bc4)
tbl_bc4$gridcell <- as.integer(tbl_bc4$gridcell)

