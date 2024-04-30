#Maartje Oostdijk LBHI 2024
library(geo)
library(mapproj)

yr="2016-2019"

rename_metier <- function(name) {
  str_replace(name, "cluster ", "M")
}

dataDEM_plot <- dataDEM_clust %>%
  mutate(new_names_column = map_chr(cluster, rename_metier))%>%
  filter(!is.na(cluster))%>%
  dplyr::select(lat, lon, new_names_column, cluster, vessel_id)%>%
  distinct()

ggplot(data.frame(lon=NA,lat=NA),aes(lon,lat)) +
  ggtitle("Métiers spatial extent") +
  geom_polygon(alpha=1,data=gbdypi.100,col='gray90', fill=NA) +
  geom_polygon(data=gbdypi.200,alpha=1,col='gray90',fill=NA) +
  #geom_polygon(data=bc2,aes(group=area),alpha=0.000003,col='black', show.legend=F)+
  #geom_contour_filled(data=base_df, aes(x, y, z = Hour1), bins=9)
  geom_point(data=dataDEM_plot, aes(lon, lat,  colour=cluster), alpha=0.03, size=0.003) +
  #scale_fill_brewer(palette="Greens")+
  #geom_text(data=bc3, aes(x=lon, y=lat, label=quart), size=5, fontface="bold") +
  geom_polygon(data=island, fill="grey") +
  ylab("Latitude")+xlab("Longitude")+
  coord_map('mercator', xlim=c(-28,-8.5),ylim=c(62,67.5))+
  #scale_colour_manual(values=col_vector)+
  theme_classic()+facet_wrap(~new_names_column) + theme(axis.line=element_line())+
  theme(panel.spacing = unit(0, "lines"))+theme(legend.position="none")

ggsave(file = paste0(folder.pic,"metierssp.jpeg"),path=folder.pic, width = 5, height = 5, dpi=400)
