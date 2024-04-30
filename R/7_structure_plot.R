#Maartje Oostdijk LBHI 2024
atlmet <-dataDEM_clust  %>%
  dplyr::filter(!is.na(cluster))%>%
  dplyr::group_by(cluster, species)%>%
  dplyr::summarise(s_catch =sum(catch))%>%
  dplyr::mutate(metier = as.factor(cluster))%>%
  dplyr::mutate(pr = s_catch/sum(s_catch))%>%
  dplyr::mutate(species = ifelse(pr<0.05, "other", species))%>%
  dplyr::group_by(metier, species)%>%
  dplyr::summarise(pr = sum(pr))%>%
  dplyr::mutate(species = ifelse(species=="Cod", "Atlantic cod",
   ifelse(species=="Redfish", "Golden redfish", 
    ifelse(species=="Deepwater redfish", "Demersal beaked redfish", species))))%>%
mutate(species = factor(species, levels = c("Atlantic cod", 'Haddock',
 "Golden redfish", "Saithe",
 "Greenland halibut", "Plaice", "Greater argentine", "Demersal beaked redfish","other")))%>%
mutate(M=ifelse(metier=="cluster 1", "M2",
 ifelse(metier=="cluster 2", "M4",
        ifelse(metier=="cluster 3", "M1",
               ifelse(metier=="cluster 4", "M3",
                      ifelse(metier=="cluster 5", "M5",
                             ifelse(metier=="cluster 6", "M6", 
                                    ifelse(metier=="cluster 7", "M7"))))))))


#my.labels <- c("Métier 1", "Métier 2", "Métier 3", "Métier 4", "Métier 5", "Métier 6", "Métier 7")
my.labels <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")
#my.labels <- c("BTM1", "BTM2", "BTM3", "BTM4", "BTM5")

library(ggplot2)
library(viridis)

ggplot(atlmet, aes(x = metier, y = pr, fill = species, group=pr)) +
  geom_bar(stat = 'identity') +
  labs(x = "Métier", y = "Harvest Proportion",
       fill = "Species") +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_classic() +
  theme(text = element_text(size = 26)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.65)) +
  scale_x_discrete(labels = my.labels)+
  ggtitle("Métier proportions of species")

ggsave(file = paste0("structure_",yr,".jpg"), path = folder.pic, width = 12, height = 8, dpi=400)

tot=sum(dataDEM_clust$catch)
#summary stats
catch_p_cluster = dataDEM_clust%>% 
  dplyr::group_by(cluster)%>%
  dplyr::summarise(landings= sum(catch), perc_c_landings = sum(landings/tot*100))

df_summary <- dataDEM_clust %>%
  dplyr::group_by(haul_id) %>%
  dplyr::mutate(total_landings = sum(catch)) %>%
  dplyr::mutate(percentage = (catch/ total_landings) * 100) %>%
  mutate(dominant_species_present = max(percentage) > 75) %>%
  mutate(dominant_species_present = as.integer(dominant_species_present)) %>%
  mutate(dominant_species_present_5 = max(percentage) > 50) %>%
  mutate(dominant_species_present_5 = as.integer(dominant_species_present_5)) %>%
  dplyr::select(haul_id, species, catch, percentage, dominant_species_present, dominant_species_present_5)

tot7=sum(df_summary$catch[df_summary$dominant_species_present==1])
tot5=sum(df_summary$catch[df_summary$dominant_species_present_5==1])

tot7/tot
tot5/tot

hl7=length(unique(df_summary$haul_id[df_summary$dominant_species_present==1]))
hl5=length(unique(df_summary$haul_id[df_summary$dominant_species_present_5==1]))

hl=length(unique(df_summary$haul_id))

hl7/hl
hl5/hl


#fleet characteristics
vessels=dataDEM_clust%>%
  dplyr::select(vessel_id, vessel_length, production_year)%>%
  distinct()

mean(vessels$production_year[!is.na(vessels$production_year)])
