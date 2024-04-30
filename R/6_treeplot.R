#Elzbieta Baranowska MFRI 2024

### Treeplot ###
if(!require("RColorBrewer")) {install.packages("RColorBrewer")}
if(!require("clustree")) {install.packages("clustree")}
if(!require("patchwork")) {install.packages("patchwork")}



max_clusternumber <- 7

tmp2 <- data.frame(cutree(hc1_ward2, k=c(1:max_clusternumber)))
colnames(tmp2) <- gsub("X","K",colnames(tmp2))
tmp2$ship_ID <- rownames(tmp2)
# tmp2 %>% filter(!ship_ID=='-893641') -> tmp2
# Define the number of colors you want
# mycolors <- rev(colorRampPalette(brewer.pal(8, "YlOrRd"))(max_clusternumber))
clustree(x = tmp2, prefix = "K", node_size_range = c(3,15), node_label = "size", node_label_nudge=-0.4, show_axis=T) +
 theme(legend.position = "none")+
  # scale_fill_manual(values = rep("#bfcd8c",max_clusternumber))+
  labs(y= "Number of clusters k")

ggsave(file = paste0("treeclust_",yr,".jpg"), path = folder.pic, width = 12, height = 8, dpi=400)


# 
