#Maartje Oostdijk LBHI 2024

#library(caret)
if(!require("tidyverse")) {install.packages("tidyverse")}
if(!require("ranger")) {install.packages("ranger")}
if(!require("tidymodels")) {install.packages("tidymodels")}
if(!require("DALEXtra")) {install.packages("DALEXtra")}
if(!require("vip")) {install.packages("vip")}



load("~/metier/metier-analysis/predictor_data/datanew.Rdata") #datanew2 contains variables without scaling
#load("~/metier/metier-analysis/predictor_data/datanew_sens.Rdata") #sensitivity with prices in Euro
#load("~/metier/metier-analysis/predictor_data/datanew3.Rdata") #including date and long & lat




DataInput = na.omit(DataInput)#remove NAs
head(DataInput)#check data

DataInput = DataInput%>%
  dplyr::select(-lat, -lon, -landing_lon, -landing_lat)%>%
  #dplyr::select(-landing_lon, -landing_lat)%>%#see how precise it gets including lat & long
  #dplyr::mutate(date=as.numeric(date))%>%#see how precise it gets including date
  distinct()#%>%
 # filter(year>2016)#sensitivity for prices due to strong increase value ISK

#piece of code to split data into how much they catch each of the metiers, tried but not used in the end
#summarize vessels by how big of a percentage they catch in each of the metiers
# tal <- DataInput %>%
#   dplyr::select(cluster, vessel_id) %>%
#   group_by(vessel_id) %>%
#   mutate(talt = n()) %>%
#   group_by(cluster, vessel_id) %>%
#   mutate(tal = n(), perc = tal / talt) %>%
#   distinct()
# 
# percs <- tal %>%
#   dplyr::group_by(cluster) %>%
#   dplyr::summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)), perc1 = quantile(perc, c(0.25, 0.5, 0.75)))
# 
# high <- percs %>%
#   filter(quantile == "75%")
# 
# tal <- tal %>%
#   left_join(high) %>%
#   mutate(high = ifelse(perc > perc1, 1, 0)) %>%
#   dplyr::select(cluster, vessel_id, perc, high)%>%
#   mutate(own_clust = "other")%>%
#   mutate(own_clust = case_when(high == 1 & cluster == "cluster 7" ~ "clust 7", TRUE ~ own_clust))%>%
#   mutate(own_clust = case_when(high == 1 & cluster == "cluster 5" & own_clust == "other" ~ "clust 5", TRUE ~ own_clust))%>%
#   mutate(own_clust = case_when(high == 1 & cluster == "cluster 6" & own_clust == "other" ~ "clust 6", TRUE ~ own_clust))%>%
#   mutate(own_clust = case_when(high == 1 & cluster == "cluster 1" & own_clust == "other" ~ "clust 1", TRUE ~ own_clust))
# 
# vessels <- sort(unique(tal$vessel_id))
# 
# clust_out <- purrr::map_df(vessels, function(i) {
#   dat <- tal[tal$vessel_id == i, ]
#   
#   nown_clust <- ifelse("clust 7" %in% dat$own_clust, "clust 7",
#                        ifelse("clust 5" %in% dat$own_clust, "clust 5",
#                               ifelse("clust 3" %in% dat$own_clust, "clust 3",
#                                      ifelse("clust 6" %in% dat$own_clust, "clust 6",
#                                             ifelse("clust 1" %in% dat$own_clust, "clust 1", "other")))))
#   
#   df <- data.frame(vessel_id = i, nown_clust = nown_clust)
#   return(df)
# })
# 
# 
# print(clust_out)
# 
# rename_metier <- function(name) {
#   str_replace(name, "cluster", "Métier")
# }
# 
# DataInput<- DataInput %>%
#   mutate(cluster = map_chr(cluster, rename_metier))%>%
#   distinct()

#set factors
DataInput$cluster=as.factor(DataInput$cluster)
DataInput$freezer= as.numeric(DataInput$freezer)
DataInput$OBJECTID_1= as.character(DataInput$OBJECTID_1)

DataInput= DataInput%>%
  dplyr::select(-vessel_id, -owner)

glimpse(DataInput)

#try something new
split <- rsample::initial_split(DataInput, strata = cluster, prop = 4/5)
train <- rsample::training(split)
test <- rsample::testing(split)


#random forest model
rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger", importance = "permutation") %>% #switched out impurity for permutation
  set_mode("classification")



#random forest workflow
rf_wflow_c <- 
  workflow() %>% 
  add_model(rf_model)%>%
  add_formula(
    cluster ~ .) 

#resample & fit rf model
cv <- rsample::vfold_cv(train, strata = cluster, v = 10)


hyper_parameters <- tune::tune_grid(rf_wflow_c, resamples = cv)

#calculate accuracy
collect_metrics(hyper_parameters)
#print accuracy
print(collect_metrics(hyper_parameters))
#select best hyperparameters
best_hp <- select_best(hyper_parameters, metric = "accuracy")
#select best workflow
best_workflow <- tune::finalize_workflow(rf_wflow_c, best_hp)
#sensitivity etc metrics
metrics <- metric_set(accuracy, sens, spec, roc_auc, kap)
#final fit based on selected metrics
final_fit <- last_fit(object = best_workflow, split = split, metrics = metrics)

final_fit%>% collect_metrics()

print(final_fit%>% collect_metrics())

best_model <- extract_model(fit(best_workflow, DataInput))#extract best model

#confusion matrix
cf = final_fit %>% 
  collect_predictions() %>% 
  conf_mat(truth = cluster, estimate = .pred_class)


#saveRDS(cf, "~/metier/metier-analysis/predictor_data/cf_new2.rds")
saveRDS(cf, "~/metier/metier-analysis/predictor_data/cf_new_sens.rds")

#variable importance
vips <-final_fit %>% 
 extract_fit_parsnip() %>% 
  vip(num_features = 20)

#saveRDS(vips, "~/metier/metier-analysis/predictor_data/vips_new2.rds")
saveRDS(vips, "~/metier/metier-analysis/predictor_data/vips_sens.rds")

final_fitted <- final_fit$.workflow[[1]]

#explainer
explainer_rf <-explain_tidymodels(
  #best_model, 
  final_fitted,
data = dplyr::select(train, -cluster), 
y = train$cluster,
label = "random forest"
)


#partial dependence information, top 20 predictors
pdp_rf <- model_profile(explainer_rf, 
                       variables = c("btemp", "depth", "vessel_length", "distance","triplength", "stemp","sal",  "HHI",  "homerange","CE_share", "exploration",
                                     "npp", "month", "Redfish_quota","prop_highwaves","Cod_quota", "Ling_quota", "Ling"))

#pdp_rf <- variable_profile(explainer_rf, 
                       # variables = c("depth", "stemp"))

pd_rf = as_tibble(pdp_rf$agr_profiles)

#saveRDS(pd_rf, "~/metier/metier-analysis/predictor_data/pd_rf_new2.rds")#continuous partial dependence profiles
saveRDS(pd_rf, "~/metier/metier-analysis/predictor_data/pd_rf_sens.rds")#continuous partial dependence profiles

pdp_rf2 <- variable_profile(explainer_rf, 
 variables = c("freezer", "OBJECTID_1"), variable_type='categorical')

pd_rf2 = as_tibble(pdp_rf2$agr_profiles)

#saveRDS(pd_rf2, "~/metier/metier-analysis/predictor_data/pd_rf_new22.rds")#discrete partial dependence profiles
saveRDS(pd_rf2, "~/metier/metier-analysis/predictor_data/pd_rf_new22_sens.rds")#discrete partial dependence profiles

