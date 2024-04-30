#' @name kvoti_kvotategundir
#' @title kvoti_kvotategundir
#' @param mar tenging vid mar
#'
#' @return fyrirspurn
#' @export
kvoti_kvotategundir <- function(mar){
  tbl_mar(mar,'kvoti.kvotategundir') %>%
    dplyr::select(-c(snt,snn,sbt,sbn)) %>%
    left_join( tbl_mar(mar,'kvoti.kvotategundir_heiti')) %>%
    dplyr::select(-c(snt,snn,sbt,sbn))
}

#' @title londunarhofn
#' @name kvoti_stadur
#' @param mar tenging vid mar
#'
#' @return fyrirspurn
#' @export
kvoti_stadur <- function(mar){
  tbl_mar(mar,'kvoti.stadur') %>%
    dplyr::select(-c(snt:sbg))
}


#' @title Skipaskra saga
#' @name skipaskra_saga
#'
#' @param mar tenging vid mar
#'
#' @return skipasaga
#' @export
skipaskra_saga <- function(mar){
  tbl_mar(mar,'kvoti.skipasaga') %>%
    dplyr::rename(skip_lengd = lengd) %>%
    dplyr::select(-c(snt:sbn,gerandi)) %>%
    left_join(kvoti_utg_fl(mar), by = 'flokkur') %>%
    left_join(kvoti_stadur(mar) %>% dplyr::select(heimah=stad_nr,heimahofn_heiti=heiti), by = 'heimah') %>%
    left_join(tbl_mar(mar,'kvoti.skip_extra') %>%
                dplyr::select(skip_nr,smidaar:smidanumer,nettotonn:efnistegund,skrufufjoldi:orka_velar_2),
                by = 'skip_nr')
}



#' @title Operation categorization
#' @name kvoti_utg_fl
#'
#' @param mar connection to mar
#'
#' @return query of vessel and operation classification
#' @export
kvoti_utg_fl <- function(mar){
  tbl_mar(mar,'kvoti.utg_fl') %>%
    dplyr::mutate(license_system = case_when(veidikerfi == 'Erlend_skip'~'Foreign vessels',
                                      veidikerfi == 'Aflamark'~'Quota permit',
                                      veidikerfi == 'Soknardagar'~'Periodic permit',
                                      veidikerfi == 'Kafarar'~'Divers',
                                      veidikerfi == 'Krokaaflamark'~'Hook permit',
                                      veidikerfi == 'Aflamark'~'Quota_vessel',
                                      veidikerfi == 'Strandveidi'~'Coastal fishing',
                                      veidikerfi == 'Þangsl. og þaratekja'~'Seaweed harvesting',
                                      TRUE~'Non commercial license')) %>%
    dplyr::mutate(enskt_heiti = case_when( heiti == "Varðskip"~'Coast guard',
                                    heiti == "Hagræðingarsjóður"~"Obsolete vessels, quota transferred",
                                    heiti == "Erlend skip"~'Foreign vessel',
                                    heiti == "F-færslur"~"Storage pot",
                                    heiti == "Óvirkar veiðiheimildir"~"Inactive licences",
                                    heiti == "Tímabundnar aflamarksheimildir"~"Temporary quota permit",
                                    heiti == "Tímabundnar krókaheimildir"~"Temporary hook permit",
                                    heiti == "Frystitogari"~"Freezer trawler",
                                    heiti == "Raðsmíðaskip"~"Sequence build ship",
                                    heiti == "Togbátur"~"Trawler",
                                    heiti == "Síldarbátur"~"Herring boat",
                                    heiti == "Humarbátur"~"Nephrop boat",
                                    heiti == "Humar- og Síldarbátur"~"Herring and nephrop boat",
                                    heiti == "Rækjubátur við Eldey"~"Shrimp boat at Eldey grounds",
                                    heiti == "Rækjubátur í Arnarfirði"~"Shrimp boat in Arnarfjordur",
                                    heiti == "Rækjubátur í Djúpi og Húnaflóa"~"Shrimp boat in Isafjardardjup fjord and Hunafloi bay",
                                    heiti == "Skelbátur"~"Scallop boat",
                                    heiti == "Loðnuskip"~"Capelin boat",
                                    heiti == "Smábátur með meðalaflamark"~"Small boat with bycatch quota",
                                    heiti == "Línu- og handfærabátur"~"Jigger",
                                    heiti == "Án veiðil. í Ísl.lh."~"Without fishing license in Icelandic waters",
                                    heiti == "Þorskaflahámarksbátur"~"Cod quota boat",
                                    heiti == "Kafarar"~"Divers",
                                    heiti == "Handfærabátur"~"Jigger",
                                    heiti == "Smábátur með sóknardaga"~"Small boat with periodic permit",
                                    heiti == "Gildruveiðar"~"Trap fishing",
                                    heiti == "Frístundaveiðar - aflamark"~"Recreational fishing - w. quota",
                                    heiti == "Frístundaveiðar án aflaheimilda"~"Recreational fishing - w.o. quota",
                                    heiti == "Hugbúnaðarprófun"~"Software testing",
                                    TRUE ~ enskt_heiti)) %>%
    dplyr::rename(utg_fl = heiti) %>%
    dplyr::rename(op_cat = enskt_heiti) %>%
    dplyr::rename(utg_fl_aths = aths)
}

#fiskar_hafnir <- function(mar){
 # sql('select port_no, name, latitude, longitude from channel.port') %>% 
 #   tbl(mar,.) %>% 
 #   dplyr::mutate(TMP = 100*floor(LATITUDE/100), breidd = TMP/100 + ((LATITUDE - TMP)/60)) %>% 
 #   dplyr::mutate(TMP = 100*floor(LONGITUDE/100), lengd = TMP/100 + ((LONGITUDE - TMP)/60)) #%>% 
   # dplyr::mutate(breidd = 100*breidd,
   #          lengd = 100*lengd) %>% 
   #  mar:::geoconvert(col.names = c('lengd','breidd'))
#}


fiskar_hafnir <- function(mar){
  tbl_mar(mar,'channel.port_v') 
}


populate_vessel_map <- function(mar){
  dbRemoveTable(mar,'vessel_log_map')
  afli_stofn(mar) %>%
    select(visir,dags=vedags,skipnr) %>%
    left_join(tbl_mar(mar,'kvoti.skipasaga') %>%
                select(skipnr=skip_nr,saga_nr,i_gildi,ur_gildi)) %>%
    filter((dags > i_gildi & dags <= ur_gildi) | nvl(skipnr,-999)==-999 | nvl(i_gildi,to_date('01.01.2100','dd.mm.yyyy'))==to_date('01.01.2100','dd.mm.yyyy')) %>%
    select(visir,skipnr,saga_nr) %>%
    compute(name='vessel_log_map',temporary=FALSE)
  dbExecute(mar, 'grant select on "vessel_log_map" to public')
  
}
