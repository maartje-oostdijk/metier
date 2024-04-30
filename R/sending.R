#' @title flat_logbook
#' @name flat_logbook
#' @param mar tenging vid mar
#'
#' @return flot afladagbok
#' @export
flat_logbook <- function(mar){
  afli_stofn(mar) %>% 
    #rename(veidarfaeri_nr = veidarf) %>% 
    left_join(tbl_mar(mar,'ops$bthe."gear_mapping"') %>% 
                dplyr::select(veidarf=veidarfaeri, gear_code = gear),by='veidarf') %>%
    left_join(afli_afli(mar) %>% dplyr::select(visir,tegund,afli=afli), by='visir') %>%
    left_join(fiskar_hafnir(mar) %>% 
                dplyr::rename(landing_lon = longitude) %>% 
                dplyr::rename(landing_lat = latitude) %>% 
                dplyr::rename(landing_port_name = name),
              by = c('lhofn'='port_no')) %>% 
    left_join(tbl_mar(mar,'ops$bthe."vessel_log_map"'), 
              by = c("visir","skipnr")) %>% 
    mutate(saga_nr = nvl(saga_nr,0)) %>% 
    left_join(skipaskra_saga(mar),by=c("skipnr"="skip_nr","saga_nr")) %>%
    left_join(tbl_mar(mar,'kvoti.adili'), by = c('rek_adili'='adila_nr')) %>%
    mutate(gridcell = 10*reitur + smareitur) %>%
    
    left_join(les_tegund(mar) %>% 
                dplyr::select(tegund_nr, heiti, enskt_heiti) %>% 
                dplyr::rename(tegund_heiti = heiti, 
                       tegund_enskt_heiti = enskt_heiti),
              by=c('tegund'='tegund_nr')) %>%
    
    left_join(afli_toga(mar) %>% dplyr::select(visir, togtimi), by=c('visir')) %>%
    left_join(mar::afli_lineha(mar) %>%
                dplyr::mutate(hooks = onglar*bjod, nr_net = dregin) %>%
                dplyr::select(visir,hooks, nr_net),
              by = 'visir') %>%
    mutate(orka = nvl(orka_velar_1,0) + nvl(orka_velar_2,0) + nvl(orka_hjalparvela,0)) %>%
    dplyr::select(haul_id=visir,date=vedags,year=ar,month=man,gridcell,vessel_id=skipnr,
           vessel_name = heiti,operator = nafn,op_cat,license_system,
           home_port = heimahofn_heiti,depth=dypi, 
           grt = brl, nrt = netto_rumlestir, gt = bruttotonn, 
           nt = nettotonn,vessel_length = skip_lengd, pwr = orka,
           gear_id=veidarf,#aeri_nr,
           gear_description=gear_code,#vf_enskt_heiti,
           crew_nr=ahofn,haul_lat=breidd,haul_lon=lengd,
           pull_lat=breidd_lok,pull_lon=lengd_lok,
           landing_date=ldags,landing_port=lhofn,
           landing_lon, landing_lat,species_id=tegund,
           species = tegund_enskt_heiti,towtime = togtimi,
           hooks, nr_net, catch = afli, smidaar)
}
