agg_indi <- function(mydat = ptdat, var_family = indi_fam, dt_type = data_type, agg = agg_level) {
  
  if (var_family == 'water') {
  # mdg methods
  #levels <- c('piped', 'surface','imp','unimp','bottled','bottled_sp','bottled_wl','well_cw',
  #           'well_imp','well_unimp','spring_cw','spring_imp','spring_unimp')

  # sdg way
  levels <- c('piped', 'surface','imp','unimp','well_cw','well_imp','well_unimp',
              'spring_cw','spring_imp','spring_unimp')
  }
  
  if (var_family == 'sani') {
    levels <- c('imp', 'unimp','od','latrine_cw','latrine_imp','latrine_unimp')
  }
  
  if (var_family == 'hw') {
    levels <- c('hw_station','hw_unimp','hw_basic')
  }

  results <- list()
  for (i in levels) {
    
    # Subsetting data for hw_basic and hw_unimp to include obs with hw_stations
    if (i %in% c('hw_basic','hw_unimp') {
      mydat <- filter(mydat, hw_station != 0)
    }

    message(paste("Aggregating",i))
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    if (dt_type == 'pt') {
      if(agg == 'country') {
        
        mydatresults <- mydat %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size,
                                         eff_n_num = hhweight*hh_size, eff_n_denom = (hhweight^2)*hh_size) %>% 
          group_by(nid, iso3, survey_series, year_start) %>% 
          summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
                    total_hh = ((sum(eff_n_num))^2)/sum(eff_n_denom)) %>%
          mutate(urban = NA)
        
      } else {
        
          mydatresults <- mydat %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size,
                                           eff_n_num = hhweight*hh_size, eff_n_denom = (hhweight^2)*hh_size) %>% 
                          group_by(id_short, nid, iso3, lat, long, survey_series, urban, year_start, shapefile, location_code) %>% 
                          summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
                          total_hh = ((sum(eff_n_num))^2)/sum(eff_n_denom))
      }
    }
    
    if (dt_type == 'poly') {
      
      if(agg == 'country') {
        
        mydatresults <- mydat %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size,
                                         eff_n_num = hhweight*hh_size, eff_n_denom = (hhweight^2)*hh_size) %>% 
          group_by(nid, iso3, survey_series, year_start) %>% 
          summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
                    total_hh = ((sum(eff_n_num))^2)/sum(eff_n_denom)) %>%
          mutate(urban = NA)
        
      } else {
      
      mydatresults <- mydat %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size,
                                       eff_n_num = hhweight*hh_size, eff_n_denom = (hhweight^2)*hh_size) %>% 
                      group_by(id_short, nid, iso3, lat, long, survey_series, year_start, shapefile, location_code) %>% 
                      summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
                      total_hh = ((sum(eff_n_num))^2)/sum(eff_n_denom)) %>%
                      mutate(urban = NA)
      }
    }
    
    names(mydatresults)[which(names(mydatresults) == 'wtavg_indi')] <- paste0(i)
    results[[length(results)+1]] <- mydatresults
    names(mydat)[which(names(mydat) == 'indi')] <- i
    
    
  }
  
  message("Merging all results...")
  mydat <- Reduce(function(x,y) merge(x,y,all = T),results)
  
  return(mydat)
  
}
