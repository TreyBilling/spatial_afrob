
get_africa <- function(level) {
  
  if(level == 2)
    afnames <- raster::ccodes() %>% 
      filter(continent == "Africa") %>% 
      dplyr::select(NAME) %>% 
      filter(NAME != "Western Sahara" & NAME != "Seychelles" & NAME != "Mauritius" & NAME != "Mayotte" & NAME != "Cape Verde" & NAME != "Comoros" & NAME != "French Southern Territories" & NAME != "Lesotho" & NAME != "Libya")
  else(afnames <- raster::ccodes() %>% 
         filter(continent == "Africa") %>% 
         dplyr::select(NAME))
  
  
  temp0 <- vector("list", 0)
  
  for(i in afnames$NAME) {
    print(i)
    temp0[[i]] <-  raster::getData(name = "GADM", 
                                   country = i, 
                                   download = T, 
                                   level = level, 
                                   path = here::here("shapefiles"))
  }
  
  list(temp0, makeUniqueIDs = T) %>% 
    purrr::flatten() %>% 
    do.call(rbind, .) %>% 
    st_as_sf()
  
  
}

