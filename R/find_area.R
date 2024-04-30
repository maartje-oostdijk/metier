find.area <- function(lat,lon){
  areas <- as.matrix(1:15)
  tmp <- apply(areas,1,
               function(x) geo::geoinside(data.frame
                                          (lat=lat,lon=lon),geo::reg.bc
                                          [[x]]))
  tmp <- sapply(tmp,function(x) rownames(x))
  x <- data.frame(area=rep(0,length(lat)),id=1:length(lat))
  for(a in areas){
    x$area[x$id %in% as.numeric(tmp[[a]])] <- a
  }
  return(x$area)
}
