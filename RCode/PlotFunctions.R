
PlotPopADM<-function(pop,ADM,zoomy=8){
  # Get background map
  mad_map <- get_stamenmap(ADM@bbox,source = "stamen",maptype = "terrain",zoom=zoomy)
  p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
  # Add population data (log)
  q<- p+ geom_raster(data=as.data.frame(pop),aes(x,y,fill=POPULATION),
                     interpolate = T, inherit.aes = FALSE) + coord_cartesian() +
    scale_fill_gradient2(low = "blue",mid="blue",high = "red",trans = "log",
                         # breaks=c(0,1,10,100),
                         na.value = "transparent")+#,limits=c(0,100)) +
    # labs(fill = "Displaced Pop. / Total Pop.")+xlab("Longitude") + ylab("Latitude");p
    labs(fill = "Population Density")+xlab("Longitude") + ylab("Latitude")
  
  # ADM@data$id<-1:nrow(ADM@data)
  # ADM%<>%fortify(region='id')
  # Add admin boundaries
  outer<-q+geom_sf(data=st_as_sf(ADM),mapping=aes(fill=ADM@data$POPULATION,colour=ADM@data$ADM2NM),
                   alpha=0,size=0.5,inherit.aes = F); outer
  
  return(outer)
  
}
