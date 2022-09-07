# Call all the required packages for RiXie from the GetPackages.R file
source("./RCode/GetPackages.R")

# Extract boundaries file (1st admin level)
ADM1<-as(sf::st_read("./Data/AdminBoundaries/UNmap0_shp/BNDA_A1.shp"),"Spatial")
ADM1 <- ADM1[ADM1@data$ISO3CD ==ISO, ]
ADM2<-as(sf::st_read("./Data/AdminBoundaries/UNmap0_shp/BNDA_A2.shp"),"Spatial")
ADM2 <- ADM2[ADM2@data$ISO3CD ==ISO, ]
saveRDS(list(ADM1=ADM1,ADM2=ADM2),paste0("./Data/",ISO,"_ADM.Rdata"))

bbox<-ADM1@bbox
e <- as(extent(bbox[c(1,3,2,4)]), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

# Using the @polygon component of a SpatialPolygonsDataFrame, gives the bounding box
RecalcBBOX<-function(polygons){
  coords<-matrix(ncol = 2)
  for (i in 1:length(polygons)){
    for (j in 1:length(polygons[[i]]@Polygons)){
      coords%<>%rbind(polygons[[i]]@Polygons[[j]]@coords)
    }
  }
  return(c(apply(coords,2,min,na.rm=T),apply(coords,2,max,na.rm=T)))
}
# Filter the admin boundary shapes by country or admin level
filterADM<-function(ADM,iso=NULL,adlev=NULL){
  
  if(is.null(iso) & is.null(adlev)) return(ADM)
  
  inds<-rep(TRUE,nrow(ADM))
  if(!is.null(iso))  inds<-inds & ADM$ISO3CD%in%iso
  # if(!is.null(adlev)) inds<-inds & ADM$
  
  ADM@data<-ADM@data[inds,]
  ADM@polygons<-ADM@polygons[inds]
  ADM@plotOrder<-ADM@plotOrder[inds]
  ADM@bbox[1:4]<-RecalcBBOX(ADM@polygons)
  
  return(ADM)
  
}

if(ISO=="MDV"){
  
  ADM2$AltName<-"Total"
  ADM2$AltName[ADM2$ADM1NM%in%c("Haa Alifu","Haa Dhaalu","Shaviyani")]<-"North"
  ADM2$AltName[ADM2$ADM1NM%in%c("Noonu","Raa","Baa","Lhaviyani")]<-"North Central"
  ADM2$AltName[ADM2$ADM1NM%in%c("Male'","Kaafu","Alifu Alifu","Alifu Dhaalu","Vaavu")]<-"Male"
  ADM2$AltName[ADM2$ADM1NM%in%c("Faafu","Dhaalu","Meemu")]<-"Central"
  ADM2$AltName[ADM2$ADM1NM%in%c("Thaa","Laamu","Gaafu Alifu","Gaafu Dhaalu")]<-"South Central"
  ADM2$AltName[ADM2$ADM1NM%in%c("Gnaviyani","Seenu")]<-"South"

  ADM2%<>%merge(SHDI,by="AltName")
  
}





