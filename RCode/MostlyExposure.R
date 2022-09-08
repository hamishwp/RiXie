#################################################################
######################### EXPOSURE DATA #########################
#################################################################
# Exposure data is split into four main components:
#   1) Population & Demographic
#   2) Physical Infrastructural Assets
#   3) Financial/Economic Assets
#   4) Environmental & Cultural Assets
# 
#@@@@@@@@@@@@@@@@@@@@@@ Population & Demography @@@@@@@@@@@@@@@@@@@@@@#
source(paste0(dir,"/RCode/GetDemographics.R"))

# GDP from Kummu et al 2018
FilterKummu<-function(GDP,bbox,melted=F){
  
  lat<-as.numeric(colnames(GDP))
  lon<-as.numeric(rownames(GDP))
  nlat<- length(lat)
  nlon<- length(lon)
  
  imnlon<-which.min(abs(lon-bbox[1]))
  if(imnlon>1) imnlon<-imnlon-1
  imxlon<-which.min(abs(lon-bbox[3]))
  if(imxlon<nlon) imxlon<-imxlon+1
  imnlat<-which.min(abs(lat-bbox[2]))
  if(imnlat>1) imnlat<-imnlat-1
  imxlat<-which.min(abs(lat-bbox[4]))
  if(imxlat<nlat) imxlat<-imxlat+1
  
  # GDP_PPP[longitude,latitude]
  if(!melted) return(GDP[imnlon:imxlon,imnlat:imxlat])
  GDP<-GDP[imnlon:imxlon,imnlat:imxlat]
  GDP<-melt(GDP);colnames(GDP)<-c("X","Y","data")
  return(GDP)
  
}

GetKummu<-function(dir,bbox=NULL,yr=2015L){
  
  iii<-yr-1989L
  
  # file<-paste0(dir,"Demography_Data/SocioEconomic/KUMMU/GDP_PPP_30arcsec_v3.nc")
  file<-paste0(dir,"/Data/Exposure/GDP/GDP_per_capita_PPP_1990_2015_v2.nc")
  GDP<-brick(file,varname="GDP_per_capita_PPP")
  GDP<-GDP[[iii]]
  
  if(!is.null(bbox)) {
    e <- as(raster::extent(c(bbox[c(1,3,2,4)])), 'SpatialPolygons')
    crs(e) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
    GDP%<>%raster::crop(e)
  }
  
  GDP%<>%as('SpatialPixelsDataFrame')
  names(GDP)[1]<-"GDP"
  return(GDP)
  
}

GetGDP<-function(ADM,ISO,ncores=2){
  GDP<-GetKummu(dir,bbox=c(ADM@bbox),yr=2015L)
  # Resample onto admin boundaries
  # Aggregate the population data to admin level 2
  GDP%<>%Grid2ADM(ADM,outsiders = F,
                   sumFn="mean",
                   index = 1,
                   ncores = ncores)
  # For many countries, the GDP data is only one value
  if(max(table(GDP))/length(GDP)>0.8) {
    warning(paste0("Only one GDP value was present in the Kummu dataset for ",convIso3Country(ISO)))
    GDP<-as.numeric(names(which.max(table(GDP))))
  }
  GDP<-GDP*InterpGDPWB(ISO,Sys.Date(),normdate=as.Date("2015-01-01"))$factor
  # Combine into one large data.frame
  ADM@data%<>%cbind(data.frame(GDP=GDP))
  return(ADM)
}

plotGDP<-function(GDP,zoom=5){
  mad_map <- get_stamenmap(GDP@bbox,source = "stamen",maptype = "toner",zoom=zoom)
  p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
  p+geom_contour_filled(data = as.data.frame(GDP),
                        mapping = aes(x,y,z=X2015),
                        alpha=0.7)+ 
    labs(fill = "GDP-PPP [USD-2015]")
}










