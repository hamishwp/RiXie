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

# GHS Built-Up Area
GetInfra<-function(ADM,ISO,ext){
  Infra<-brick(paste0(dir,"/Data/Exposure/BuiltUpArea/GHS_BUILT_S_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
  projection(Infra)<-"+proj=longlat +datum=WGS84 +no_defs"
  Infra%<>%crop(ext)
  # Note we use bilinear interpolation because none of the none-zero values 
  # will be inside the admin boundaries
  ADM$BuiltUp<-Infra%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  
}

# https://www.eea.europa.eu/data-and-maps/data/global-land-cover-250m

# Land Cover CCI Climate Research Data Package (CRDP)
# C3S Global Land Cover products available for 2019 
# https://maps.elie.ucl.ac.be/CCI/viewer/download.php
# Using "./Data/Exposure/Ecological/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"
GetLandCover<-function(ADM,ISO,ext){
  # ecol<-brick(paste0(dir,"/Data/Exposure/Ecological/GLC2000_EU_250m.tif"))
  file<-paste0(dir,"/Data/Exposure/Ecological/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")
  LC<-nc_open(file)
  LdCov<-brick(file,varname="lccs_class")
  nc_close(LC)
  projection(LdCov)<-"+proj=longlat +datum=WGS84 +no_defs"
  LdCov%<>%crop(ext)%>%as("SpatialPixelsDataFrame")
  names(LdCov)<-"Class"
  classes<-read_csv(paste0(dir,"/Data/Exposure/Ecological/LandCoverClassesMapping.csv"),show_col_types = F)
  # Per grouping of land cover, add a layer
  for(grp in unique(classes$Group)){
    tmp<-LdCov; tmp@data$Class<-0
    tmp$Class[LdCov@data$Class%in%classes$Tag[classes$Group==grp]]<-1
    # Use in-polygons algorithm to make the average land cover per polygon
    ADM@data$tmp<-Grid2ADM(tmp,ADM,sumFn="mean",index=1,ncores=1,outsiders=F)
    # Name it
    colnames(ADM@data)[ncol(ADM)]<-grp
  }
  
  return(ADM)
  
}

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

GetKummu<-function(dir,ext=NULL,yr=2015L){
  
  iii<-yr-1989L
  
  # file<-paste0(dir,"Demography_Data/SocioEconomic/KUMMU/GDP_PPP_30arcsec_v3.nc")
  file<-paste0(dir,"/Data/Exposure/GDP/GDP_per_capita_PPP_1990_2015_v2.nc")
  GDP<-brick(file,varname="GDP_per_capita_PPP")
  GDP<-GDP[[iii]]
  
  if(!is.null(ext)) GDP%<>%raster::crop(ext)
  
  GDP%<>%as('SpatialPixelsDataFrame')
  names(GDP)[1]<-"GDP"
  return(GDP)
  
}

GetGDP<-function(ADM,ISO,ext,ncores=2){
  GDP<-tryCatch(GetKummu(dir,ext,yr=2015L),error=function(e) NA)
  if(any(is.na(GDP))) return(ADM)
  # Resample onto admin boundaries
  # Aggregate the population data to admin level 2
  GDP%<>%raster%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  # For many countries, the GDP data is only one value
  if(max(table(GDP))/length(GDP)>0.8) {
    warning(paste0("Only one GDP value was present in the Kummu dataset for ",convIso3Country(ISO)))
    GDP<-as.numeric(names(which.max(table(GDP))))
  }
  
  factor<-tryCatch(InterpGDPWB(ISO,Sys.Date(),normdate=as.Date("2015-01-01"))$factor,error=function(e) NA)
  GDP<-GDP*ifelse(is.na(factor),1,factor)
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










