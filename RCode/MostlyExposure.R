#################################################################
######################### EXPOSURE DATA #########################
#################################################################
# Exposure data is split into four main components:
#   1) Population & Demographic
#   2) Physical Infrastructural Assets
#   3) Financial/Economic Assets
#   4) Environmental & Cultural Assets
# 
#------------------1.Population & Demography------------------------#
source("./RCode/GetDemographics.R")

#------------------2.Physical Infrastructural Assets------------------# 
# GHS Built-Up Area
#amount of square meters of built-up surface in the cell.
GetInfra<-function(ADM,ISO,ext){
  Infra<-brick(paste0(dir,"/Data/Exposure/BuiltUpArea/GHS_BUILT_S_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
  projection(Infra)<-"+proj=longlat +datum=WGS84 +no_defs"
  Infra%<>%crop(ext)
  # Note we use bilinear interpolation because none of the none-zero values 
  # will be inside the admin boundaries
  ADM$BuiltUp<-Infra%>%raster::extract(ADM,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  
}


GetInfra_surface<-function(admin_poly){ #iso3c code per country, and a polygon at desired admin boundary level
  #---------------GHS_BUILT_S--------------------------------------------
  #https://ghsl.jrc.ec.europa.eu/download.php?ds=bu
  #data gives the amount of square meters of built-up surface in the cell
  #100m resolution
  #----------------------------------------------------------------------
  #read BuiltUp_S raster:
  r<-list.files(path="./Data/Exposure",pattern = "GHS_BUILT_S_E2020.*.\\.tif$",recursive=TRUE,full.names = TRUE) %>%
    terra::rast()
  #crs(r)<-CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs") 
  #extract values at L2; loop for each L2 polygon + test/check plot(?)
  
  #Check projection and make the same:
  crs1<-st_crs(r)
  crs2<-st_crs(admin_poly)
  
  if(!is.na(crs1) && crs1!=crs2){ #make crs2 same as crs1
    admin_poly<-st_transform(admin_poly, crs=st_crs(r))
  }
  b<-terra::crop(r,admin_poly)%>%
    terra::mask(., admin_poly)
  b[b==0]<-NA
  
  bsurf<- sum(values(b),na.rm = TRUE)#raster is already in 100 x 100 m or 1 ha.
  admin_poly%<>%mutate(BuiltUp_Area_Ha = unlist(bsurf))%>%
    st_drop_geometry()%>%
    dplyr::select(c("ADM2CD","BuiltUp_Area_Ha"))
  
  return(admin_poly)
}


GetInfra_area_per_class<-function(admin_poly){
  #---------GHS_BUILT_C_MSZ-------------------------
  #GHS Settlement Characteristics from https://ghsl.jrc.ec.europa.eu/ghs_buC2022.php
  #10m resolution,#large raster!
  #------------------------------------------------
  if(nrow(admin_poly)==0){
    bclass<- NULL
  }else{
    r <- list.files(path="./Data/Exposure",pattern = "GHS_BUILT_C_MSZ.*.\\.tif$",recursive=TRUE,full.names = TRUE) %>%
      terra::rast()  #crs(builtup_ch)<-CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs") 
   
    built_codes<-read.csv("./Data/Exposure/BuiltUpArea/GHS_BUILT_C_codes.csv", header=TRUE, stringsAsFactors = FALSE)
    
    #Check projection and make the same:
    crs1<-st_crs(r)
    crs2<-st_crs(admin_poly)
    
    if(!is.na(crs1) && crs1!=crs2){ #make crs2 same as crs1
      admin_poly<-st_transform(admin_poly, crs=st_crs(r))
    }
    #extract values at L2; loop for each L2 polygon + test/check plot(?)
    bch <- terra::crop(r,admin_poly)%>%
      terra::mask(., admin_poly)
    bch[bch==0]<-NA
    #calculate total surface area per built-up characteristic:
    bclass <-bch %>%
      terra::freq() %>% #count pixels per value; raster is categorical so each class
      mutate(built_area_hctre = count*(10*10)*0.0001)%>% #express value in hectares
      dplyr::select(c(value,built_area_hctre)) %>%
      rename("built_class_codes" = "value") %>%
      left_join(.,built_codes, by = c("built_class_codes"="Code")) %>%
      dplyr::select(-c(built_class_codes,GHS_BuiltUp_Description))%>%
      pivot_wider(names_from = GHS_BuiltUp_Charc_Shortname,values_from = built_area_hctre)
  }
  if(nrow(bclass)==0)
    bclass[1, ] <- NA    # Add empty row containing only NA value
  bclass%<>%as.data.frame
  
  return(bclass)
}






#--------------------3.Financial/Economic Assets------------------
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


#------------------------4.Environmental & Cultural Assets--------------
# https://www.eea.europa.eu/data-and-maps/data/global-land-cover-250m

# Land Cover CCI Climate Research Data Package (CRDP)
# C3S Global Land Cover products available for 2019 
# https://maps.elie.ucl.ac.be/CCI/viewer/download.php
# Using "./Data/Exposure/Ecological/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"

GetLandCover<-function(ADM,ISO,ext){
  # ecol<-brick(paste0(dir,"/Data/Exposure/Ecological/GLC2000_EU_250m.tif"))
  file<-paste0(dir,"/Data/Exposure/Ecological/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc")
  #LC<-nc_open(file)
  LdCov<-brick(file,varname="lccs_class")
  #nc_close(LC)
  #projection(LdCov)<-"+proj=longlat +datum=WGS84 +no_defs"
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


GetLCover_class_area<-function(admin_poly){
  #----------------------------------------------------------------------------------------         
  #Land cover area (in hectares) per class(as is or do we group them further, e.g. group all trees class?)
  #per admin level 2
  #------------------------------------------------------------------------------------
  library(ncdf4)
  file<-list.files(path= "./Data/Exposure/", pattern = "*LCCS",recursive = TRUE,full.names = TRUE)
  lcover<-raster(file,varname="lccs_class")
  #lc_codes
  lc_codes<-read.csv("./Data/Exposure/LandCover/CCI_LC_classes.csv", header=TRUE, stringsAsFactors = FALSE)%>%
    
  
  lc_adm_l2 <-terra::crop(lcover,admin_poly) %>%
    mask(., admin_poly)#mask LC raster to admin unit
  lc_classes<-lc_adm_l2 %>%
    terra::freq() %>%
    as.data.frame() %>%
    mutate(lc_area_hctre = count*(300*300)*0.0001)%>% #in hectares
    dplyr::select(c(value,lc_area_hctre)) %>%
    rename("lc_codes" = "value")%>%
    left_join(.,lc_codes, by = c("lc_codes" = "Value")) %>%
    dplyr::select(-c(lc_codes,LandCover_Description))%>%
    pivot_wider(names_from = Short_name, values_from = lc_area_hctre) 
}

