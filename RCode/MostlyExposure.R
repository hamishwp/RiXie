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


GetBuilt_surface<-function(ADM,ISO,ext){ #iso3c code per country, and a polygon at desired admin boundary level
  #---------------GHS_BUILT_S--------------------------------------------
  #https://ghsl.jrc.ec.europa.eu/download.php?ds=bu
  #data gives the amount of square meters of built-up surface in the cell
  #100m resolution
  #----------------------------------------------------------------------
  #read BuiltUp_S raster:
  r<-list.files(path="./Data/Exposure/BuiltUpArea/",pattern = "GHS_BUILT_S_E2020.*.\\_0.tif$",recursive=TRUE,full.names = TRUE) %>%
    terra::rast()
  #crs(r)<-CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs") 
  #extract values at L2; loop for each L2 polygon + test/check plot(?)
  
  #Check projection and make the same:
  admin_poly<-ADM
  crs1<-st_crs(r)
  crs2<-st_crs(admin_poly)
  
  if(!is.na(crs1) && crs1!=crs2){ #make crs2 same as crs1
    admin_poly<-st_transform(admin_poly, crs=st_crs(r))
    ext_tra <- st_transform(st_as_sf(ext), crs=st_crs(r))
  }
  ADM$b<-r%>%crop(ext_tra)%>%
    terra::extract(admin_poly,method='bilinear',fun=sum,na.rm=T,ID=FALSE) %>%
     unlist()
  names(ADM)[names(ADM) == "b"] <- "BuiltUp_Area_Ha"
  return(ADM)
}


GetBuilt_area_per_class<-function(ADM,ISO){
  #---------GHS_BUILT_C_MSZ-------------------------
  #GHS Settlement Characteristics from https://ghsl.jrc.ec.europa.eu/ghs_buC2022.php
  #10m resolution,#large raster!
  #------------------------------------------------
  if(nrow(ADM)==0){
    bclass<-NULL
  }else{
    r <- list.files(path="./Data/Exposure",pattern = "GHS_BUILT_C_MSZ.*.\\.tif$",recursive=TRUE,full.names = TRUE) %>%
      terra::rast()  #crs(builtup_ch)<-CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs") 
   
    built_codes<-read.csv("./Data/Exposure/BuiltUpArea/GHS_BUILT_C_codes.csv", header=TRUE, stringsAsFactors = FALSE)
    
    #Check projection and make the same:
    admin_poly<-ADM
    crs1<-st_crs(r)
    crs2<-st_crs(admin_poly)
 
    
    if(!is.na(crs1) && crs1!=crs2){ #make crs2 same as crs1
      admin_poly<-st_transform(admin_poly, crs=st_crs(r))
     }
    
    #extract values at L2; loop for each L2 polygon + test/check plot(?)
    if(nrow(ADM)>1){
      adm_list<-split(admin_poly, f=ADM[["ADM2NMCD"]])
      r_list <- lapply(adm_list, function(x) terra::crop(r,x))
    }else{
      r_list<-list(terra::crop(r,ADM))
    }
    
    #calculate total surface area per built-up characteristic:
    BClassXtract<-function(r){
      r[r==0]<-NA
      if(!any(is.na(values(r)))){
        class <-r %>%
          terra::freq() %>% #count pixels per value; raster is categorical so each class
          mutate(built_area_hctre = count*(10*10)*0.0001)%>% #express value in hectares
          dplyr::select(c(value,built_area_hctre)) %>%
          rename("built_class_codes" = "value") %>%
          left_join(.,built_codes, by = c("built_class_codes"="Code")) %>%
          dplyr::select(-c(built_class_codes,GHS_BuiltUp_Description))%>%
          pivot_wider(names_from = GHS_Built_Charc_shortname,values_from = built_area_hctre)
      } else{
        class <- NA
      }
      return(class)
   
    }
    
    bclass<-lapply(r_list, BClassXtract) 
    #checking for empty lists
    len<-sapply(bclass, length)
    emp<-which(len==0)
    
    
    if(length(emp)!=0){
      for(i in emp){
     mx<-which(len == max(len))[1]
     new<-bclass[[mx]]
     bclass[[i]]<-new
     bclass[[i]][!is.na(bclass[[i]])] <- NA
      }
    }
    bclass<-lapply(bclass,as.data.frame)
    bclass%<>%plyr::rbind.fill()
  }
  
  if(length(bclass)>=0 & any(!is.na(bclass))){ #!is.null(bclass)|
  ADM<-cbind(ADM,bclass)
  }
  
  return(ADM)
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
#Kummu dataset link:
#https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0 
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

GetGDPv2<-function(ADM,ISO=ISO){
 gdp2015<-terra::rast("./Data/Exposure/GDP/GDP_PPP_30arcsec_v3.hdf")[[3]] #3 raster - 1990, 2000, 2015
 #use epsg codes
 crs(gdp2015)  <- "epsg:4326"
 terra::ext(gdp2015) <- c(-180, 180, -90, 90)
 #set zeros to NA
 gdp2015 <- clamp(gdp2015, 0, values=FALSE)
 #caculate mean total GDP:
 x<-terra::extract(gdp2015,ADM, fun="mean", na.rm=TRUE,ID=FALSE)%>%
   unlist()%>%
   unname()
 ADM$GDP_PPP2015_mean<-x
 #names(ADM)[names(ADM) == "x"]<- "GDP_PPP2015_mean"
 return(ADM)
}


# plotGDP<-function(GDP,zoom=5){
#   mad_map <- get_stamenmap(GDP@bbox,source = "stamen",maptype = "toner",zoom=zoom)
#   p<-ggmap(mad_map) + xlab("Longitude") + ylab("Latitude")
#   p+geom_contour_filled(data = as.data.frame(GDP),
#                         mapping = aes(x,y,z=X2015),
#                         alpha=0.7)+ 
#     labs(fill = "GDP-PPP [USD-2015]")
# }


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


GetLCover_class_area<-function(ADM,ISO){
  #----------------------------------------------------------------------------------------         
  #Land cover area (in hectares) per class(as is or do we group them further, e.g. group all trees class?)
  #per admin level 2
  #------------------------------------------------------------------------------------
  library(ncdf4)
  file<-list.files(path= "./Data/Exposure/", pattern = "*LCCS",recursive = TRUE,full.names = TRUE)
  lcover<-raster(file,varname="lccs_class")
  #lc_codes
  lc_codes<-read.csv("./Data/Exposure/LandCover/CCI_LC_classes.csv", header=TRUE, stringsAsFactors = FALSE)
    
  #extract values at L2; loop for each L2 polygon + test/check plot(?)
  if(nrow(ADM)>1){
    adm_list<-split(ADM, f=ADM[["ADM2NMCD"]])
    r_list <- lapply(adm_list, function(x) terra::crop(lcover,x))
    }else{
      r_list<-list(terra::crop(lcover,ADM))
      }
  
  #calculate total surface area per lcover:
  LCXtract<-function(r){
    r[r==0]<-NA
    class <-r %>%
    terra::freq() %>%
    as.data.frame() %>%
    mutate(lc_area_hctre = count*(300*300)*0.0001)%>% #in hectares
    dplyr::select(c(value,lc_area_hctre)) %>%
    rename("lc_codes" = "value")%>%
    left_join(.,lc_codes, by = c("lc_codes" = "Value")) %>%
    dplyr::select(-c(lc_codes,LandCover_Description))%>%
    pivot_wider(names_from = Short_name, values_from = lc_area_hctre)
  }
  
  lclass<-lapply(r_list, LCXtract)
  if(length(lclass)==1){
    lclass%<>%as.data.frame()
  }else{
    lclass%<>%do.call(plyr::rbind.fill,.)
  }

  ADM<-cbind(ADM,lclass)
  return(ADM)
}

