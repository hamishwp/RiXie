library(ncdf4)
library(terra)
library(raster)
library(magrittr)
library(dplyr)
library(sf)

#--------------------------------
#Read climate data netcdf
#-------------------------------

var<- "MSL"
climfil <- list.files("/home/coleen/Documents/GRAF_files/Results/CDS",pattern = paste0("*",var), full.names = TRUE, recursive = TRUE)
 

#to get info from netcdf file:
a<-nc_open(climfil[[1]])
print(a)


lon <- ncvar_get(a,names(a$var)[grep("x_coord|longitude",names(a$var))])
lat <- ncvar_get(a, names(a$var)[grep("y_coord|latitude",names(a$var))])
var_nc <- ncvar_get(a,var)
stations <- ncvar_get(a,"stations")


#df to sf

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

latlon = data.frame(lon=lon, lat=lat) %>%
  mutate(var_nc = var_nc) %>%
  rename_at(vars(var_nc), ~var) %>%
  st_as_sf(., coords = c("lon","lat"),crs=projcrs)



#--------------------------------------------------------------
#read EEZ shapefile for countries to get coastal bounds
#from:https://www.marineregions.org/eez.php
#-------------------------------------------------------------
#ISO country:

iso<-"PHL"



eez <-"/home/coleen/Documents/GRAF_files/World_EEZ_v11_20191118/eez_v11.shp" %>%
  st_read()

#extract country bounds + 5k buffer:
land_buf5k <- eez %>% 
  filter(ISO_TER1 ==iso)%>%
  st_buffer(5000)
  
#points inside land? 
pts <- st_filter(latlon,land_buf5k)

#plot
plot(st_geometry(land_buf5k))
plot(st_geometry(pts),add=TRUE,col="red")



