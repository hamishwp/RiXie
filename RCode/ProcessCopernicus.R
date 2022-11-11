library(ncdf4)
library(terra)

#---------------------
#read downloaded data:
#---------------------
library(ncdf4) # package for netcdf manipulation
fil <- list.files("/home/coleen/Documents/GRAF_files/Results/CDS", full.names = TRUE, recursive = TRUE) %>%
  lapply(., raster) 


a<-nc_open(fil[[1]])

lon <- ncvar_get(a,"station_x_coordinate")
lat <- ncvar_get(a, "station_y_coordinate")
msl <- ncvar_get(a,"MSL")

stations <- ncvar_get(a,"stations")
time <-ncvar_get(a, "time")
