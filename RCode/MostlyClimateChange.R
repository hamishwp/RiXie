#######################################################################
######################### CLIMATE CHANGE DATA #########################
#######################################################################
# ALL OF THIS SHOULD BE AUTOMATED USING
# https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html
# AND
# https://cran.r-project.org/web/packages/ecmwfr/ecmwfr.pdf

# Wrangle the NetCDF format data
CopNetCDF<-function(pather,fff,ADM,varname){
  SLR<-nc_open(paste0(pather,fff))
  RPS<-ncvar_get(SLR,varname)
  lonlat <- as.matrix(expand.grid(ncvar_get(SLR,"lon"),ncvar_get(SLR,"lat")))
  RPS<-data.frame(Longitude=(lonlat[,1]-180),Latitude=lonlat[,2],Value=as.vector(RPS[,,1]))
  nc_close(SLR)
  # Reduce the spatial dimension to only what we need
  ind<-RPS$Longitude>ADM@bbox[1]&
    RPS$Longitude<ADM@bbox[3]&
    RPS$Latitude>ADM@bbox[2]&
    RPS$Latitude<ADM@bbox[4]
  RPS<-RPS[ind,]
  return(RPS)
}
# Precipitation projections from Copernicus CDS
GetCCPrecip<-function(ADM,ISO,eyear=2050){
  pather<-paste0(dir,"/Data/ClimateChange/Copernicus_Precip/")
  filers<-list.files(path=pather,pattern="pr_Amon_EC-Earth3-CC_ssp585_r1i1p1f1_gr_")
  yearing<-sapply(1:length(filers),function(i) floor(extractnumbers(filers[i])[7]/100))
  syear<-yearing[which.min(abs(AsYear(Sys.Date())-yearing))]
  
  temporal<-data.frame()
  for (i in 1:length(filers)){
    fff<-filers[i]
    year<-yearing[i]
    RPS<-CopNetCDF(pather,fff,ADM,varname="pr")
    temporal%<>%rbind(data.frame(ISO3C=unique(ADM$ISO3CD),
                                 year=year,
                                 Precipitation=mean(RPS$Value,na.rm=T)))
  }
  
  ind<-which(yearing %in% c(syear,eyear))
    
  sRPS<-CopNetCDF(pather,filers[ind[1]],ADM,varname="pr")
  eRPS<-CopNetCDF(pather,filers[ind[2]],ADM,varname="pr")
  RPS<-sRPS; RPS$Value<-exp(eRPS$Value-sRPS$Value); rm(sRPS,eRPS)
  
  # Resample onto ADM using simple nearest neighbour
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$PrecipDiff<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  colnames(ADM@data)[ncol(ADM)]<-paste0("PrecipDiff",syear,"_",eyear)
  
  return(list(ADM=ADM,temporal=temporal))
}
# Temperature projections from Copernicus CDS
GetCCSurfTemp<-function(ADM,ISO,eyear=2050){
  pather<-paste0(dir,"/Data/ClimateChange/Copernicus_TempAtSurf/")
  filers<-list.files(path=pather,pattern="tas_Amon_EC-Earth3-CC_ssp585_r1i1p1f1_gr_")
  yearing<-sapply(1:length(filers),function(i) floor(extractnumbers(filers[i])[7]/100))
  syear<-yearing[which.min(abs(AsYear(Sys.Date())-yearing))]
  
  temporal<-data.frame()
  for (i in 1:length(filers)){
    fff<-filers[i]
    year<-yearing[i]
    RPS<-CopNetCDF(pather,fff,ADM,varname="tas")
    temporal%<>%rbind(data.frame(ISO3C=unique(ADM$ISO3CD),
                                 year=year,
                                 Temperature=mean(RPS$Value,na.rm=T)))
  }
  
  ind<-which(yearing %in% c(syear,eyear))
  
  sRPS<-CopNetCDF(pather,filers[ind[1]],ADM,varname="tas")
  eRPS<-CopNetCDF(pather,filers[ind[2]],ADM,varname="tas")
  RPS<-sRPS; RPS$Value<-eRPS$Value-sRPS$Value; rm(sRPS,eRPS)
  
  # Resample onto ADM using simple nearest neighbour
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$SurfTempDiff<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  colnames(ADM@data)[ncol(ADM)]<-paste0("SurfTempDiff",syear,"_",eyear)
  
  return(list(ADM=ADM,temporal=temporal))
}

# Total runoff projections from Copernicus CDS
GetCCTotRunoff<-function(ADM,ISO,eyear=2050){
  
  pather<-paste0(dir,"/Data/ClimateChange/Copernicus_TotRunoff/")
  filers<-list.files(path=pather,pattern="mrro_Lmon_EC-Earth3-CC_ssp585_r1i1p1f1_gr_")
  yearing<-sapply(1:length(filers),function(i) floor(extractnumbers(filers[i])[7]/100))
  syear<-yearing[which.min(abs(AsYear(Sys.Date())-yearing))]
  
  temporal<-data.frame()
  for (i in 1:length(filers)){
    fff<-filers[i]
    year<-yearing[i]
    RPS<-CopNetCDF(pather,fff,ADM,varname="mrro")
    temporal%<>%rbind(data.frame(ISO3C=unique(ADM$ISO3CD),
                                 year=year,
                                 TotRunoff=mean(RPS$Value,na.rm=T)))
  }
  
  ind<-which(yearing %in% c(syear,eyear))
  
  sRPS<-CopNetCDF(pather,filers[ind[1]],ADM,varname="mrro")
  eRPS<-CopNetCDF(pather,filers[ind[2]],ADM,varname="mrro")
  RPS<-sRPS; RPS$Value<-eRPS$Value-sRPS$Value; rm(sRPS,eRPS)
  
  # Resample onto ADM using simple nearest neighbour
  vals<-sapply(1:nrow(RPS), function(i){
        geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$TotRunoff<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  colnames(ADM@data)[ncol(ADM)]<-paste0("TotRunoff",syear,"_",eyear)
  
  return(list(ADM=ADM,temporal=temporal))
}
