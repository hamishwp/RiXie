#######################################################################
######################### CLIMATE CHANGE DATA #########################
#######################################################################
# ALL OF THIS SHOULD BE AUTOMATED USING
# https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html
# AND
# https://cran.r-project.org/web/packages/ecmwfr/ecmwfr.pdf

# Wrangle the NetCDF format data
CopNetCDF<-function(pather,fff,ADM=NULL,varname,lon="lon",lat="lat"){
  SLR<-nc_open(paste0(pather,fff))
  RPS<-ncvar_get(SLR,varname)
  lonlat <- as.matrix(expand.grid(ncvar_get(SLR,lon),ncvar_get(SLR,lat)))
  RPS<-data.frame(Longitude=(lonlat[,1]-180),Latitude=lonlat[,2],Value=as.vector(RPS[,,1]))
  nc_close(SLR)
  if(is.null(ADM)) return(RPS)
  # Reduce the spatial dimension to only what we need
  ind<-RPS$Longitude>ADM@bbox[1]&
    RPS$Longitude<ADM@bbox[3]&
    RPS$Latitude>ADM@bbox[2]&
    RPS$Latitude<ADM@bbox[4]
  RPS<-RPS[ind,]
  return(RPS)
}

# Max Consequtive Dry Days
GetCDD<-function(ADM,ISO){
  pather<-paste0(dir,"/Data/ClimateChange/ClimateIndices/")
  fff<-"cddETCCDI_yr_EC-Earth3_ssp585_r1i1p1f1_no-base_v20200310_2015-2100_v2-0.nc"
  RPS<-CopNetCDF(pather,fff,ADM,varname="cddETCCDI")
  
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$MaxConsDryDays<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  return(ADM)
}

# Max Consequtive Wet Days
GetCWD<-function(ADM,ISO){
  pather<-paste0(dir,"/Data/ClimateChange/ClimateIndices/")
  fff<-"cwdETCCDI_yr_EC-Earth3_ssp585_r1i1p1f1_no-base_v20200310_2015-2100_v2-0.nc"
  RPS<-CopNetCDF(pather,fff,ADM,varname="cwdETCCDI")
  
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$MaxConsWetDays<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  return(ADM)
}

# Length of Growing Season
GetLGS<-function(ADM,ISO){
  pather<-paste0(dir,"/Data/ClimateChange/ClimateIndices/")
  fff<-"gslETCCDI_yr_EC-Earth3_ssp585_r1i1p1f1_no-base_v20200310_2015-2100_v2-0.nc"
  RPS<-CopNetCDF(pather,fff,ADM,varname="gslETCCDI")
  
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$LenGrowSeas<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  return(ADM)
}

# Annual Minimum of Daily Minimum Temperature
GetMinTemp<-function(ADM,ISO){
  pather<-paste0(dir,"/Data/ClimateChange/ClimateIndices/")
  fff<-"tnnETCCDI_yr_EC-Earth3_ssp585_r1i1p1f1_no-base_v20200310_2015-2100_v2-0.nc"
  RPS<-CopNetCDF(pather,fff,ADM,varname="tnnETCCDI")
  
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$AnnMinDayMinTemp<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  return(ADM)
}

# Annual Maximum of Daily Maximum Temperature
GetMaxTemp<-function(ADM,ISO){
  pather<-paste0(dir,"/Data/ClimateChange/ClimateIndices/")
  fff<-"txxETCCDI_yr_EC-Earth3_ssp585_r1i1p1f1_no-base_v20200310_2015-2100_v2-0.nc"
  RPS<-CopNetCDF(pather,fff,ADM,varname="txxETCCDI")
  
  vals<-sapply(1:nrow(RPS), function(i){
    geodist(ADM@data[,c("LONGITUDE","LATITUDE")],RPS[i,1:2])
  })
  ADM$AnnMaxDayMaxTemp<-RPS$Value[sapply(1:nrow(vals),function(i) which.min(vals[i,]))]%>%as.numeric()
  return(ADM)
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
    # Note that we convert to kilograms per metre per year
    temporal%<>%rbind(data.frame(ISO3C=unique(ADM$ISO3CD),
                                 year=year,
                                 Precipitation=mean(RPS$Value,na.rm=T)*60*60*24*365))
  }
  
  ind<-which(yearing %in% c(syear,eyear))
    
  sRPS<-CopNetCDF(pather,filers[ind[1]],ADM,varname="pr")
  eRPS<-CopNetCDF(pather,filers[ind[2]],ADM,varname="pr")
  RPS<-sRPS; RPS$Value<-eRPS$Value-sRPS$Value; rm(sRPS,eRPS)
  
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
    # Note that we convert from Kelvin to Celcius
    temporal%<>%rbind(data.frame(ISO3C=unique(ADM$ISO3CD),
                                 year=year,
                                 Temperature=mean(RPS$Value,na.rm=T)-273.15))
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
    # Note that we convert to kilograms per metre per year
    temporal%<>%rbind(data.frame(ISO3C=unique(ADM$ISO3CD),
                                 year=year,
                                 TotRunoff=mean(RPS$Value,na.rm=T)*60*60*24*365))
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

# Wrangle the NetCDF format data
MonthCopNetCDF<-function(pather,fff,ADM,varname,multiplier=1){
  SLR<-nc_open(paste0(pather,fff))
  RPS<-ncvar_get(SLR,varname)
  lon <- ncvar_get(SLR,"lon")
  lat <- ncvar_get(SLR,"lat")
  time<- ncvar_get(SLR,"average_T1") + 0.5*ncvar_get(SLR,"average_DT")
  nc_close(SLR)
  imnlo<-which.min(abs(lon-mean(ADM@data$LONGITUDE)))
  imnla<-which.min(abs(lat-mean(ADM@data$LATITUDE)))
  RPS<-RPS[imnlo,imnla,]
  RPS<-data.frame(ISO=unique(ADM@data$ISO3CD),
                  Variable=varname, Value=RPS*multiplier,
                  iMonth=AsMonth(as.Date("2006-01-01")+time),
                  Year=AsYear(as.Date("2006-01-01")+time))
  RPS$Month<-factor(RPS$iMonth,levels = 1:12,
                     labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
  RPS%>%group_by(Month)%>%summarise(ISO=ISO,Variable=varname,iMonth=iMonth,
                                    minVal=min(Value),
                                    maxVal=max(Value),
                                    meanVal=mean(Value),.groups="drop")
}

GetMonthlies<-function(ISO){
  ADM<-GetUNMaps(ISO)
  pather<-paste0(dir,"/Data/ClimateChange/MonthlyClimate/")
  # Total Runoff
  fff<-"mrro_Lmon_GFDL-ESM2M_rcp85_r1i1p1_201601-202012.nc"
  RPS<-MonthCopNetCDF(pather,fff,ADM,varname="mrro",multiplier=60*60*24*365)
  # Soil Moisture
  fff<-"mrsos_Lmon_GFDL-ESM2M_rcp85_r1i1p1_201601-202012.nc"
  RPS%<>%rbind(MonthCopNetCDF(pather,fff,ADM,varname="mrsos",multiplier=1/1000))
  # Precipitation
  fff<-"pr_Amon_GFDL-ESM2M_rcp85_r1i1p1_201601-202012.nc"
  RPS%<>%rbind(MonthCopNetCDF(pather,fff,ADM,varname="pr",multiplier=60*60*24*365))
  # Surface Wind Speed
  fff<-"sfcWind_Amon_GFDL-ESM2M_rcp85_r1i1p1_201601-202012.nc"
  RPS%<>%rbind(MonthCopNetCDF(pather,fff,ADM,varname="sfcWind",multiplier=1))
  # Near-Surface Air Temperature
  fff<-"tas_Amon_GFDL-ESM2M_rcp85_r1i1p1_201601-202012.nc"
  tmp<-MonthCopNetCDF(pather,fff,ADM,varname="tas")
  tmp[,5:7]<-tmp[,5:7]-273.15
  RPS%<>%rbind(tmp)
  return(RPS)
}








