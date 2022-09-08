######################################################################
######################### IMPACT & LOSS DATA #########################
######################################################################

# Extract historical hazard events:
GetGDACS<-function(ADM,ISO){
  dfGDACS<-FilterGDACS(iso3c = ISO)
  stop("Don't know what to do from here")
}

GetEMDAT<-function(ADM,ISO){
  iso3c<-ISO
  emdat<-xlsx::read.xlsx(paste0(dir,"/Data/Impact/emdat_public_2022_09_08.xlsx"),
                         sheetName = "emdat data",as.data.frame = T, startRow = 7)%>%
    filter(ISO==iso3c)
  tmp<-SpatialPointsDataFrame(as.numeric(data.frame(emdat[,c("Longitude","Latitude")])),
                              as.numeric(data.frame(emdat[,c("Total.Deaths",
                                       "No.Injured",
                                       "No.Homeless",
                                       "Total.Damages..Adjusted...000.US..")])))
  crs(tmp)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  
  emdat%>%group_by(Disaster.Subgroup,Disaster.Type)%>%
    summarise(deaths=sum(Total.Deaths,na.rm = T),
              injured=sum(No.Injured,na.rm = T),
              homeless=sum(No.Homeless,na.rm = T),
              costDollarAdj=sum(Total.Damages..Adjusted...000.US..,na.rm = T))
  
  # for(group in unique(emdat$Disaster.Subgroup)){
  #   
  #   
  # }
}


# historical storm surge hazard sizes:
# article:   https://www.nature.com/articles/s41597-021-00906-x#Sec8
# data can be downloaded here :    http://gssr.info/

# Predicted multi-hazard economic impact on road & railway transport infrastructure:
# https://www.nature.com/articles/s41467-019-10442-3