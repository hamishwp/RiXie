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

##------------------EMDAT-GDIS----------------------------------#
## Load GDIS data (rdata or other format)
load("/home/coleen/Documents/GitHub/RiXie/Data/Impact_losses/Disaster_database/GDIS/pend-gdis-1960-2018-disasterlocations.rdata")
## Load EMDAT data
load("/home/coleen/Documents/GitHub/RiXie/Data/Impact_losses/Disaster_database/EMDAT/EMDAT_disEvents.RData")


## for versions of EM-DAT data that added the ISO3 country code to the disasterno, use this code to remove the ISO3 code to enable merge with GDIS
## rename the variable "Dis No" from EMDAT and remove the three-letter ISO from the disasterno identifier
# emdat <- disasterlist%>%
#   mutate(disasterno=substr(`Dis No`,1,nchar(`Dis No`)-4)) 

## otherwise rename the variable "Dis No" from EMDAT without removing any characters
emdat <- disasterlist%>%
  mutate(disasterno=`Dis_No`)

#Rename columns for easy export later:
emdat_rename<-c("Dis_No","Year","Seq","Glide","Disaster_Group","Disaster_Subgroup","Disaster_Type","Disaster_Subtype","Disaster_Subsubtype","Event_Name","Country",
                "ISO",'Region',"Continent","Location","Origin","Associated_Dis","Associated_Dis2","OFDA_Response","Appeal","Declaration","AID_Contribution_1kUSD",
                "Dis_Mag_Value","Dis_Mag_Scale","Latitude","Longitude","Local_Time","River_Basin","Start_Year","Start_Month","Start_Day","End_Year","End_Month","End_Day",
                "Total_Deaths","No_Injured","No_Affected","No_Homeless","Total_Affected","Reconstruction_Costs_1kUSD","Reconstruction_Costs,_Adjusted_1kUSD","Insured_Damages_1kUSD",
                "Insured_Damages_Adjusted_1kUSD","Total_Damages_1kUSD","Total_Damages_Adjusted_1kUSD","CPI","Adm_Level","Admin1_Code","Admin2_Code","Geo_Locations","disasterno")

colnames(emdat)<-emdat_rename
## ____________________________________________________________________________________ ##
## Remove geography to ease data operations
disasterlocations <- GDIS_disasterlocations %>% 
  as_tibble() %>% 
  select(-geometry) %>%
  as.data.frame()

#Append ISO to disasterNo GDIS to match emdat:
disasterlocations$disasterno<- paste(disasterlocations$disasterno,disasterlocations$iso3,sep="-")


## add continent to the locations
disasterlocations$continent <- countrycode(sourcevar = disasterlocations[, "country"],
                                           origin = "country.name",
                                           destination = "continent")

disasterlocations <- disasterlocations%>%
  mutate(continent=ifelse(country=="Micronesia","Oceania",continent))%>%
  mutate(continent=ifelse(country=="Kosovo","Europe",continent))

## add info from EM-DAT (in this case year of disaster)
disloc_emdat<-left_join(disasterlocations,emdat, by= "disasterno") #can have multipel matching rows


#make spatial:
disloc_emdat_sf<-st_as_sf(disloc_emdat)


#remove ISO and iso3
disloc_emdat_sf%<>%dplyr::select(-c("iso3","ISO","gwno",'Continent','Country',"Region","geo_id","Dis_No"))


##ADD ISO from country:
source("./RCode/GeneralFunctions.R")

disloc_emdat_sf$ISO3<-convCountryIso3(disloc_emdat_sf$country)

disloc_emdat_sf %<>%
  dplyr::select(ISO3, everything())



#