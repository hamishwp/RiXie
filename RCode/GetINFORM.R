library(httr)
library(stringr)
library(dplyr)


##--------Functions---------------
GetINFORMid_year<-function(year){
  url<-paste0("https://drmkc.jrc.ec.europa.eu/inform-index/API/InformAPI/Workflows/GetByYear/",year)
  robj<-tryCatch(GET(url = url),error = function(e) NULL)
  if (robj$status_code!=200) stop(paste0("ERROR: INFORM id lookup table not found for ",year))
  
  return(content(robj,as = "parsed")[[1]][["WorkflowCompareId"]])
  
}


GetINFORMdata<-function(indicator,year=NULL,iso=NULL,normalise=F){
  # indicator= {INFORM, CC, VU, VU.SEV, CC.INF.PHY, } 
  # For more information, visit https://drmkc.jrc.ec.europa.eu/inform-index/In-depth/API-Demo
  
  #id<-GetINFORMid_year(year)
  id= 450 #id for INFORMmid 2022 #not sure how to systematically get the workflow id!!!
  indicator<-str_to_upper(gsub(" ", "", indicator, fixed = TRUE))
  
  url<-paste0("https://drmkc.jrc.ec.europa.eu/inform-index/API/InformAPI/countries/Scores/?WorkflowId=",id,"&IndicatorId=",indicator)
  
  robj<-tryCatch(GET(url = url),error = function(e) NULL)
  if (robj$status_code!=200) stop(paste0("ERROR: INFORM id lookup table not found for ",year))
  
  tmp<-content(robj,as = "parsed")
  
  #transformed into an if condition in case inform code does not exist
  if(length(tmp)==0){
    data = NULL
    #stop(paste0("ERROR: WB indicator not found '",indicator,"' see GetINFORMdata in GetINFORM.R for examples"))
  } else{
    data<-as.data.frame(t(matrix(unlist(tmp), nrow=length(unlist(tmp[[1]]))))); data<-data[,c(1,3)]; colnames(data)<-c("iso3",indicator)
    data[,2]<-as.numeric(data[,2])
  }
  
  if(normalise) data[,2]<-data[,2]/10
  
  if(is.null(iso)) return(data)
  return(filter(data,iso3==iso))
  
}


#Remove empty list function
delete.NULLs  <-  function(x.list){   # delete null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

#create vector of codes per category for specified level
IndiCodes<-function(codes_list, level=1){
  #codes_list<-CC
  if (level == 1){
    codes = unlist(codes_list[1]) 
  } else if(level == 2){
    c1 = unlist(codes_list[1])
    c2 = sapply(codes_list[[2]], function(x) paste(c1,x,sep="."))
    codes <- c(c1,c2)
  } else {
    c1 = unlist(codes_list[1])
    c2 = sapply(codes_list[[2]], function(x) paste(c1,x,sep="."))
    c3  = sapply(codes_list[[3]], function(x) paste(c2,x,sep=".")) #Some combinations will not exist but that's fine, will just delete nulls
    codes = c(c1,c2,c3)
  }
  
}  

#function to combine values per indicator category into a df
InformVals <- function(indicator,level){
  indicator.df <- IndiCodes(indicator, level=level) %>%
    lapply(., function(x) GetINFORMdata(x)) %>%
    delete.NULLs() %>%
    Reduce(function(x,y) merge(x, y, by = "iso3", all.x = TRUE, all.y = TRUE), .)
}



#Country Rankings - input is all_df with values
Inform_cRank <- function(data){
  len <- nrow(data)
  iso3 <- data$iso3
  data[data == 0]<-NA
  ranks <- apply(data[,-1], 2, function(x) ntile(desc(x), len)) %>% #descending, so rank 1 is highest risk
    data.frame(iso3, .)
  rank_class <- apply(data[,-1], 2, function(x) as.integer(ntile(x, 5))) %>%
    data.frame(iso3, .)
  
  inform_all <-list(data, ranks, rank_class)
  names(inform_all) <-c("Value","Rank", "Rank_class")
  
  
  return(inform_all)
}
# INFORMmean<-function(indicator,iso=NULL){
#   
#   INFORM<-data.frame(iso3=character(),variable=character(),value=numeric())
#   for (id in c(360,369,370,384,386)){
#     for(indy in unique(indicator)){
#       # Get 2020 value of the INFORM parameter
#       url<-paste0("https://drmkc.jrc.ec.europa.eu/inform-index/API/InformAPI/countries/Scores/?WorkflowId=",id,"&IndicatorId=",indy)
#       robj<-tryCatch(GET(url = url),error = function(e) NULL)
#       if (robj$status_code!=200) stop(paste0("ERROR: INFORM id lookup table not found for 2019"))
#       
#       tmp<-content(robj,as = "parsed")
#       if(length(tmp)==0) stop(paste0("ERROR: WB indicator not found '",indy,"' see GetINFORMdata in GetINFORM.R for examples"))
#       
#       # I know this is ugly... I can't be bothered to re-write it. Sorry!
#       data<-as.data.frame(t(matrix(unlist(tmp), nrow=length(unlist(tmp[[1]]))))); data<-data[,c(1,3)]; colnames(data)<-c("iso3","value")
#       data[,2]<-as.numeric(data[,2])
#       
#       if(!is.null(iso)) data%<>%filter(iso3%in%iso)
#       INFORM%<>%rbind(cbind(data,data.frame(variable=rep(indy,nrow(data)))))
#       
#     }
#     
#     INFORM$value<-INFORM$value/10
#   }
#   
#   INFORM%<>%group_by(iso3,variable)%>%summarise(value=mean(value,na.rm=T))
#   INFORM<-INFORM[,c(1,3,2)]
#   
#   return(INFORM)
# }
# 
# INFORM_LM<-function(iso,Indicators,DispData){
#   # I ASSUME ONLY CC.INS.DRR is missing
#   # I know... I'm sorry. So ugly
#   ind<-Indicators$iso3%in%unique(DispData$iso3[DispData$iso3%in%unique(Indicators$iso3[Indicators$iso3%in%unique(Indicators$iso3[Indicators$variable=="CC.INS.DRR"])])])
#   DF<-data.frame(DRR=Indicators$value[Indicators$variable=="CC.INS.DRR" & ind],
#                  PHY=Indicators$value[Indicators$variable=="CC.INF.PHY" & ind],
#                  GOV=Indicators$value[Indicators$variable=="CC.INS.GOV.GE" & ind],
#                  EQ=Indicators$value[Indicators$variable=="HA.NAT.EQ" & ind],
#                  VU=Indicators$value[Indicators$variable=="VU.SEV.AD" & ind],
#                  PD=Indicators$value[Indicators$variable=="VU.SEV.PD" & ind])
#   
#   nind<-Indicators$iso3%in%unique(DispData$iso3[DispData$iso3%in%unique(Indicators$iso3[!Indicators$iso3%in%unique(Indicators$iso3[Indicators$variable=="CC.INS.DRR"])])])
#   preddy<-data.frame(PHY=Indicators$value[Indicators$variable=="CC.INF.PHY" & nind],
#                      GOV=Indicators$value[Indicators$variable=="CC.INS.GOV.GE" & nind],
#                      EQ=Indicators$value[Indicators$variable=="HA.NAT.EQ" & nind],
#                      VU=Indicators$value[Indicators$variable=="VU.SEV.AD" & nind],
#                      PD=Indicators$value[Indicators$variable=="VU.SEV.PD" & nind])
#   
#   fit<-lm(data=DF,DRR~PHY+GOV+EQ+VU+PD)
#   predictor<-predict.lm(fit,newdata = preddy)
#   
#   Indicators%<>%as.data.frame()
#   Indicators%<>%rbind(data.frame(iso3=unique(Indicators$iso3[nind]),value=unname(predictor),variable=rep("CC.INS.DRR",length(predictor))))
#   
#   return(Indicators)
#   
# }
# 
# InterpINFORMdata<-function(indicator,dater,iso=NULL){
#   # indicator= {INFORM, CC, VU, VU.SEV, CC.INF.PHY, ...} 
#   # For more information, visit https://drmkc.jrc.ec.europa.eu/inform-index/In-depth/API-Demo
#   
#   # NOTE: 2016mid does not seem to exist...
#   WorkflowId<-data.frame(year=c(2015,2015.5,2016,2017,2017.5,2018,2018.5,2019,2019.5,2020), # This is the year stated for the INFORM index
#                          ID=c(193,211,258,261,354,360,369,370,384,386), # The Workflow ID used to access this particular year
#                          Ddate=as.Date(c("2014-10-22","2015-03-05","2016-05-03","2016-08-25",
#                                          "2017-03-13","2017-08-24","2018-03-28","2018-08-30",
#                                          "2019-03-29","2019-06-21"))) # The actual dates the data was released
#   mindate<-min(WorkflowId$Ddate)
#   WorkflowId%<>%mutate(day=as.numeric((Ddate-mindate)))%>%dplyr::select(-c(Ddate))
#   
#   INFORM<-data.frame(iso3=character(),variable=character(),value=numeric())
#   for(indy in unique(indicator)){
#     IndData<-data.frame(iso3=character(),value=numeric(),day=numeric())
#     for(id in unique(WorkflowId$ID)){
#       
#       url<-paste0("https://drmkc.jrc.ec.europa.eu/inform-index/API/InformAPI/countries/Scores/?WorkflowId=",id,"&IndicatorId=",indy)
#       
#       robj<-tryCatch(GET(url = url),error = function(e) NULL)
#       if (robj$status_code!=200) stop(paste0("ERROR: INFORM id lookup table not found for ",year))
#       
#       tmp<-content(robj,as = "parsed")
#       if(length(tmp)==0) stop(paste0("ERROR: WB indicator not found '",indy,"' see GetINFORMdata in GetINFORM.R for examples"))
#       
#       # I know this is ugly... I can't be bothered to re-write it. Sorry!
#       data<-as.data.frame(t(matrix(unlist(tmp), nrow=length(unlist(tmp[[1]]))))); data<-data[,c(1,3)]; colnames(data)<-c("iso3","value")
#       data[,2]<-as.numeric(data[,2])
#       
#       if(!is.null(iso)) data%<>%filter(iso3%in%iso)
#       data$day=rep(WorkflowId$day[WorkflowId$ID==id],nrow(data))
#       
#       IndData%<>%rbind(data)
#     }
#     
#     # Interpolate for actual day value
#     intp<-InterpDay(ndata = IndData,day = as.numeric(dater-mindate))
#     INFORM%<>%rbind(cbind(intp,data.frame(variable=rep(indy,nrow(intp)))))
#     
#   }
#   
#   return(INFORM)
#   
# }


#-------------Extracting the data/ Applying the functions starts here----------------
#Codes list with levels
INFORM<- list("INFORM")
CC <-list("CC", list("INS","INF"), list("DRR","GOV","COM","PHY","AHC"))
VU <- list("VU", list("SEV","VGR"), list("PD", "INQ","AD","UP", "OG"))
HA <- list( "HA" , list("NAT","HUM"), list("EQ","TS","FL","TC","DR","EPI","CON"))


Inform_codes_list <- list(HA, VU, CC)

Inform <-InformVals(INFORM, level=1)
Inform_all_df<-lapply(Inform_codes_list, InformVals,level=3) %>%
  lapply(., function(x) x[,c("iso3",sort(colnames(x[-1])))]) %>%
  Reduce(function(x,y) merge(x, y, by = "iso3", all.x = TRUE, all.y = TRUE) , .) 


Inform_all_df<- Reduce(function(x,y) merge(x, y, by = "iso3", all.x = TRUE, all.y = TRUE), list(Inform, Inform_all_df))


#add ranks
Country_with_ranks <- Inform_cRank(Inform_all_df)

##description for index or indicator codes----------
df<-data.frame(code = as.character(),Description = as.character())
codes <-list(c("HA", "NAT","HUM","EQ","TS","FL","TC","DR","EPI","CON"), c("VU","SEV","VGR","INQ","AD","UP", "OG"),
             c("CC","INS","INF","DRR","GOV","COM","PHY","AHC"))
Description <- list(c("Hazards & Exposure", "Natural", "Human","Earthquake","Tsunami","Flood","Cyclone","Drought",
                                 "Epidemic","Conflict Risk"),  c("Social-Economics Vulnerability","Vulnerable Groups","Poverty & Development",
                                                                 "Inequality", "Economic Dependence", "Uprooted People", "Other Vulnerable Groups"),
                    c("Lack of Coping Capacity", "Institutional", "Infrastructure","DRR Implementation", "Governance", "Communication",
                      "Phyiscal Connectivity", "Access to Healthcare"))
code_cat<-c("HA","VU","CC")
codes_list <-list()                      
for(i in seq_along(codes)){
  codes_list[[i]]<- data.frame(codes[[i]],Description[[i]])
  colnames(codes_list[[i]]) <- c("code","Description")
}

names(codes_list) <- code_cat


codes2label <- function(code){
  code = country_inform$Index[[3]]
   if(substr(code,1,2) =="HA"){
     list <- which(names(codes_list) == "HA")
     lab<- 
   }
  
}