library(tidyverse)
library(magrittr)
library(readxl)
AsYear<-function(date,red=F,limit=T){
  # handle the NAs
  indies<-is.na(date); date<-date[!is.na(date)]
  date%<>%as.Date
  if(!red) year<-as.numeric(format(date,"%Y"))
  else year<-as.numeric(format(date,"%y"))
  
  if(limit&any(year>as.numeric(format(Sys.Date(),"%Y")))) 
    year[year>as.numeric(format(Sys.Date(),"%Y"))]<-AsYear(Sys.Date())
  
  outs<-rep(NA,length(indies))
  outs[!indies]<-year
  
  return(outs)
}
convIso3Country<-function(iso3){
  countrycode::countrycode(sourcevar = iso3,
                           origin = "iso3c",
                           destination = "country.name",warn = F)
}

options(stringsAsFactors = FALSE)

# Had to add the CreationDate variable and delete an extra 'etime' which was not a date
full<-readxl::read_xlsx(path = "~/Downloads/tmp_290822.xlsx",sheet = "Official_RiX_DIIF_XData_Inv_0")
early<-readxl::read_xlsx(path = "~/Downloads/tmp_jul02_codenames(2).xlsx",sheet = "Official_RiX_DIIF_XData_Inv_0")
# Change the full database column names to match the coded names
finalnamers<-colnames(full)<-colnames(early)

# Extract the column names from ArcGIS export:
colnamers<-colnames(readxl::read_xlsx(path = "~/Downloads/Corrected_DB.xlsx"))
# Bloody readxl messes up the date columns, so we have to reinsert them:
datecolumns<-c("stime","entry_date","year_publication_dataset","startyear","endyear","etime","CreationDate","EditDate")
# First the early database
earlydates<-read_xlsx(path = "~/Downloads/earlydates.xlsx",
                              sheet = "Official_RiX_DIIF_XData_Inv_0",
                              col_types = "date")
early[,datecolumns]<-earlydates[,datecolumns]
# Then the recent one
fulldates<-read_xlsx(path = "~/Downloads/fulldates.xlsx",
                      sheet = "Official_RiX_DIIF_XData_Inv_0",
                      col_types = "date")
colnames(fulldates)<-colnames(earlydates)
full[,datecolumns]<-fulldates[,datecolumns]
rm(fulldates,earlydates)

# sum(duplicated(full$GlobalID))
# sum(duplicated(dplyr::select(rbind(early,full),english_name_dataset,org_name,url_dataset)))

FINAL<-(rbind(early,full))[!duplicated(dplyr::select(rbind(early,full),english_name_dataset,org_name,url_dataset),fromLast = T),]
rm(full,early)
# Replacing Anagha's SSD mistaken entry
FINAL[FINAL$ObjectID==1005,c("countries_spatial","concat_countries_spatial","final_countries_spatial")]<-"SSD"
FINAL<-FINAL[!FINAL$ObjectID%in%c(805,604,776,993,975,1007,1012,809,824,
                                  294,341,343,505,506,474,694,699,1245,1305,1316),]
colnames(FINAL)<-colnamers

# tmp<-FINAL[FINAL$english_name_dataset%in%FINAL$english_name_dataset[duplicated(FINAL[,c("english_name_dataset","org_name")])] &
#            FINAL$org_name%in%FINAL$org_name[duplicated(FINAL[,c("english_name_dataset","org_name")])],]
# openxlsx::write.xlsx(tmp,"~/Downloads/Duplicate_checker.xlsx")



# UNEDIT<-xlsx::read.xlsx("~/Downloads/tmp_bothnoedits.xlsx",sheetName = "Sheet1")
# EDIT<-xlsx::read.xlsx("~/Downloads/Dataset Validation.xlsx",sheetName = "Sheet1")
# 
# tmp<-data.frame(GlobalID=UNEDIT$GlobalID,
#                 Vulsub=UNEDIT$Please.select.the.vulnerability.sub.types,
#                 Impsub=UNEDIT$Please.select.the.impact.and.loss.data.sub.types,
#                 CCsub=UNEDIT$Please.select.the.climate.change.sub.types)
# tmp<-merge(EDIT,tmp,by="GlobalID")
# xlsx::write.xlsx(tmp,"~/Downloads/Corrected_DB.xlsx",showNA = F)
# 
# tmp<-UNEDIT%>%filter(!GlobalID%in%EDIT$GlobalID)
# xlsx::write.xlsx(tmp,"~/Downloads/LeftOut_DB_2.xlsx",showNA = F)
# 
# FINAL<-xlsx::read.xlsx("~/Downloads/Corrected_DB.xlsx",sheetName = "Sheet1")

choices<-readxl::read_xlsx("/home/hamishwp/Downloads/Official RiX-DIIF (Data Inventory Input Form)/Official RiX-DIIF (Data Inventory Input Form).xlsx",sheet = "choices")
choices$list_name%<>%as.character()
choices$name%<>%as.character()
choices$label%<>%as.character()

columner<-data.frame(coden=c("des_formattype_dataset",
                             "des_formatextension_dataset",
                             "des_cca_themes",
                             "cca_group",
                             "des_cca_group",
                             "des_continent_spatial",
                             "concat_countries_spatial",
                             "final_countries_spatial",
                             "des_final_countries_spatial",
                             "one_country_data",
                             "final_label_countries",
                             "des_hazardtype",
                             "des_hazardsubtype",
                             "des_hazardsubsubtype",
                             "des_exposuretype",
                             "des_exposuresubtype",
                             "exp_sdg",
                             "exp_jiaf",
                             "des_vultype",
                             "des_vulsubtype",
                             "vul_sdg",
                             "vul_jiaf",
                             "des_cctype",
                             "des_ccsubtype",
                             "cc_sdg",
                             "cc_jiaf",
                             "des_imptype",
                             "des_impsubtype",
                             "imp_sdg",
                             "imp_jiaf",
                             "fin_sdg",
                             "fin_jiaf",
                             "des_fin_sdg",
                             "des_fin_jiaf"),
                     
                     depend=c("What.kind.of.data.is.it.",
                              "What.is.the.format.extension.of.the.data.file.",
                              "Please.select.the.different.themes.that.this.dataset.s..could.be.used.to.analyse",
                              "Please.select.the.different.themes.that.this.dataset.s..could.be.used.to.analyse",
                              "cca_group",
                              "Please.select.which.continents.are.covered.by.this.data",
                              "Please.state.which.countries.are.included",
                              "",
                              "final_countries_spatial",
                              "",
                              "",
                              "Which.of.the.following.hazard.type.s..are.present.in.the.data.",
                              "Please.select.hazard.cluster.s..present.in.the.data",
                              "Please.select.specific.hazard.s..present.in.the.data",
                              "Which.of.the.following.exposure..population.or.asset..data.classifications.are.present.in.the.data.",
                              "Please.select.the.exposure.sub.types",
                              "Please.select.the.exposure.sub.types",
                              "Please.select.the.exposure.sub.types",
                              "Which.of.the.following.vulnerability.data.classifications.are.present.in.the.data.",
                              "Please.select.the.vulnerability.sub.types",
                              "Please.select.the.vulnerability.sub.types",
                              "Please.select.the.vulnerability.sub.types",
                              "Which.of.the.following.climate.change.data.classifications.are.present.in.the.data.",
                              "Please.select.the.climate.change.sub.types",
                              "Please.select.the.climate.change.sub.types",
                              "Please.select.the.climate.change.sub.types",
                              "Which.of.the.following.impact.and.loss.data.classifications.are.present.in.the.data.",
                              "Please.select.the.impact.and.loss.data.sub.types",
                              "Please.select.the.impact.and.loss.data.sub.types",
                              "Please.select.the.impact.and.loss.data.sub.types",
                              "",
                              "",
                              "fin_sdg",
                              "fin_jiaf"),
                     
                     fnc=c("mchoicelabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "columnlabel",
                           "tmchoicelabel",
                           "mchoicelabel",
                           "concatvector",
                           "",
                           "tmchoicelabel",
                           "",
                           "",
                           "mchoicelabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "columnlabel",
                           "columnlabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "columnlabel",
                           "columnlabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "columnlabel",
                           "columnlabel",
                           "mchoicelabel",
                           "mchoicelabel",
                           "columnlabel",
                           "columnlabel",
                           "",
                           "",
                           "tmchoicelabel",
                           "tmchoicelabel"),
                     
                     other=c("",
                             "If.other..please.provide.more.information",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "",
                             "If.you.can.t.find.the.hazard.type.subtype.you.were.looking.for..enter.it.here",
                             "If.you.can.t.find.the.hazard.type.subtype.you.were.looking.for..enter.it.here",
                             "If.you.can.t.find.the.exposure.or.asset.type.subtype.you.were.looking.for..enter.it.here",
                             "If.you.can.t.find.the.exposure.or.asset.type.subtype.you.were.looking.for..enter.it.here",
                             "",
                             "",
                             "If.you.can.t.find.the.vulnerability.type.subtype.you.were.looking.for..enter.it.here",
                             "If.you.can.t.find.the.vulnerability.type.subtype.you.were.looking.for..enter.it.here",
                             "",
                             "",
                             "If.you.can.t.find.the.climate.change.type.subtype.you.were.looking.for..enter.it.here",
                             "If.you.can.t.find.the.climate.change.type.subtype.you.were.looking.for..enter.it.here",
                             "",
                             "",
                             "If.you.can.t.find.the.impact.or.loss.type.subtype.you.were.looking.for..enter.it.here",
                             "If.you.can.t.find.the.impact.or.loss.type.subtype.you.were.looking.for..enter.it.here",
                             "",
                             "",
                             "",
                             "",
                             "",
                             ""),
                     
                     choicescol=c("label",
                                  "label",
                                  "label",
                                  "CCA",
                                  "label",
                                  "label",
                                  "",
                                  "",
                                  "label",
                                  "",
                                  "",
                                  "label",
                                  "label",
                                  "label",
                                  "label",
                                  "label",
                                  "SDGs",
                                  "JIAF",
                                  "label",
                                  "label",
                                  "SDGs",
                                  "JIAF",
                                  "label",
                                  "label",
                                  "SDGs",
                                  "JIAF",
                                  "label",
                                  "label",
                                  "SDGs",
                                  "JIAF",
                                  "",
                                  "",
                                  "label",
                                  "label"))

for (j in 1:nrow(columner)){
  
  if(columner$coden[j]=="one_country_data"){
    tmp<-str_split(FINAL$Please.state.which.countries.are.included,",")
    FINAL$one_country_data<-sapply(1:length(tmp), function(i) {
      ttt<-unlist(tmp[[i]]); ttt<-ttt[ttt!="" & !is.na(ttt) & !is.null(ttt)]; 
      if(length(ttt)==1) return(convIso3Country(unlist(tmp[[i]])))
      return("")
    })
    next
  }
  
  if(columner$coden[j]=="final_label_countries") {
    tmp<-cbind(FINAL$global_data,
               FINAL$regional_data,
               FINAL$multiple_country_data,
               FINAL$one_country_data)
    tmp[is.na(tmp)]<-""
    FINAL$final_label_countries<-unlist(sapply(1:nrow(tmp),
                                          function(i) {
                                            ttt<-unlist(str_split(tmp[i,],","))
                                            ttt<-sort(unique(ttt)); ttt<-ttt[ttt!=""]
                                            if(length(ttt)==0) return("")
                                            return(paste0(ttt,collapse = " "))
                                          }))
    next
  }
  
  
  if(columner$coden[j]=="fin_sdg"){
    tmp<-cbind(FINAL$exp_sdg,
               FINAL$vul_sdg,
               FINAL$cc_sdg,
               FINAL$imp_sdg)
    tmp[is.na(tmp)]<-""
    FINAL$fin_sdg<-sapply(1:nrow(tmp),
                          function(i) {
                            ttt<-unlist(str_split(tmp[i,],";"))
                            ttt<-sort(unique(ttt)); ttt<-ttt[ttt!=""]
                            return(paste0(ttt,collapse = ";"))
                          })
    next
  }
  
  if(columner$coden[j]=="fin_jiaf"){
    tmp<-cbind(FINAL$exp_jiaf,
               FINAL$vul_jiaf,
               FINAL$cc_jiaf,
               FINAL$imp_jiaf)
    tmp[is.na(tmp)]<-""
    FINAL$fin_jiaf<-sapply(1:nrow(tmp),
                          function(i) {
                            ttt<-unlist(str_split(tmp[i,],";"))
                            ttt<-sort(unique(ttt)); ttt<-ttt[ttt!=""]
                            return(paste0(ttt,collapse = ";"))
                          })
    next
  }
  
  if(columner$coden[j]=="final_countries_spatial") {
    tmp<-cbind(FINAL$concat_countries_spatial,
               FINAL$africa_spatial,
               FINAL$africa_spatial,
               FINAL$americas_spatial,
               FINAL$antarctica_spatial,
               FINAL$asia_spatial,
               FINAL$europe_spatial,
               FINAL$oceania_spatial,
               FINAL$contother_spatial)
    tmp[is.na(tmp)]<-""
    FINAL$final_countries_spatial<-sapply(1:nrow(tmp),
                 function(i) {
                   ttt<-unlist(str_split(tmp[i,],";"))
                   ttt<-sort(unique(ttt)); ttt<-ttt[ttt!=""]
                   return(paste0(ttt,collapse = ";"))
                 })
    next
  }
  # Make the exact same column, but concatenated (appropriate when using Survey123, but odd in this context!)
  if(columner$fnc[j]=="concatvector") {
    tmp<-str_split(FINAL[[columner$depend[j]]],pattern = ",")
    FINAL[[columner$coden[j]]]<-sapply(1:length(tmp), 
                                       function(i){
                                         ttt<-unlist(tmp[[i]]); ttt<-ttt[ttt!=""]
                                         if(all(is.na(ttt))|all(ttt=="")|length(ttt)==0) return("")
                                         ttt%<>%unique()%>%sort()%>%paste0(collapse = ";")%>%return()
                                       })
    next
  }
  
  if(columner$fnc[j]=="tmchoicelabel") splitter<-";" else splitter<-","
  
  stringies<-str_split(FINAL[[columner$depend[j]]],splitter)
  
  # Directly replace entries (risky...)
  FINAL[[columner$coden[j]]]<-sapply(1:length(stringies), function(i){
    
    if((  length(stringies[[i]])==0 | all(is.na(stringies[[i]])) | all(stringies[[i]]=="") ) & columner$other[j]!=""){
      if(!is.null(FINAL[[columner$other[j]]])){
        if(!is.na(FINAL[[columner$other[j]]][i])){
          return(paste0("Other - ",FINAL[[columner$other[j]]][i]))
        } else return("")
      } else return("")
    } else if (length(stringies[[i]])==0 | all(is.na(stringies[[i]])) | all(stringies[[i]]=="")) return("")
    
    tmp<-unique(as.character(na.omit(sort(stringies[[i]]))))
    tmp<-tmp[tmp!="" & !is.null(tmp)]
    
    output<-sapply(1:length(tmp),function(k) choices[[columner$choicescol[j]]][!is.na(choices$name) & choices$name==tmp[k]])
    
    if(is.null(output) | all(is.na(output)) | all(output=="") | length(output)==0){
      if(!is.null(FINAL[[columner$other[j]]])){
        if(!is.na(FINAL[[columner$other[j]]][i])){
          return(paste0("Other - ",FINAL[[columner$other[j]]][i]))
        } else return("")
      }
    } 
    
    output%<>%unlist()%>%unique()%>%sort()
      
    if(columner$other[j]!="" & !is.null(FINAL[[columner$other[j]]])){
      if(!is.na(FINAL[[columner$other[j]]][i])){
        output%<>%c(paste0("Other - ",FINAL[[columner$other[j]]][i]))
      } 
    }
    
    if(columner$fnc[j]=="columnlabel"){
      output%>%paste0(collapse = ";")%>%str_split(";")%>%unlist()%>%unique()%>%sort()%>%paste0(collapse = ";")%>%return()
    } else output%>%paste0(collapse = ", ")%>%return()
      
  })
}
rm(tmp,stringies)
# Reintroduce column names
colnames(FINAL)<-finalnamers
# Sometimes the dates get messed up, not sure why...
for(daters in c("stime","entry_date","etime","CreationDate","EditDate"))  
  FINAL[is.na(FINAL[,daters]),daters]<-as.POSIXct("2022-08-29 08:10:00 UTC")  

FINAL$startyear[is.na(FINAL$startyear)]<-as.POSIXct("1900-01-01 18:30:00 UTC")
FINAL$endyear[is.na(FINAL$endyear)]<-NA

dessies<-data.frame(res=c("des_pub_year","des_start_year","des_end_year"),
                    inp=c("year_publication_dataset","startyear","endyear"))
for(i in 1:3) FINAL[,dessies$res[i]]<-AsYear(FINAL[,dessies$inp[i]])

class(FINAL$formduration)<-"numeric"; FINAL$formduration[is.na(FINAL$formduration)]<-0

FINAL$user[is.na(FINAL$user)]<-"hamish.patten_undrr"

FINAL$ObjectID<-1:nrow(FINAL)
# Save it out!
openxlsx::write.xlsx(FINAL,"~/Downloads/JSCorrected_DB_4ESRI_01092022.xlsx",showNA = F)
