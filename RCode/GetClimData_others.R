##----------------------OTHER CLIMATE DATA SOURCES-----------------------
library(RCurl)
library(magrittr)
library(stringr)

#----------------SLR--------------------

#https://www.cen.uni-hamburg.de/en/icdc/data/ocean/ar5-slr.html#beschreibung

#
url = "ftp://ftp-icdc.cen.uni-hamburg.de/ar5_sea_level_rise/"
fil = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
  strsplit(., "\n") %>%
  unlist() %>%
  str_subset(pattern = "total*") #files to dl

dest <-"/media/coleen/DDrive/A_UNDRR_GRAF/Results/CDS/UNI_HAMB_SSH"
for (f in fil) {
  download.file(paste(url, f, sep = ""), paste(dest, "/", f,sep = ""))
}

##-------------NASA-NEX downscaled CMIP6 datasets global------------
clim_mod<-read.csv("/media/coleen/DDrive/A_UNDRR_GRAF/clim_models.csv", header=FALSE) %>%
  unlist() %>%
  toupper()

scen<-c("ssp585","ssp370","ssp245","ssp126","historical")
base_url <-"https://ds.nccs.nasa.gov/thredds/fileServer/AMES/NEX/GDDP-CMIP6"

var = c("pr", "tasmax", "tasmin")
yrs <-seq(2015,2100,1)

url.list <- list()
url.scen <- list()
url.data <-list()

for(i in seq_along(clim_mod)){ #level 1 for
  mod <-clim_mod[[i]]
  
    #for model variant:
    if(mod == "CNRM-CM6-1|CNRM-ESM2-1|GISS-E2-1-G|MIROC-ES2L|UKESM1-0-LL"){
      id <- "r1i1p1f2"
    } else if (mod == "HadGEM3-GC31-LL|HadGEM3-GC31-MM"){
      id <- "r1i1p1f3"
    } else if (mod == "CESM2-WACCM|FGOALS-g3"){
      id <- "r3i1p1f1"
    } else if (mod == "CESM2"){
      id <- "r4i1p1f1"
    } else {
      id <- "r1i1p1f1"
    }
  for(s in seq_along(scen)){ #level 2 forloop
    url_part <-paste(base_url,mod,scen[[s]],id,var, sep= "/")   
     
          for (v in seq_along(var)){ #level3 forlopp
            data_nam <- paste(var[[v]],"day",mod,scen[[s]],id, sep = "_")
            url.data[[v]] <- paste(url_part,data_nam,sep="/")
          }
    
    url.scen[[s]] <- url.data
  }
  url.list[[i]] <- url.scen
}

#data_nam <-paste(var,"_day_",mod,"_",scen,"_",id,"_gr1",yrs,".nc")  



