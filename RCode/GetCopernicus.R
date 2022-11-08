# Install packages if needed 
packages <- c("ecmwfr", "raster", "ncdf4", "stringr", "foreach","doParallel")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
# Load packages
lapply(packages , require, character.only = TRUE)


# https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html

#----------------------------
# API REQUEST
#----------------------------
period <- c('1951-1980', '1985-2014', '2021-2050')

req_list <- list()

for(i in period){
  if(grepl("2050", i) == TRUE){
    exp <- "future"
  }else {
    exp <- "historical"
  }
  
  req_list[[i]] <- list(
    format = "zip",
    variable = 'mean_sea_level',
    derived_variable ='absolute_value',
    experiment = exp,
    period = i,
    target = paste0("CDS_MSL_",i,".zip"),
    dataset_short_name = 'sis-water-level-change-indicators-cmip6'
    )
    
}

#-------------------------
# Download data
#------------------------
user <- "161815" #coleen personal user ID
key <- "b631611e-7dc1-4453-a598-f7aaac7a095b"
service <- "cds"

token <- wf_set_key(user, key, service)

for(i in 1:length(req_list)){
  
  req <- req_list[[i]]
  
  dl <- wf_request(user = token,request = req ,transfer = TRUE,path = "/home/coleen/Documents/GRAF_files/Results/CDS/",verbose =FALSE)
  print(paste(period[i],"has been downloaded"))
}
# 

#read downloaded data:
library(ncdf4) # package for netcdf manipulation
fil<- list.files("/home/coleen/Documents/GRAF_files/Results/CDS", full.names = TRUE )
cds.list<- lapply(fil, nc_open)




# #-------------------------
# # DOWNLOAD DATA PARALLEL
# #------------------------
# 
# uid <- "161815" 
# key <- "b631611e-7dc1-4453-a598-f7aaac7a095b"
# service <- "cds"
# 
# token <- wf_set_key(user, key, service)
# 
# cores <- detectCores()
# cl <- makeCluster(cores[1]-22)
# registerDoParallel(cl)
# 
# foreach(f = 1:length(req_list)) %dopar% {
#   
#   req <- req_list[[f]]
#   
#   dl <- ecmwfr::wf_request(user = token,
#                            request = req,   
#                            transfer = TRUE,  
#                            path = "./CDS/",
#                            verbose = FALSE)
# }
# stopCluster(cl)
# 
# 
# 
