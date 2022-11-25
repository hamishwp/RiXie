# Install packages if needed 
packages <- c("ecmwfr", "raster", "ncdf4", "stringr", "foreach","doParallel")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
# Load packages
lapply(packages , require, character.only = TRUE)
#https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html



#--------------------API--------------
#CDS API access/download


cds_api_dl <- function(cds_user,cds_key, target_path,req_list){
  service <- "cds"
  token <- wf_set_key(cds_user, cds_key, service)
  for(i in 1:length(req_list)){
    req <- req_list[[i]]
    dl <- wf_request(user = token,request = req ,transfer = TRUE,path = target_path,verbose =FALSE)
    #print(paste(years[i],"has been downloaded"))
  }
  }

#user API download params:
target_path <- "/media/coleen/DDrive/A_UNDRR_GRAF/Results/CDS"
user <- "161815" #coleen personal user ID #@RiX_climate! pass
key <- "b631611e-7dc1-4453-a598-f7aaac7a095b"


#----------------------DATASET 01---------------------------------------
#Global sea level change indicators from 1950 to 2050 derived
#from reanalysis and high resolution CMIP6 climate projections

#'sis-water-level-change-indicators-cmip6'


#API REQUEST parameters:

#function:
cds_wat_lev_indic<-function(variable, years, cds_user, cds_key, target_path, fil_desc){
  #creatting request list:
  req_list <- list()
  
  for(i in years){
    if (i >= 2015){
      exp <- "future"
      } else {
        exp <- "historical"
      }
    
  req_list[[i]] <- list(
    format = "zip",
    variable = variable, #download 1 variable per nc?
    derived_variable ='absolute_value',
    experiment = exp,
    period = i,
    target = paste0("CDS_",fil_desc,"_",i,".zip"),
    dataset_short_name = 'sis-water-level-change-indicators-cmip6'
  )
  }
  
  req_list<-Filter(Negate(is.null), req_list)
  
  #downloading each req:
  cds_api_dl(cds_user,cds_key, target_path,req_list)
}



#data params:
years<- sprintf("%04d",seq(2049,2050,1))
varia <-c("mean_sea_level","tidal_range")
desc<-"water_level_change"

#RUN function:
cds_wat_lev_indic(varia, years, user, key, target_path, desc)


#----------------------------DATASET 02---------------------
#Climate extreme indices and heat stress indicators
#derived from CMIP6 global climate projections
#Could be: ETCCDI or HSI
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-extreme-indices-cmip6?tab=form
#'sis-extreme-indices-cmip6'


#function
cds_extre_indic<-function(var_cat, var, period, cds_user, cds_key, target_path, fil_desc){
  if(var_cat == "ETCCDI"){
    product_type =  'base_independent'
    temporal_aggregation = "yearly"
    #period = c('185001-201412', '201501-210012')
    
  } else if(var_cat == "HSI") {
    product_type =  'bias_adjusted'
    temporal_aggregation = "daily"
    #period = c('19510101-20101231', '20110101-21001231')
  }
  
  req_list <- list()
  for(i in period){ #loop based on period
   
    req_list[[i]] <- list(
      version = "2_0",
      format = "zip",
      model = c('access_cm2', 'access_esm1_5', 'bcc_csm2_mr','canesm5', 'ec_earth3', 'ec_earth3_veg',
                'gfdl_cm4', 'gfdl_esm4', 'inm_cm4_8', 'inm_cm5_0', 'kace_1_0_g', 'kiost_esm',
                'miroc6', 'mpi_esm1_2_hr', 'mpi_esm1_2_lr','mri_esm2_0', 'nesm3', 'noresm2_lm','noresm2_mm'),
      variable = var, #download 1 variable per nc?
      ensemble_member ='r1i1p1f1',
      experiment = c('historical', 'ssp1_2_6', 'ssp2_4_5','ssp3_7_0', 'ssp5_8_5'),
      period = i,
      temporal_aggregation = temporal_aggregation,
      product_type = product_type,
      target = paste0("CDS_",fil_desc,"_",i,".zip"),
      dataset_short_name = 'sis-extreme-indices-cmip6'
    )
  }
  req_list<-Filter(Negate(is.null), req_list)
  
#downloading each req:
cds_api_dl(cds_user,cds_key, target_path, req_list)
}

#data params:
var <-c("maximum_5_day_precipitation","maximum_1_day_precipitation")
fil_desc<-"extr_indices"
var_cat <- "ETCCDI"
period = c('1850-2014', '2015-2100') #ETCCDI 1850-2014', '2015-2100',
period = c('19510101-20101231', '20110101-21001231') #HSI

#RUN function:
cds_extre_indic(var_cat,var,period, user, key, target_path, fil_desc)




##-------------------------DATASET 03-------------------------------------------------------
#Essential climate variables for assessment of climate variability from 1979 to present

#'ecv-for-climate-change'

#function
cds_ecv<-function(vars,years, cds_user, cds_key, target_path, fil_desc){
  
  req_list <- list()
  
  for(i in years){ #loop based on time
  
    req_list[[i]] <- list(
    version = "2_0",
    format = "zip",
    variable = vars,
    product_type = 'anomaly', #'climatology', 'monthly_mean'),
    time_aggregation = '1_month_mean',
    origin ='era5',
    month = sprintf("%02d",seq(1:12)),
    year = as.character(i),
    climate_reference_period = '1981_2010',# '1991_2020'),
    target = paste0("CDS_",fil_desc,"_",i,".zip"),
    dataset_short_name = 'ecv-for-climate-change'
    )
  }
  req_list<-Filter(Negate(is.null), req_list)
  
  #downloading each req:
  cds_api_dl(cds_user,cds_key, target_path, req_list)
  
}

#data params:
var <-c('0_7cm_volumetric_soil_moisture', 'precipitation', 'sea_ice_cover',
        'surface_air_relative_humidity', 'surface_air_temperature')
fil_desc<-"ecv"
years<- sprintf("%04d",seq(2000,2020,1)) #change as downloading data sometimes stops due to timeout.


#RUN function:
cds_ecv(var,years, user, key, target_path, fil_desc)

# 
# ##-------------------------DATASET 04-------------------------------------------------------
# #CMIP6 climate projections - selected variables, montlhy
# 
# # 'projections-cmip6'
# 
#    
# #function
# cds_cmip6<-function(vars, years,exp, cds_user, cds_key, target_path, fil_desc){
#   #subset years based on experiment:
#   if(exp == "historical"){
#     years <-years[years <= 2015]
#   } else{
#     years <- years[years >= 2015]
#   }
#   
#   req_list <- list()
#   for(i in years){ #loop based on time
#     req_list[[i]] <- list(
#       format = "zip",
#       variable = vars,
#       temporal_resolution = 'monthly',
#       experiment = exp,
#       model = j,
#       month = sprintf("%02d",seq(1:12)),
#       year = as.character(i),
#       target = paste0("CDS_",fil_desc,"_",j,"_",exp,"_",i,".zip"),
#       dataset_short_name = 'projections-cmip6'
#     )
#   }
# }
#   #clean
#   req_list<-Filter(Negate(is.null), req_list)
#   
#   #downloading each req:
#   #cds_api_dl(cds_user,cds_key, target_path, req_list)
# }
# 
# 
# 
# #data params:
# clim_mod<-read.csv("/media/coleen/DDrive/A_UNDRR_GRAF/clim_models.csv",header=FALSE)
# var <-'near_surface_air_temperature'
# years<- sprintf("%04d",seq(1950,2100,1))
# fil_desc <-"cmip6"
# 
# #lapply over experiment
# exp<- c("historical",'ssp1_2_6', 'ssp2_4_5', 'ssp3_7_0', 'ssp5_8_5')
# 
# 
# #RUN function:
# a<-lapply(exp, function(x) cds_cmip6(var,years, x, user, key, target_path, fil_desc))


##--------------DATASET 
