library(countrycode)
library(raster)
library(geosphere)

library(rworldmap)
library(rworldxtra)
library(sp)

url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}

checkurls<-function(urls){
  
  sitecheck<-sapply(urls,url_exists)
  if(any(is.na(sitecheck))) warning(paste0(urls[is.na(sitecheck)]))
  
}

AsYear<-function(date,red=F,limit=T){
  date%<>%as.Date
  if(!red) year<-as.numeric(format(date,"%Y"))
  else year<-as.numeric(format(date,"%y"))
  
  if(limit&any(year>as.numeric(format(Sys.Date(),"%Y")))) 
    year[year>as.numeric(format(Sys.Date(),"%Y"))]<-AsYear(Sys.Date())
  
  return(year)
}
AsMonth<-function(date){
  return(as.numeric(format(date,"%m")))
}
# Used to save output files by date and time for model validation comparison in time
DateTimeString<-function(){
  return(gsub(gsub(Sys.time(),pattern = " ", replacement = "_"),pattern = ":",replacement = ""))
}

CheckBbox<-function(bbox,correct=TRUE){
  # bbox should be [min_longitude, min_latitude, max_longitude, max_latitude]
  if(abs(bbox[2])>90 | abs(bbox[4])>90 | abs(bbox[1])>180 | abs(bbox[3])>180) {
    stop("Error: non-physical bounding box values given to IIDIPUS")
  }
  
  if (bbox[1]>bbox[3]) {
    print("WARNING: bounding box range is long{-180,180}, lat{-90,90} with bbox[min_lon,min_lat,max_long,max_lat]")
    print("Found min_long>max_long")
    if(correct){
      tmp<-bbox[3]
      bbox[3]<-bbox[1]
      bbox[1]<-tmp
    }
  }
  if (bbox[2]>bbox[4]) {
    print("WARNING: bounding box range is long{-180,180}, lat{-90,90} with bbox[min_lon,min_lat,max_long,max_lat]")
    print("Found min_lat>max_lat")
    if(correct){
      tmp<-bbox[4]
      bbox[4]<-bbox[2]
      bbox[2]<-tmp
    }
  }
  
  return(bbox)
}

histosplitter<-function(vals,rmblank=F){
  vals%<>%strsplit(split = ",",fixed = T)%>%unlist()
  if(rmblank) vals%<>%str_remove_all(" ")
  return(vals)
}

areaBbox<-function(bbox){
  s1<-cbind(lon=c(bbox[1],bbox[3],bbox[1],bbox[3]),lat=c(bbox[2],bbox[2],bbox[4],bbox[4]))
  sp1 <- spPolygons(s1, crs="+proj=longlat +datum=WGS84")
  mp1 <- makePoly(sp1, interval=100000)
  return(areaPolygon(mp1)*1e-3)
  # R<-6378.137
  # return((pi/180)*R^2 *abs(sin(bbox[2])-sin(bbox[4]))*abs(bbox[1]-bbox[3]))
}

checkMatlonglat<-function(array){
  
  long<-as.numeric(rownames(array))
  lat<-as.numeric(colnames(array))
  
  if(!(any(is.na(long)) | any(is.na(lat)))) {
    colnames(array)<-lat
    rownames(array)<-long
    
    array%<>%reshape2::melt()
    
    return(array)
  }
  
  ladiff<-median(diff(lat),na.rm = T)
  lodiff<-median(diff(long),na.rm = T)
  
  # Unfortunately sometimes the GetPopDemo function returns a character in col/rownames
  # Additionally, sometimes diff(long/lat) is not unique
  # Let's fix that!
  if(is.na(lat[1])) lat[1]<-lat[2]-ladiff
  if(is.na(lat[length(lat)])) lat[length(lat)]<-lat[length(lat)-1]+ladiff 
  if(is.na(long[1])) long[1]<-long[2]-lodiff 
  if(is.na(long[length(long)])) long[length(long)]<-long[length(long)-1]+lodiff 
  
  if(any(is.na(long)) | any(is.na(lat))) stop("nan values in longitude/latitude values of array col/row names")
  
  colnames(array)<-lat
  rownames(array)<-long
  
  array%<>%reshape2::melt()
  # if(array$Var1!=long | array$Var2!=lat) {
  #   
  # }
  
  return(array)
}

convRaster2SPDF<-function(raster,namez=NULL,crs="WGS84"){
  
  raster%<>%as("SpatialPixelsDataFrame")
  colnames(raster@coords) <- c("Longitude","Latitude")
  rownames(raster@bbox) <- c("Longitude","Latitude")
  if(!is.null(namez)) colnames(raster@data)<-namez
  
}

convMat2DF<-function(array,name=NULL){
  
  array%<>%checkMatlonglat()
  
  if(is.null(name)) name<-"Value"
  colnames(array)<-c("Longitude","Latitude",name)
  
  return(array)
  
}

convMat2raster<-function(array,name=NULL,crs="WGS84"){
  
  array%<>%checkMatlonglat()
  if(is.null(name)) name<-"Value"
  colnames(array)<-c("Longitude","Latitude",name)
  array %<>%raster
  
  if(crs=="WGS84") {crs(array)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  } else {stop("ERROR: Unknown coordinate system in convMat2SPDF, see Functions.R")}
  
  return(array)
  
}

convMat2SPDF<-function(array,name=NULL,crs="WGS84"){
  
  array%<>%checkMatlonglat()
  
  if(is.null(name)) name<-"Value"
  colnames(array)<-c("Longitude","Latitude",name)
  array <- SpatialPixelsDataFrame(points = array[c("Longitude","Latitude")],
                                  data = array[name])
  
  if(crs=="WGS84") {crs(array)<-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  } else {stop("ERROR: Unknown coordinate system in convMat2SPDF, see Functions.R")}
  
  ##### THIS SECTION IS TO ORDER THE VECTOR VALUES OF THE POPULATION MATRIX
  ##### OTHERWISE THE HAZARD INTERPOLATION IS SPLIT IN THE HORIZONTAL PLANE
  
  xo<-array@coords[1:array@grid@cells.dim[1],1]
  yo<-array@coords[1:array@grid@cells.dim[2]*array@grid@cells.dim[1]-array@grid@cells.dim[1]+1,2]
  
  if(!any(sort(yo)==yo)) {
    
    # find index to split data.frame
    ind<-which.min(array@coords[,2])-1L
    array@data[[name]]<-c(array@data[[name]][(ind+1):nrow(array)],array@data[[name]][1:ind])
    array@grid.index<-c(array@grid.index[(ind+1):nrow(array)],array@grid.index[1:ind])
    array@coords[,1]<-c(array@coords[(ind+1):nrow(array),1],array@coords[1:ind,1])
    array@coords[,2]<-c(array@coords[(ind+1):nrow(array),2],array@coords[1:ind,2])
    
  }
  
  return(array)
  
}

asSPDF<-function(obj,namez=NULL,crs="WGS84"){
  
  if(class(obj)=="raster") return(convRaster2SPDF(obj,namez=namez,crs=crs))
  if(class(obj)=="matrix") return(convMat2SPDF(obj,namez=namez,crs=crs))
  stop(paste0("Can't convert ",class(obj)," to SpatialPixelDataFrame"))
  
}

extractnumbers<-function(str){
  return(as.numeric(unlist(regmatches(str,gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",str, perl=TRUE)))))
}

expandBbox<-function(bbox,f,scaling=T){
  Dx<-bbox[3]-bbox[1]
  Dy<-bbox[4]-bbox[2]
  A<-Dx*Dy
  if(scaling) dx<-Dx*(-1+sqrt(f))
  else dx<-Dx*(-1+sqrt(1+(f-A)/A)) # use f as as the resulting area
  dy<-dx*Dy/Dx
  return(bbox+0.5*c(-dx,-dy,dx,dy))
}

convIso2Iso3<-function(iso2){
  countrycode::countrycode(sourcevar = iso2,
                           origin = "iso2c",
                           destination = "iso3c",warn = F)
}

convIso3Country<-function(iso3){
  countrycode::countrycode(sourcevar = iso3,
                           origin = "iso3c",
                           destination = "country.name",warn = F)
}

convIso3Continent<-function(iso3){
  continents<-countrycode::countrycode(sourcevar = iso3,
                                       origin = "iso3c",
                                       destination = "continent",warn = F)
}

coords2country = function(points,iso=T)
{  
  if(dim(points)[2]!=2) stop("coords2country Error: long/lat coords are invalid")
  if(dim(points)[1]==1) points=rbind(points,points)
  
  countriesSP <- rworldmap::getMap(resolution='high')
  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)
  
  # return the ISO3 of country
  if(iso) return(as.character(indices$ISO3))
  # return the ADMIN names of country
  return(as.character(indices$ADMIN))
}

countriesbbox<-function(iso3){
  
  countriesSP <- rworldmap::getMap(resolution='low')
  indies<-which(countriesSP$ISO3%in%iso3)
  
  mnlo<-mxlo<-mnla<-mxla<-c()
  for(c in 1:length(indies)){
    indy<-indies[c]
    for (i in 1:length(countriesSP@polygons[[indy]]@Polygons)){
      mnlo<-min(c(mnlo,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,1]))
      mxlo<-max(c(mxlo,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,1])) 
      mnla<-min(c(mnla,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,2]))
      mxla<-max(c(mxla,countriesSP@polygons[[indy]]@Polygons[[i]]@coords[,2]))
    }
  }
  
  return(c(mnlo,mnla,mxlo,mxla))
  
}

PlotDisaster<-function(pop,dfpoly,bbox=NULL,map=FALSE,ncity=1,namer="Disaster",filer="./"){
  
  if(is.null(bbox)) bbox<-as.numeric(c(min(rownames(pop)),min(colnames(pop)),max(rownames(pop)),max(colnames(pop))))
  longData<-reshape2::melt(pop)
  longData<-longData[longData$value!=0,]
  
  cities<-maps::world.cities%>%filter(lat>bbox[2]&lat<bbox[4]&long>bbox[1]&long<bbox[3])%>%arrange(desc(pop))
  if(ncity>1){wordcloud::wordcloud(words=cities$name,freq = cities$pop,max.words = 30,scale = c(2.5,0.2))}
  cities<-slice(cities,1:ncity)
  
  p<-ggplot(longData, aes(x = Var1, y = Var2)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="gray80", high="black") +
    labs(x="Longitude", y="Latitude", title=namer) +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  for (j in unique(dfpoly$ncontour)){
    tp<-filter(dfpoly,ncontour==j)
    p<-p+geom_polygon(data = tp,aes(x=Longitude,y=Latitude,group=Intensity,colour=Intensity),alpha=0,na.rm = T,size=2)+
      scale_color_gradient(low="mistyrose2", high="red")
  }
  p<-p+geom_label(data = cities, aes(long, lat, label = name), size = 4, fontface = "bold", nudge_x = 0.05*(bbox[3]-bbox[1]))
  
  print(p)
  if(!is.null(filer)) ggsave(paste0(namer,".eps"), plot=p,path = filer,width = 9,height = 7.)
  return(p)
}
