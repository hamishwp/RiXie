library(raster)
library(geosphere)
library(tidyverse)
library(magrittr)
library(ggplot2)
# library(summarytools)
library(leaflet)
library(sf)
library(ggmap)
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

saveSFasTif<-function(SFobj,filename=NULL){
  
  SFobj$ISO3C<-NULL
  class(SFobj)<-"SpatialPixelsDataFrame"
  SFobj<-brick(SFobj)
  
  if(!is.null(filename)) {
    x <- rast(SFobj)
    terra::writeRaster(x,filename=filename,overwrite=TRUE)
  }
  return(SFobj)
  
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

# And the opposite!
convCountryIso3<-function(country){
  countrycode::countrycode(sourcevar = country,
                           origin = "country.name",
                           destination = "iso3c",warn = F)
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
# Function to extract the coordinates of all the polygons
extractPolyCoords<-function(ADM){
  coords<-data.frame()
  for(i in 1:length(ADM@polygons)){
    for(j in 1:length(ADM@polygons[[i]]@Polygons)){
      coords%<>%rbind(data.frame(LONGITUDE=ADM@polygons[[1]]@Polygons[[1]]@coords[,1],
                                 LATITUDE=ADM@polygons[[1]]@Polygons[[1]]@coords[,2],
                                 i=i,j=j))
    }
  }
  return(coords)
}
# Find which coordinates of an S4 spatial object lie inside (or on the boundary of) a spatial polygon file
inPoly<-function(poly,pop,iii=1,sumFn="sum",reducer=NULL){
  
  Ifin<-rep(F,nrow(pop@data))
  if(is.null(reducer)) reducer<-!Ifin
  
  pop<-pop[reducer,]
  
  if(any(class(pop)=="SpatialPointsDataFrame") | any(class(pop)=="SpatialPixelsDataFrame")){
    coords<-pop@coords
    data<-pop@data
  } else {
    coords<-as.data.frame(pop[,c("LONGITUDE","LATITUDE")])
    data<-as.data.frame(pop)
  }
  
  insidepoly<-rep(FALSE,nrow(pop))
  
  for (i in 1:length(poly@Polygons)){
    # Get rid of values outside the bounding box first
    minipoly<-rep(FALSE,length(insidepoly))
    indies<-coords[,1]>=min(poly@Polygons[[i]]@coords[,1]) &
      coords[,1]<=max(poly@Polygons[[i]]@coords[,1]) &
      coords[,2]>=min(poly@Polygons[[i]]@coords[,2]) &
      coords[,2]<=max(poly@Polygons[[i]]@coords[,2])
    # Now we only need to calculate a few points that lie inside the polygon!
    minipoly[indies]<-sp::point.in.polygon(coords[indies,1],
                                           coords[indies,2],
                                           poly@Polygons[[i]]@coords[,1],
                                           poly@Polygons[[i]]@coords[,2])>0
    # Add to the total
    insidepoly<- insidepoly | minipoly
  }
  
  outer<-match.fun(sumFn)(data[insidepoly,iii],na.rm=T)
  
  Ifin[reducer]<-insidepoly
  
  return(list(vals=outer,indies=Ifin))
}
# Aggregate gridded data onto a polygon
Grid2ADM<-function(pop,ADM,sumFn="sum",index=1,ncores=4,outsiders=T)  {
  
  outer<-mclapply(1:length(ADM@polygons), 
                  function(j) {
                    funcy<-function(cccs,cind,extrF) vapply(1:length(cccs@polygons[[j]]@Polygons),
                                                            function(i) match.fun(extrF)(cccs@polygons[[j]]@Polygons[[i]]@coords[,cind],na.rm=T),
                                                            numeric(1))
                    # Find the bounding box for this polygon to minimise computation
                    bbox<-c(funcy(ADM,1,"min"),
                            funcy(ADM,2,"min"),
                            funcy(ADM,1,"max"),
                            funcy(ADM,2,"max"))
                    iii<-pop@coords[,1]>=bbox[1] &
                      pop@coords[,1]<=bbox[3] &
                      pop@coords[,2]>=bbox[2] &
                      pop@coords[,2]<=bbox[4]
                    inPoly(ADM@polygons[[j]],pop,index,sumFn=sumFn,reducer=iii)
                  }, mc.cores = ncores)
  vals<-c(); indies<-rep(F,nrow(pop))
  for(i in 1:length(outer)) {
    vals%<>%c(outer[[i]]$vals)
    indies<- indies | outer[[i]]$indies
  }
  if(!outsiders) return(vals)
  # Add the values that were not found inside a polygon
  if(any(class(pop)=="SpatialPointsDataFrame") | any(class(pop)=="SpatialPixelsDataFrame")){
    coords<-pop@coords
    data<-pop@data
  } else {
    coords<-as.data.frame(pop[,c("LONGITUDE","LATITUDE")])
    data<-as.data.frame(pop)
  }
  percy<-round(100*(1-match.fun(sumFn)(data[indies,index],na.rm=T)/match.fun(sumFn)(data[,index],na.rm=T)),digits = 2)
  if(percy>15) stop(paste0(percy,"% of values were not inside any polygon"))
  warning(paste0(percy,"% of values were not inside any polygon, adding them to nearest polygon now "))
  # Now to catch the annoying values that weren't located inside any of the polygons!
  # make a dataframe full of the coordinates of the polygons and indices of the corresponding polygon
  polycoords<-extractPolyCoords(ADM)
  # Make sure that the distance is not more than 4 grid cells away:
  mdistie<-sqrt(sum(pop@grid@cellsize^2))*4
  mind<-kk<-valmis<-0; vals_s<-vals
  for(i in which(!indies)) {
    # Find the closest polygon
    disties<-abs(coords[i,1]-polycoords$LONGITUDE)*abs(coords[i,2]-polycoords$LATITUDE)
    minnie<-which.min(disties)
    if(disties[minnie]>mdistie) {kk<-kk+1; valmis<-valmis+data[i,index]; next}
    mind%<>%max(disties[minnie])
    vals[polycoords$i[minnie]]<-vals[polycoords$i[minnie]]+data[i,index]
  }
  warning(paste0("Number & sum of gridpoints not inside admin boundaries = ",kk," totaling ",valmis))
  return(list(polyonly=vals_s,all=vals,mind=mind))
}

# Go from one set of admin boundaries to another, using the centroids of one of them
Poly2poly<-function(polyDF,ADM,SHDI,sumFn=NULL,index=1,ncores=4,outsiders=T){
  # Find the ADM boundaries that lie within GDL boundaries
  whichin<-sapply(1:length(polyDF@polygons),
              function(i) sapply(1:length(polyDF@polygons[[i]]@Polygons), 
                                 function(j) which(sp::point.in.polygon(ADM$LONGITUDE,
                                                                        ADM$LATITUDE,
                                                                        polyDF@polygons[[i]]@Polygons[[j]]@coords[,1],
                                                                        polyDF@polygons[[i]]@Polygons[[j]]@coords[,2])>0)))
  adder<-c()
  for(i in 1:length(whichin)) adder%<>%c(rep(i,length(unlist(whichin[[i]]))))
  indies<-data.frame(indexADM=unlist(whichin),indexGDL=adder, GDLCODE=SHDI$GDLCODE[adder])
  # Find any admin areas that were not found inside a GDL boundary
  missADM<-which(!1:nrow(ADM) %in% indies$indexADM)
  if(length(missADM)!=0) {
    # Now lets see whether any of their boundary coordinates fall inside a GDL boundary
    stillmiss<-c()
    for(mm in missADM){
      longitude<-unlist(sapply(1:length(ADM@polygons[[mm]]@Polygons),function(k) ADM@polygons[[mm]]@Polygons[[k]]@coords[,1],simplify = T))
      latitude<-unlist(sapply(1:length(ADM@polygons[[mm]]@Polygons),function(k) ADM@polygons[[mm]]@Polygons[[k]]@coords[,2],simplify = T))
      whichin<-sapply(1:length(polyDF@polygons),
                      function(i) sapply(1:length(polyDF@polygons[[i]]@Polygons), 
                                         function(j) which(sp::point.in.polygon(longitude,latitude,
                                                                                polyDF@polygons[[i]]@Polygons[[j]]@coords[,1],
                                                                                polyDF@polygons[[i]]@Polygons[[j]]@coords[,2])>0)))
      if(length(unlist(whichin))==0) {
        # indies%<>%rbind(data.frame(indexADM=mm,indexGDL=NA,GDLCODE=NA))
        stillmiss%<>%c(mm)
        next
      }
      missInd<-which.max(sapply(1:length(whichin),function(i) length(unlist(whichin[[i]]))))
      indies%<>%rbind(data.frame(indexADM=mm,indexGDL=missInd,GDLCODE=SHDI$GDLCODE[missInd]))
    }
    # FINAL RESORT... Use the first two admin level numbers closest to this value to determine GDL boundary
    for(mm in stillmiss){
      first<-indies$indexADM[which.min(abs(stillmiss-indies$indexADM))]
      second<-indies$indexADM[indies$indexADM!=first][which.min(abs(stillmiss-indies$indexADM[indies$indexADM!=first]))]
      if(indies$GDLCODE[first]==indies$GDLCODE[second]) {
        indies%<>%rbind(data.frame(indexADM=mm,indexGDL=indies$indexGDL[first],GDLCODE=indies$GDLCODE[first]))
        next
      } else {
        warning(paste0("GDL admin boundaries didn't pair up with UN ones for ",ADM$ADM2NM[mm]))
        indies%<>%rbind(data.frame(indexADM=mm,indexGDL=NA,GDLCODE=NA))
      }
    }
  }
  return(indies)
}

# 
# Points2Grid<-function(confy,pop,agg){
#  
#     vapply(1:nrow(pop@coords),function(ind) {
#       length(confy[which.min(c(abs(pop@coords[ind,1]-confy$LONGITUDE)*abs(pop@coords[ind,2]-confy$LATITUDE))),1])
#     },numeric(1))
#    
# }

# pop is S4 spatial object, so gridded, arrayz is Spatial Points or a dataframe
Points2Grid<-function(pop, arrayz, ncores=4, funcy="nrow",columner=1,namer="Pop", napop=F){
  
  funcy<-match.fun(funcy)
  
  grid<-as.data.frame(pop@grid)
  pop@data$array<-NA
  
  if(any(class(arrayz)=="SpatialPointsDataFrame")){
    arrayz<-data.frame(LONGITUDE=arrayz@coords[,1],
                       LATITUDE=arrayz@coords[,2],
                       POPULATION=arrayz@data[,columner]) 
  }
  
  # Create a function that takes in input i in 1:ncores that reduces FBpop (rename as tmp) as it goes
  # and returns a list of N/ncores values
  parAGG<-function(kk){
    
    if(napop) ijs<-which(!is.na(pop@data[,1])) else ijs<-1:nrow(pop@data)
    iiis<-floor((kk-1L)*length(ijs)/ncores+1L):floor((kk)*length(ijs)/ncores)
    if(kk==ncores) iiis<-floor((kk-1L)*length(ijs)/ncores+1L):length(ijs)
    ijs<-ijs[iiis]
    
    inds<-arrayz$LONGITUDE< (min(pop@coords[ijs,1]) - grid$cellsize[1])&
      arrayz$LONGITUDE>=(max(pop@coords[ijs,1]) + grid$cellsize[1])&
      arrayz$LATITUDE< (min(pop@coords[ijs,2]) - grid$cellsize[2])&
      arrayz$LATITUDE>=(max(pop@coords[ijs,2]) + grid$cellsize[2])
    
    tmp<-arrayz[!inds,]
    
    output<-rep(NA,length(ijs))
    i<-1
    for (z in ijs){
      
      inds<-tmp$LONGITUDE< (pop@coords[z,1] + grid$cellsize[1])&
        tmp$LONGITUDE>=(pop@coords[z,1] - grid$cellsize[1])&
        tmp$LATITUDE< (pop@coords[z,2] + grid$cellsize[2])&
        tmp$LATITUDE>=(pop@coords[z,2] - grid$cellsize[2])
      output[i]<-funcy(tmp[inds,columner])
      
      tmp<-tmp[!inds,]
      i<-i+1
      
    }
    
    return(output)
    
  }
  
  if(napop) indies<-which(!is.na(pop@data[,1])) else indies<-1:nrow(pop@data)
  
  if(ncores>1) {pop@data$array[indies]<-unlist(mclapply(1:ncores, FUN = parAGG, mc.cores = ncores))  
  } else{pop@data$array[indies]<-parAGG(1)}
  
  colnames(pop@data)[ncol(pop@data)]<-namer
  
  return(pop)
  
}

Genx0y0<-function(SFobj){
  
  xo<-SFobj@coords[1:SFobj@grid@cells.dim[1],1]
  yo<-SFobj@coords[1:SFobj@grid@cells.dim[2]*SFobj@grid@cells.dim[1]-SFobj@grid@cells.dim[1]+1,2]
  
  return(list(xo=xo,yo=yo))
}

Interp2GRID<-function(GridSF,IntData){
  
  # K-Nearest neighbour approach, applying K=1
  return(IntData%>%raster%>%raster::extract(GridSF@coords,method='bilinear'))
  
  # IntData%<>%as.data.frame()
  # coords<-Genx0y0(GridSF)
  # layer<-with(IntData,akima::interp(x=x,
  #                                   y=y,
  #                                   z=names(IntData)[1],
  #                                   xo=coords$xo,yo=coords$yo,
  #                                   linear=F,extrap = T))
  # return(c(layer$z))
  
}

InterpOn<-function(FromDF,ToDF,namer="tmp",ext=NULL){
  # Crop the data only to fit a certain area
  if(!is.null(ext)) FromDF%<>%crop(ext)
  # Interpolation depends on the comparative spatial resolution of the two datasets
  if(prod(FromDF@grid@cells.dim)>prod(ToDF@grid@cells.dim)){
    # Nearest neighbour
    ToDF@data$tmp<-FromDF%>%raster::extract(ToDF,method='bilinear',fun=mean,na.rm=T)%>%as.numeric()
  } else {
    # Bicubic spline interpolation
    FL%<>%as("SpatialPixelsDataFrame")
    ToDF@data$tmp<-interp::interp(x = FromDF@coords[,1],y=FromDF@coords[,2],z = FromDF@data[,1],
                xo=ToDF@coords[,1],yo=ToDF@coords[,2],linear = T,output="points")
  }
  names(ToDF@data)[ncol(ToDF@data)]<-namer
  return(ToDF)
}

