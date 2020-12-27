library(readr)
library(proj4)
library(sf)
library(mapview)

source("constants.R")

getStazioni<-function(force=F){
  stazioni<-NULL
  if(file.exists("stazioni.rds")) stazioni<-readRDS("stazioni.rds")
  if(!force && !is.null(stazioni)){
    return(stazioni)
  } 
  
  stazioni<- readr::read_delim("stazioni.csv", delim = " ", trim_ws = T, col_names = c("id","nid", "fid", "X", "Y", "Z", "l") )
  rownames(stazioni)<-stazioni$nid
  sc<-proj4::ptransform(stazioni[, c("X","Y","Z")], 
                        src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                        dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  latlong<-as.data.frame(sc)/pi*180
  df<-cbind( latlong[,1:2], stazioni[,2:3] )
  df.sf<-sf::st_as_sf(df, coords=c("x","y") )
  saveRDS(df.sf, "stazioni.rds")
  df.sf
  #mapview() + mapview(df.sf)
}



RINEX.process<-function(filePath){
  obs.nav<-manageRinexObsNav(filePath)
  rinexFile<-obs.nav$obs
  aster<-gsub("[on]$", "*", obs.nav[[1]])
  
  p<-processx::process$new( paths2bin$rnx2rtkp, 
                            c(obs.nav$obs, obs.nav$nav), 
                            stdout = "|", stderr = "|")
  out<-p$read_output_lines()
  err<-p$read_error_lines()
  
  rm(p)
  gc()
  
}

RINEX.manageObsNav<-function(filePath){
  receiver.obs<-NULL
  receiver.nav<-NULL
  ext<-raster::extension(filePath)
  if(tolower(ext)==".zip"){
    files<-unzip(filePath, exdir =  tempdir(), list = T )
    unzip(filePath, exdir =  tempdir() )
    obs<- grep(".[20-9]o", ignore.case = T, raster::extension(files$Name) ) 
    nav<- grep(".[20-9]n", ignore.case = T, raster::extension(files$Name) ) 
    if(length(obs) > 0 ) receiver.obs<-file.path(tempdir(), files$Name[[ obs[[1]] ]])
    if(length(nav) > 0 ) receiver.nav<-file.path(tempdir(), files$Name[[ nav[[1]] ]])
    
    
  } else {
    receiver.obs<-filePath
    receiver.nav<-gsub("o$", "n", filePath, ignore.case = T )
    if(!file.exists(receiver.nav)){
      print("NAV file not found!")
    }
  }
  
  if(is.null(receiver.nav) || !file.exists(receiver.nav)){
    print("NAV file not found")
  }
  
  if(is.null(receiver.obs) || !file.exists(receiver.obs)){
    return("OBS file not found")
  }
  
  list(obs=receiver.obs, nav=receiver.nav)
}

RINEX.getTimeXYZfromRINEX<-function(rinexFile){
  lines<-readLines(rinexFile, 10 )
  nm<-substr(lines, 61,80)
  vv<-substr(lines, 1,60)
  vv<-setNames(vv, nm)
  xyz<-na.omit(as.numeric(strsplit(vv["APPROX POSITION XYZ"], " ")[[1]]))
  sc<-proj4::ptransform(data =  t(cbind(xyz)), 
                        src.proj = "+proj=geocent +ellps=GRS80 +units=m +no_defs", 
                        dst.proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  latlong<-sc[1:2]/pi*180
  
  date.time<-(strsplit(vv["PGM / RUN BY / DATE"], " "))[[1]]
  ll<-length(date.time)
  timestamp<-as.POSIXct(paste(date.time[c((ll-2):ll)], collapse =" "),  
                        format="%Y%m%d %H%M%S", tz=date.time[[ll]] )
  list(xyz=latlong, timestamp=timestamp)
} 

timestamp2parts<-function(timestamp){
  list(
    year=format(timestamp, "%Y"),
    yearNc=format(timestamp, "%y"),
    month=format(timestamp, "%m"),
    day=format(timestamp, "%d"),
    hour=format(timestamp, "%H"),
    julianDay=format(timestamp, "%j")
  )
  
}

getFile.liguria<-function(timestamp, station){
  ts<-timestamp2parts(timestamp)
  lett<-letters[(as.integer(ts$hour)+1)] 
  url<-sprintf("http://gnss.regione.liguria.it/data/%s/rinex/1sec/%d/%d/%d/%s%d%s.%sd.Z",
               toupper(station), as.integer(ts$year), as.integer(ts$month),
               as.integer(ts$day), tolower(station), as.integer(ts$julianDay), lett,
               ts$yearNc )
  
  fn<-sprintf("%s%d%s.%sd.Z", tolower(station), as.integer(ts$julianDay), lett, ts$yearNc)
  
  download.error<-tryCatch( download.file(fn, fn), error=function(e){
    return(e)
  })
  
  if(download.error==0){
    return(fn)
  } else {
    return(download.error)
  }
}

getFile.veneto<-function(timestamp, station){
  ts<-timestamp2parts(timestamp)
  lett<-letters[(as.integer(ts$hour)+1)] 
  
  url<-sprintf(
    "http://retegnssveneto.cisas.unipd.it/Dati/Rinex/%s/1sec/%d/%d/%s%d%s.%sd.Z",
               toupper(station), as.integer(ts$year), as.integer(ts$julianDay), 
               tolower(station), as.integer(ts$julianDay), lett,
               ts$yearNc )
  
  fn<-sprintf("%s%d%s.%sd.Z", tolower(station), as.integer(ts$julianDay), lett, ts$yearNc)
  
  download.error<-tryCatch( download.file(fn, fn), error=function(e){
    return(e)
  })
  
  if(download.error==0){
    return(fn)
  } else {
    return(download.error)
  }
}

getFile<-list()
getFile[["LIGURIA"]]<-getFile.liguria
getFile[["VENETO"]]<-getFile.veneto
