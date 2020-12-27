library(raster)
library(processx)

if(file.exists("allLogFiles.rds")) allLogFiles<- readRDS("allLogFiles.rds") else allLogFiles<-list()

paths2rinex<-"/archivio/cirgeoGNSSrinex_bot/Download"
allFiles<-list.files(paths2rinex, pattern=".*\\.([123][0-9][oO]|zip)$",  recursive = T, full.names = T)

 
filePath<-allFiles[[6]]

 



readRinex<-function(filePath){
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


 
p<-processx::process$new( paths2bin$rnx2rtkp, "--help", stdout = "|", stderr = "|")
p<-processx::process$new( paths2bin$rnx2rtkp, allFiles[[9]], stdout = "|", stderr = "|")

if(shiny::isTruthy(err)){
  print(err)
}
  