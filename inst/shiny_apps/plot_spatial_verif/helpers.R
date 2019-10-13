DaysInMonth <- function(yyyy,mm){
  t1 <-  format(as.Date(paste(yyyy,"-",mm,"-","01",sep=""),format="%Y-%m-%d")+31,"%Y-%m-01")
  t2 <- as.Date(t1,format="%Y-%m-%d")-1
  as.numeric(format(t2,"%d"))
## as a 1-liner:
# as.numeric(format(as.Date(format(as.Date(paste(yyyy,"-",mm,"-","01",sep=""),format="%Y-%m-%d")+31,"%Y-%m-01"),format="%Y-%m-%d")-1,"%d"))
}

makedate <- function(x) as.POSIXct(x,origin="1970-01-01",tz="UTC")
dateMIN <- makedate("2013-01-01")
dateMAX <- makedate(as.character(Sys.Date()-1))
yearMIN <- as.numeric(substr(dateMIN,1,4))
yearMAX <- as.numeric(substr(dateMAX,1,4))

VF_fcdate_format <- function(x) as.numeric(format(x,"%Y%m%d"))
VF_fctime_format <- function(x) as.numeric(format(x,"%H"))

