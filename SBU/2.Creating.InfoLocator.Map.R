# Program:  SBU-reading tables
# Purpose: Creating Biomonitoring Layer for the Info Locator Map
# Programmer: Alene Onion 
# Date: 2/27/2018

#tidy up. This removes everything stored in short term memory
rm(list=ls())

setwd("L:/DOW/StreamDatabase/SBU")
metrics <- read.csv("metrics.csv")
sites <- read.csv("Sites.csv")

############################################################################################
#Remove samples collected outside the sampling period
############################################################################################

#Convert dates to date values
metrics$DATE <- as.Date(metrics$DATE,"%m/%d/%Y")
#Create a new column for sampling month
metrics$month <- format(metrics$DATE,"%m")

#pull records where month is between 7-9
mon = metrics$month == "07" | metrics$month == "08" | metrics$month == "09"
metrics <- metrics[mon,]
rm(mon)

#Restrict the samples to only kick (1) and mp (2 and 5)
sample = metrics$COLLECT=="1"|metrics$COLLECT=="2"|metrics$COLLECT=="5"
metrics<-metrics[sample,]
rm(sample)

#add stream name and lat/lon to the metrics table
sites$SITE_ID <- paste(sites$BASIN,"-",sites$LOCATION,"-",sites$RIVMILE,sep="")
sites<-unique(sites[c("SITE_ID","NAME","LATITUDE","LONGITUDE")])

metrics$SITE_ID<-paste(metrics$BASIN,"-",metrics$LOCATION,"-",metrics$RIVMILE,sep="")
metrics<-unique(metrics[c("SITE_ID","DATE","REPLICATE","FINAL_SCORE")])

data<-merge(sites,metrics,all=TRUE)
rm(list=c("metrics","sites"))

#Remove NA values
data <- data[!is.na(data$DATE),]

#Remove duplicates and reorder columns
data<-unique(data[c("NAME","SITE_ID","LATITUDE","LONGITUDE","DATE","REPLICATE","FINAL_SCORE")])

############################################################################################
#Average replicate samples
############################################################################################

#Remove replicate column so I can average it in the next step
keep <- c("NAME","SITE_ID","LATITUDE","LONGITUDE","DATE","FINAL_SCORE")
data1 <- data[keep]
rm(keep)

#aggregate
require("reshape2")
data1 <- melt(data1,id=1:5)
data1 <- dcast(data1,NAME+SITE_ID+LATITUDE+LONGITUDE+DATE~variable,mean)

data<-data1
rm(data1)


############################################################################################
#Creating simplified table for google map
############################################################################################

#merge the latlon
data$latlon <- do.call(paste,c(data[c("LATITUDE","LONGITUDE")],sep=","))

#reorganize table
data <- unique(data[c("NAME","SITE_ID","latlon","LATITUDE","LONGITUDE","DATE","FINAL_SCORE")])

#sort by station then date
data <- data[order(data$SITE_ID,data$latlon,data$DATE),]

#rename coll_date to DATE and collect to Method
names(data)[names(data)=="FINAL_SCORE"]<-"bap"

#Write table for use to make individual files
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web")
write.csv(data,file="Compiled.SBU.data.csv")

############################
#calculating assessment per station and combining bap records per station
############################
sites <- unique(data$latlon)
nsites <- length(sites)

#Starting with the first
  keep = data$latlon == sites[1]
  temp <- data[keep,]
  #for the first record
  bap = temp$bap[1]
  Most.Recent.Assessment.Date = temp$DATE[1]
  SiteID <- temp$SITE_ID[1]
  name <- temp$NAME[1]
  LATITUDE <- temp$LATITUDE[1]
  LONGITUDE <- temp$LONGITUDE[1]
  dataurl <- paste("www.hrecos.org/WAVE.Data/RIBS/",SiteID,".csv", sep="")
  SBU2 = data.frame(name,SiteID,LATITUDE,LONGITUDE,bap,Most.Recent.Assessment.Date,dataurl)
  rm(list=c('keep','temp','bap','Most.Recent.Assessment.Date','SiteID','name','LATITUDE','LONGITUDE','dataurl'))


#now for all subsequent sites
for(i in 2:nsites){
  keep = data$latlon == sites[i]
  temp <- data[keep,]
  #for the first record
  bap = temp$bap[1]
  Most.Recent.Assessment.Date = temp$DATE[1]
  SiteID <- temp$SITE_ID[1]
  name <- temp$NAME[1]
  LATITUDE <- temp$LATITUDE[1]
  LONGITUDE <- temp$LONGITUDE[1]
  dataurl <- paste("www.hrecos.org/WAVE.Data/RIBS/",SiteID,".csv", sep="")
  temp2 = data.frame(name,SiteID,LATITUDE,LONGITUDE,bap,Most.Recent.Assessment.Date,dataurl)
  SBU2 <- merge(SBU2,temp2,all=TRUE)
  rm(list=c('keep','temp','bap','Most.Recent.Assessment.Date','SiteID','name','LATITUDE','LONGITUDE','dataurl','temp2'))
}
rm(list=c('i','nsites','sites'))

data <- SBU2
rm(SBU2)

#Convert bap score to assessment
data$Most.Recent.Assessment <- NA
data$Most.Recent.Assessment <- ifelse(data$bap>=(7.5),"Non-Impacted",data$Most.Recent.Assessment)
data$Most.Recent.Assessment <- ifelse(data$bap<(7.5)&data$bap>=(5),"Slightly Impacted",data$Most.Recent.Assessment)
data$Most.Recent.Assessment <- ifelse(data$bap<(5)&data$bap>=(2.5),"Moderately Impacted",data$Most.Recent.Assessment)
data$Most.Recent.Assessment <- ifelse(data$bap<(2.5),"Severely Impacted",data$Most.Recent.Assessment)

#Remove the duplicates and reorder columns
data<-unique(data[c("name","SiteID","LATITUDE","LONGITUDE","Most.Recent.Assessment","Most.Recent.Assessment.Date","dataurl")])

#rename columns
names(data)[names(data)=="name"]<-"Stream"
names(data)[names(data)=="dataurl"]<-"Link.to.raw.data"

# write the table
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web")
write.table(data,file="map.info.locator.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())

############################################################################################################
#Now produce individual data files per station
############################################################################################################

# set the working directory 
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web")

#read previously collected data
SBU <- read.csv("Compiled.SBU.data.csv")

#Add an assessment column
SBU$Assessment <- NA
SBU$Assessment <- ifelse(SBU$bap>=(7.5),"Non-Impacted",SBU$Assessment)
SBU$Assessment <- ifelse(SBU$bap<(7.5)&SBU$bap>=(5),"Slightly Impacted",SBU$Assessment)
SBU$Assessment <- ifelse(SBU$bap<(5)&SBU$bap>=(2.5),"Moderately Impacted",SBU$Assessment)
SBU$Assessment <- ifelse(SBU$bap<(2.5),"Severely Impacted",SBU$Assessment)

#set the working directory to store the individual station data
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web/station.data")

#########################################################
#create individual files
#########################################################
stations = unique(SBU$SITE_ID)
nstations = length(stations)
i=0

for(i in 1:nstations){
  station = SBU$SITE_ID == stations[i]
  onestat <- SBU[station,]
  onestat <- onestat[!is.na(onestat$SITE_ID),]
  statID <- onestat$SITE_ID[1]
  this = paste0(statID,".csv")
  write.table(onestat,file=this,sep=",",row.names=FALSE)
  rm (onestat)
  rm(this)
  i=i+1
}
