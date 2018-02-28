# Program:  SBU-reading tables
# Purpose: reading and checking SBU tables
# Programmer: Alene Onion 
# Date: 11/9/2017

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

#add stream name and lat/lon to the metrics table
sites$SITE_ID <- paste(sites$BASIN,"-",sites$LOCATION,"-",sites$RIVMILE,sep="")
sites<-unique(sites[c("SITE_ID","NAME","LATITUDE","LONGITUDE")])

metrics$SITE_ID<-paste(metrics$BASIN,"-",metrics$LOCATION,"-",metrics$RIVMILE,sep="")
data<-merge(sites,metrics,all=TRUE)
rm(list=c("metrics","sites"))

#Remove NA values
data <- data[!is.na(data$DATE),]

#Remove duplicates and reorder columns
data<-unique(data[c("NAME","SITE_ID","LATITUDE","LONGITUDE","DATE","REPLICATE","COLLECT","FINAL_SCORE","RICHNESS","RICH_SCORE","HBI","HBI_SCORE","SHANNON","SHANNON_SCORE","PMA","PMA_SCORE","EPT_RICH","EPT_SCORE","NBI_P","NBI_P_SCORE","NCO_RICH","NCO_RICH_SCORE","PCT_DOM3","PCT_DOM3_SCORE")])

#Write table for use to make individual files
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web")
write.csv(data,file="Compiled.SBU.data.csv",row.names=FALSE)

############################################################################################
#Average replicate samples
############################################################################################

#Restrict the samples to only kick (1) and mp (2 and 5)
sample = data$COLLECT=="1"|data$COLLECT=="2"|data$COLLECT=="5"|data$COLLECT=="3"
data<-data[sample,]
rm(sample)

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

#reorganize table
data <- unique(data[c("NAME","SITE_ID","LATITUDE","LONGITUDE","DATE","FINAL_SCORE")])

#sort by station then date
data <- data[order(data$SITE_ID,data$DATE,decreasing=TRUE),]

#rename coll_date to DATE 
names(data)[names(data)=="FINAL_SCORE"]<-"bap"

#Convert bap score to assessment
data$assessment <- NA
data$assessment <- ifelse(data$bap>=(7.5),"Non-Impacted",data$assessment)
data$assessment <- ifelse(data$bap<(7.5)&data$bap>=(5),"Slightly Impacted",data$assessment)
data$assessment <- ifelse(data$bap<(5)&data$bap>=(2.5),"Moderately Impacted",data$assessment)
data$assessment <- ifelse(data$bap<(2.5),"Severely Impacted",data$assessment)

############################
#calculating assessment per station and combining bap records per station
############################
sites <- unique(data$SITE_ID)
nsites <- length(sites)

#Starting with the first
keep = data$SITE_ID == sites[1]
temp <- data[keep,]
temp$assessments <- paste(temp$DATE,": ",temp$bap," ",temp$assessment,sep="")
Assessments <- unique(temp$assessments)
Assessments<- paste(Assessments,sep="\n",collapse="\n")
Most.Recent.Assessment = temp$assessment[1]
LATITUDE <- temp$LATITUDE[1]
LONGITUDE <- temp$LONGITUDE[1]
SiteID <- temp$SITE_ID[1]
name <- temp$NAME[1]
dataurl <- paste("www.hrecos.org/WAVE.Data/RIBS/",SiteID,".csv", sep="")
SBU2 = data.frame(name,SiteID,LATITUDE,LONGITUDE,Most.Recent.Assessment,Assessments,dataurl)
rm(list=c('keep','temp','Assessments','Most.Recent.Assessment','SiteID','name','LATITUDE','LONGITUDE','dataurl'))

#now for all subsequent sites
for(i in 2:nsites){
  keep = data$SITE_ID == sites[i]
  temp <- data[keep,]
  temp$assessments <- paste(temp$DATE,": ",temp$bap," ",temp$assessment,sep="")
  Assessments <- unique(temp$assessments)
  Assessments<- paste(Assessments,sep="\n",collapse="\n")
  Most.Recent.Assessment = temp$assessment[1]
  LATITUDE <- temp$LATITUDE[1]
  LONGITUDE <- temp$LONGITUDE[1]
  SiteID <- temp$SITE_ID[1]
  name <- temp$NAME[1]
  dataurl <- paste("www.hrecos.org/WAVE.Data/RIBS/",SiteID,".csv", sep="")
  temp2 = data.frame(name,SiteID,LATITUDE,LONGITUDE,Most.Recent.Assessment,Assessments,dataurl)
  SBU2 <- merge(SBU2,temp2,all=TRUE)
  rm(list=c('keep','temp','Assessments','Most.Recent.Assessment','SiteID','name','LATITUDE','LONGITUDE','dataurl','temp2'))
}
rm(list=c('i','nsites','sites'))

data <- SBU2
rm(SBU2)

#Remove the duplicates and reorder columns
data<-unique(data[c("name","SiteID","LATITUDE","LONGITUDE","Most.Recent.Assessment","Assessments","dataurl")])

#rename columns
names(data)[names(data)=="name"]<-"Stream"
names(data)[names(data)=="dataurl"]<-"Link.to.raw.data"

# write the table
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web")
write.table(data,file="map.csv",sep=",",row.names=FALSE)

#tidy up. This removes everything stored in short term memory
rm(list=ls())

############################################################################################################
#Now produce individual data files per station
############################################################################################################

# set the working directory 
setwd("C:/Rscripts/Stream.Data/SBU.Data/Data.for.Web")

#read previously collected data
SBU <- read.csv("Compiled.SBU.data.csv")

#sort by station then date
SBU <- SBU[order(SBU$DATE,decreasing=TRUE),]


#Add an assessment column
SBU$Assessment <- NA
SBU$Assessment <- ifelse(SBU$FINAL_SCORE>=(7.5),"Non-Impacted",SBU$Assessment)
SBU$Assessment <- ifelse(SBU$FINAL_SCORE<(7.5)&SBU$FINAL_SCORE>=(5),"Slightly Impacted",SBU$Assessment)
SBU$Assessment <- ifelse(SBU$FINAL_SCORE<(5)&SBU$FINAL_SCORE>=(2.5),"Moderately Impacted",SBU$Assessment)
SBU$Assessment <- ifelse(SBU$FINAL_SCORE<(2.5),"Severely Impacted",SBU$Assessment)

#Remove duplicates and reorder columns
SBU<-unique(SBU[c("NAME","SITE_ID","LATITUDE","LONGITUDE","DATE","REPLICATE","COLLECT","FINAL_SCORE","Assessment","RICHNESS","RICH_SCORE","HBI","HBI_SCORE","SHANNON","SHANNON_SCORE","PMA","PMA_SCORE","EPT_RICH","EPT_SCORE","NBI_P","NBI_P_SCORE","NCO_RICH","NCO_RICH_SCORE","PCT_DOM3","PCT_DOM3_SCORE")])

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
