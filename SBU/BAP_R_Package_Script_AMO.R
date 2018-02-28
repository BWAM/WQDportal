# NYSDEC Stream Biomonitoring Unit
# BAP Package Script
# 
# Provides the ability to run all BAP package metric functions in R directly by connecting to the package repository on BitBucket
# This package will run 1) Riffle/Kick Sample BAP 2) Net Jab/Sandy Streams Sample BAP 3) Multiplate Navigable River Sample BAP
# 4) Multiplate Non-Navigable River Sample BAP and 5) Ponar Sample BAP
# 
# This script created on 10/12/16 by Zach Smith and modified 2/27/18 by Alene Onion

#tidy up. This removes everything stored in short term memory so you start with a clean slate
rm(list=ls())

######## Import the Package ##########
# devtools must be installed
# This will connect R directly to the BAP package repository on BitBucket and allow the different functions of the BAP 
# package to run, using the latest updates
# YOU ONLY NEED TO DO THIS ONCE. If the script isn't working, it's worth loading this again in case your version is old.
#devtools::install_bitbucket(repo = "zsmith/NYSDEC.BAP")
#devtools::install_github(repo = "zsmith27/BAP")

######## Load the BAP library of functions ##########
library(BAP)

######## Calls on tables contained in the package #########
data("pma.model")
data("pma.ponar")
data("isd.df")
data("master")

#adding missing data to the master table (aka species list)
#***YOU NEED TO UPDATE THE FILE NAME, THIS SHOULD BE LOCATED IN YOUR WORKING DIRECTORY***
setwd("L:/DOW/StreamDatabase/SBU")
missed <-read.csv("Missing.Taxa.csv")
master<-rbind(master,missed)
rm(missed)

######## Import the Taxa Data ##########
#***YOU NEED TO UPDATE THE FILE NAME, THIS SHOULD BE LOCATED IN YOUR WORKING DIRECTORY***
setwd("L:/DOW/StreamDatabase/SBU")
taxa.df <- read.csv("Species.csv")
#remove NA river miles
taxa.df<-taxa.df[!is.na(taxa.df$RIVMILE),]

######## The following functions will run the BAPs for each of the 5 collection methods ############
#If the script isn't working (failing on the bap caluclation step), The first calculation includes lines that can help you figure out which sample it's failing on

#### Riffle BAP #### (COLLECT=1)
  #restrict to only riffle samples
    riffle<-taxa.df[taxa.df$COLLECT=="1",]
  #USE THIS SERIES OF LINES TO DETERMINE WHERE ERRORS LIE. SWITCH BETWEEN head and tail in the second line to bracket the error 
    #samples<-unique(riffle[c("BASIN","LOCATION","RIVMILE","COLL_DATE")])
    #samples<-head(samples,3211)
    #riffle<-merge(riffle,samples,by=c("BASIN","LOCATION","RIVMILE","COLL_DATE"),all=FALSE)
  #remove the sample: 11-ADKS11_2010-0.3-8/10/2011
    riffle$junk<-paste(riffle$BASIN,"-",riffle$LOCATION,"-",riffle$RIVMILE,"-",riffle$COLL_DATE,sep="")
    keep = riffle$junk == "11-ADKS11_2010-0.3-8/10/2011"
    junk <- riffle[keep,]
    rm(keep)
    riffle<-riffle[!(riffle$junk %in% junk$junk),]
    rm(junk)
  # This function prepares the data for the assessment
    long.df <- data_prep(riffle)
  #Calculating the BAP scores
    bap.riffle <- bap_riffle(long.df)
    rm(list=c("long.df","riffle"))

#### Jab BAP
  #restrict to only riffle samples
    jab<-taxa.df[taxa.df$COLLECT=="6",]
  # This function prepares the data for the assessment
    long.df <- data_prep(jab)
  #Calculating the BAP scores
    bap.jab <- bap_jab(long.df)
    rm(list=c("long.df","jab"))

#### MP Navigable Waters BAP (COLLECT=2)
  #restrict to only riffle samples
    mpnav<-taxa.df[taxa.df$COLLECT=="2",]
  #This function prepares the data for the assessment
    long.df <- data_prep(mpnav)
  #Calculating the BAP scores
    bap.mp.nav.waters <- bap_mp_nav_waters(long.df)
    rm(list=c("long.df","mpnav"))
    
#### MP Non-Navigable Waters BAP (COLLECT=5)
  #restrict to only riffle samples
    mpnonnav<-taxa.df[taxa.df$COLLECT=="5",]
  #This function prepares the data for the assessment
    long.df <- data_prep(mpnonnav)
  #Calculating the BAP scores
    bap.mp.non.nav.waters <- bap_mp_non_nav_water(long.df)
    rm(list=c("long.df","mpnonnav"))

#### Ponar BAP
  #restrict to only riffle samples
    ponar<-taxa.df[taxa.df$COLLECT=="3",]
  #This function prepares the data for the assessment
    long.df <- data_prep(ponar)
  #Calculating the BAP scores
    bap.ponar <- bap_ponar(long.df)
    rm(list=c("long.df","ponar"))

#### Remove the tables used to calculate these metrics so you're left only with the metric tables
rm(list=c("master","pma.model","pma.ponar","isd.df"))

######## The following functions will merge the metric calculations into one file and add back in Collect, Replicate, and Project columns ############
metrics <- merge(bap.riffle,bap.mp.nav.waters,all=TRUE)
metrics <- merge(metrics,bap.mp.non.nav.waters,all=TRUE)
metrics <- merge(metrics,bap.ponar,all=TRUE)
metrics <- merge(metrics,bap.jab,all=TRUE)

#Remove spaces from the event id
metrics$EVENT_ID<-gsub(" ","",metrics$EVENT_ID)
### remove duplicate records and reorder the columns
metrics<-unique(metrics[c("EVENT_ID","LOCATION","RIVMILE","BASIN","DATE","FINAL_SCORE","RICHNESS","RICH_SCORE","HBI","HBI_SCORE","SHANNON","SHANNON_SCORE","PMA","PMA_SCORE","EPT_RICH","EPT_SCORE","NBI_P","NBI_P_SCORE","NCO_RICH","NCO_RICH_SCORE","PCT_DOM3","PCT_DOM3_SCORE")])

#adds back in collect, replicate, and project columns
taxa.df$EVENT_ID<-paste(taxa.df$RIVMILE,"_",taxa.df$LOCATION,"_",taxa.df$BASIN,"_",taxa.df$COLL_DATE,"_",taxa.df$REPLICATE,sep="")
taxa.df<-unique(taxa.df[c("EVENT_ID","COLLECT","REPLICATE","PROJECT")])
metrics<-merge(metrics,taxa.df,by=c("EVENT_ID"),all=FALSE)

### remove duplicate records and reorder the columns
metrics<-unique(metrics[c("EVENT_ID","LOCATION","RIVMILE","BASIN","DATE","COLLECT","REPLICATE","PROJECT","FINAL_SCORE","RICHNESS","RICH_SCORE","HBI","HBI_SCORE","SHANNON","SHANNON_SCORE","PMA","PMA_SCORE","EPT_RICH","EPT_SCORE","NBI_P","NBI_P_SCORE","NCO_RICH","NCO_RICH_SCORE","PCT_DOM3","PCT_DOM3_SCORE")])

######### Use the function below to export your file #########
setwd("L:/DOW/StreamDatabase/SBU")
write.csv(metrics, "metrics.csv",row.names=FALSE)


#tidy up. This removes everything stored in short term memory
rm(list=ls())