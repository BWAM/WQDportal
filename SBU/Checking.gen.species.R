#Checking if there are any gen/species not in the master species table
#2/2018
#Onion


#tidy up. This removes everything stored in short term memory
rm(list=ls())

# set the working directory
setwd("L:/DOW/StreamDatabase/SBU")
oursp<-read.csv("Species.csv")

######## Load the BAP library of functions ##########
library(BAP)

#add missing taxa to master list
#reading master species table
data("master")
#adding missing data
missed <-read.csv("Missing.Taxa.csv")
master<-rbind(master,missed)
rm(missed)

#Comparing the species in our species table to the master species list
keep<-c("MACRO_GENSPECIES","INDIV")
oursp<-oursp[keep]
rm(keep)
oursp$our<-"our"
oursp<-unique(oursp[c("MACRO_GENSPECIES","our")])

keep<-c("GENUS_SPECIES","TOLERANCE")
species<-master[keep]
rm(master)
rm(keep)
species$list <- "list"
species<-unique(species[c("GENUS_SPECIES","list")])
names(species)[names(species)=="GENUS_SPECIES"]<-"MACRO_GENSPECIES"

missing<-merge(oursp,species,all=TRUE)
missing<-missing[is.na(missing$list),]


#tidy up. This removes everything stored in short term memory
rm(list=ls())
