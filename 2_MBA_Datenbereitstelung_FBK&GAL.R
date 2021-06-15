# Datenschnittstelle Schweiz K33

#install.packages("dplyr")
#install.packages("readr")
#install.packages("stringr")
#install.packages("xlsx")
#install.packages("openxlsx", dependencies = TRUE)
#install.packages("data.table")

# Wahl Working Dir
library(dplyr)
library(readr)
library(stringr)
library("xlsx")
library("ggplot2")
library("openxlsx")
library("data.table")

setwd("C:/Users/aba/Desktop/Masterarbeit MBA/Datenzusammenstellen")



#set the working directory from which the files will be read from
setwd("C:/Users/nbe/Documents/daten_MBA/fbk")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/nbe/Documents/daten_MBA/fbk")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
datasetFBK <- data.frame()


#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], colClasses = "character",sep = "\n", header = FALSE, verbose = TRUE) #read in files using the fread function from the data.table package
  datasetFBK <- rbindlist(list(datasetFBK, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

#Einlsesen Geburtsabläufe

setwd("C:/Users/nbe/Documents/daten_MBA/gal")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/nbe/Documents/daten_MBA/gal")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
datasetGAL <- data.frame()


#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], colClasses = "character",strip.white=FALSE,fill=TRUE,sep = "\n", header = FALSE, verbose = TRUE) #read in files using the fread function from the data.table package
  datasetGAL <- rbindlist(list(datasetGAL, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

datasetGAL[1]

#Einlsesen PedigreeFile

setwd("C:/Users/nbe/Documents/daten_MBA/")

datasetPED <- data.frame()

datasetPED <- fread("20201216_pedigree_shb_allBreeds.dat", colClasses = "character",strip.white=FALSE, fill=TRUE,sep = "\n", header = FALSE, verbose = TRUE,)



dim(datasetFBK)
fbk<-datasetFBK

dim(datasetPED)
ped<-datasetPED

dataset.tempfbk <- data.frame(fbk)
dataset.tempped <- data.frame(ped)

as.numeric(3.4)

#fbkDatei - Spalten auswählen

dataset.fbk <- dataset.tempfbk %>%
  mutate(Tier.TVD = substring(V1,1,14),
         Lakt.Nr = substring(V1,23,24),
         #Kalbe.Dat = strptime(substring(V1,25,32),format="%Y%m%d"),
         #Bes.Dat = strptime(substring(V1,33,40),format="%Y%m%d"),
         Kalbe.Dat = substring(V1,25,32),
         Bes.Dat = substring(V1,33,40),
         Bes.Betrieb = substring(V1,65,71),
         Code.Bes = substring(V1,72,73),
         Natursprung.Enddatum= substring(V1,134,141),
  )

dim(dataset.fbk)
summary(dataset.fbk)

dataset.fbk$Kalbe.Dat <- as.Date(dataset.fbk$Kalbe.Dat,format="%Y%m%d")
dataset.fbk$Bes.Dat <- as.Date(dataset.fbk$Bes.Dat,format="%Y%m%d")
dataset.fbk$Natursprung.Enddatum <- as.Date(dataset.2$Natursprung.Enddatum,format="%Y%m%d")

#Erste Spalte löschen
dataset.3 <- dataset.fbk[,2:dim(dataset.fbk)[2]]
#Alle Rinder Besamungen löschen
dataset.4 <- dataset.3[dataset.3$Lakt.Nr!=" 0",]
#Besamungne älter 2015 löschen
dataset.5 <- dataset.4[dataset.4$Bes.Dat>"2015-01-01",]


#galDatei - Spalten auswählen

dataset.gal <- datasetGAL %>%
  mutate(Tier.ID = str_trim(substring(V1,1,10)),
         Bes.Datum = substring(V1,98,105),
         #Kalbe.Dat = strptime(substring(V1,25,32),format="%Y%m%d"),
         #Bes.Dat = strptime(substring(V1,33,40),format="%Y%m%d"),
  )
dataset.gal$Bes.Datum <- as.Date(dataset.gal$Bes.Datum,format="%Y%m%d")

#Erste Spalte löschen
dataset.tempgal2 <- dataset.gal[,2:dim(dataset.gal)[2]]
dim(dataset.tempgal2)



#pedDatei - Spalten auswählen

dataset.ped <- dataset.ped %>%
  mutate(Tier.ID = str_trim(substring(V1,1,10)),
         Tier.TVD = str_trim(substring(V1,57,71)),
  )

#Erste Spalte löschen
dataset.pedtemp <- dataset.ped[,2:dim(dataset.ped)[2]]

#Join GAL und PED
TOTAL<-merge(dataset.pedtemp, dataset.tempgal2, by = "Tier.ID", all=FALSE)
TOTAL <- mutate(TOTAL, Erfolg = "1")
TOTAL[1,]


setDT(dataset.fbk)
is.data.table(dataset.fbk)


#Create Testdataset for testing
BetriebSophie<-dataset.5[dataset.5$Bes.Betrieb == 6310030,] 
Betrieb1<-dataset.5[dataset.5$Bes.Betrieb == 6310015,] 
Betrieb2<-dataset.5[dataset.5$Bes.Betrieb == 1950009,] 
Betrieb3<-dataset.5[dataset.5$Bes.Betrieb == 6310012,] 
Betrieb4<-dataset.5[dataset.5$Bes.Betrieb == 2650034,] 
Betrieb5<-dataset.5[dataset.5$Bes.Betrieb == 3419016,]
Betrieb6<-dataset.5[dataset.5$Bes.Betrieb == 6632102,] 
Betrieb7<-dataset.5[dataset.5$Bes.Betrieb == 6998093,] 

Betriebe<- rbind(BetriebSophie,Betrieb1,Betrieb2,Betrieb3,Betrieb4,Betrieb5,Betrieb6,Betrieb7)
dim(Betriebe)
is.data.table(BetriebSophie)

Betriebe$Kalbe.Dat <- as.Date(Betriebe$Kalbe.Dat,format="%Y%m%d")
Betriebe$Bes.Dat <- as.Date(Betriebe$Bes.Dat,format="%Y%m%d")
Betriebe$Natursprung.Enddatum <- as.Date(Betriebe$Natursprung.Enddatum,format="%Y%m%d")

is.data.table(BetriebSophie)
dim(BetriebSophie)
BetriebSophie[1:10,]
summary(BetriebSophie)
summary(TOTAL)
TOTAL2<-merge(TOTAL, Betriebe, by.x = c("Tier.TVD","Bes.Datum"),by.y = c("Tier.TVD","Bes.Dat"), all.y=TRUE)
dim(TOTAL2)

#Daten Schreiben
setwd("C:/Users/nbe/Documents/daten_MBA/")
write.csv2(TOTAL2, file = "BesamungenBetriebe.csv", row.names = FALSE)
write.csv2(BetriebSophie[,1], file = "BesamungenSophieTierliste.csv", col.names = NA, row.names = FALSE)

ListeTierID<-data.table(Tiere=unique(BetriebSophie$Tier.TVD, incomparables = FALSE))
write.table(ListeTierID, file = "BesamungenSophieTierliste.csv", col.names = FALSE, row.names = FALSE)
