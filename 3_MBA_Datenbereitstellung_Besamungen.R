# Wahl Working Dir
install.packages("tidyr")
library(dplyr)
library(readr)
library(stringr)
library("xlsx")
library("ggplot2")
library("openxlsx")
library("data.table")
library(tidyr)

setwd("C:/Users/nbe/Documents/daten_MBA/")

#Einlesen Besamungen
K.10 <- fread("DataExp_K10_060251_M03_ABA.K10", colClasses = "character",strip.white=FALSE, fill=TRUE,sep = "\n", header = FALSE, verbose = TRUE,)
class(K.10)
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


#Einlsesen PedigreeFile

setwd("C:/Users/nbe/Documents/daten_MBA/")
datasetPED <- data.frame()
datasetPED <- fread("20201216_pedigree_shb_allBreeds.dat", colClasses = "character",strip.white=FALSE, fill=TRUE,sep = "\n", header = FALSE, verbose = TRUE,)



#Besamungs File - Spalten auswählen

K.10.1 <- K.10 %>%
  mutate(Tier.TVD = substring(V1, 23, 36),
         Bes.Betrieb = substring(V1,9,15),
         Bes.Lakt.Nr = substring(V1,69,70),
         Bes.Kalbe.Dat = substring(V1,72,79),
         Bes.Datum = substring(V1,80,87),
         Bes.Code. = str_squish(substring(V1,88,88)),
         Bes.Nr = str_squish(substring(V1,89,90)),
         Bes.Natursprung.Enddatum = substring(V1,198,205),
  ) 

K.10.1 <- K.10.1 %>% mutate_all(na_if," ") 
K.10.1 <- K.10.1 %>% mutate_all(na_if,"")


K.10.1$Bes.Kalbe.Dat <- as.Date(K.10.1$Bes.Kalbe.Dat,format="%Y%m%d")
K.10.1$Bes.Datum <- as.Date(K.10.1$Bes.Datum,format="%Y%m%d")
K.10.1$Bes.Natursprung.Enddatum <- as.Date(K.10.1$Bes.Natursprung.Enddatum,format="%Y%m%d")
K.10.1 <- mutate(K.10.1, Bes.Nautrsprung = ifelse(is.na(Bes.Natursprung.Enddatum), "NEIN", "JA"))
K.10.1 <- K.10.1[K.10.1$Bes.Datum>"2015-01-01",]
K.10.1 <- K.10.1[K.10.1$Bes.Datum<"2020-01-01",]
K.10.1 <- select (K.10.1,-c(V1,Bes.Natursprung.Enddatum))
K.10.1 <- na.omit(K.10.1)
sum(is.na(K.10.1))
summary(K.10.1)
dim(K.10.1)
K.10.1[1,]

#galDatei - Spalten auswählen

dataset.gal <- datasetGAL %>%
  mutate(Tier.ID = str_trim(substring(V1,1,10)),
         Bes.Datum = substring(V1,98,105),
         #Kalbe.Dat = strptime(substring(V1,25,32),format="%Y%m%d"),
         #Bes.Dat = strptime(substring(V1,33,40),format="%Y%m%d"),
  ) %>% mutate_all(na_if," ") %>% mutate_all(na_if," ")

dataset.gal$Bes.Datum <- as.Date(dataset.gal$Bes.Datum,format="%Y%m%d")
dataset.gal <- select(dataset.gal,-V1)
dataset.gal <- na.omit(dataset.gal)

sum(is.na(dataset.gal))
summary(dataset.gal)
dim(dataset.gal)
dataset.gal[1,]


#pedDatei - Spalten auswählen

datasetPED <- datasetPED %>%
  mutate(Tier.ID = str_trim(substring(V1,1,10)),
         Tier.TVD = str_trim(substring(V1,57,71)),
  ) %>% mutate_all(na_if," ") %>% mutate_all(na_if," ")

#Erste Spalte löschen
datasetPED[1,]
dataset.ped <- datasetPED[,2:dim(datasetPED)[2]]
dim(dataset.ped)
dataset.ped <- na.omit(dataset.ped)


sum(is.na(dataset.ped))
summary(dataset.ped)
dim(dataset.ped)
dataset.ped[1,]

#Merge GAL und PED
gal_ped<-merge(dataset.ped, dataset.gal, by = "Tier.ID", all=FALSE)
gal_ped<- mutate(gal_ped, Bes.Erfolg = "JA")
gal_ped<-gal_ped[,2:4]
#Merge gal_ped und K10.2
FINAL<-merge(K.10.1, gal_ped, by = c("Tier.TVD","Bes.Datum"), all.x=TRUE)
dim(K.10.1)
sum(is.na(FINAL))
dim(FINAL)
dim(gal_ped)

FINAL[is.na(FINAL)] <- "NEIN"

FINAL100000Zeilen <- FINAL[1:100000,]
1144814/100000


write.csv(FINAL, "FINAL_BES.csv", na = "0", row.names = FALSE)
write.csv(FINAL100000Zeilen, "FINAL_BES100000ZeilenTestFile.csv", na = "0", row.names = FALSE)


