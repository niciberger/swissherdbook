library(dplyr)
library(readr)
library(stringr)
library("xlsx")
library("ggplot2")  
library("openxlsx")
library("data.table")


#SET Working Directory
setwd("C:/Users/nbe/Documents/daten_MBA/")
T1 <- fread("Total1.csv",sep=",")
T2 <- fread("Total2.csv",sep=",")
T3 <- fread("Total3.csv",sep=",")
T4 <- fread("Total4.csv",sep=",")
T5 <- fread("Total5.csv",sep=",")
T6 <- fread("Total6.csv",sep=",")
T7 <- fread("Total7.csv",sep=",")
T8 <- fread("Total8.csv",sep=",")
T9 <- fread("Total9.csv",sep=",")
T10 <- fread("Total10.csv",sep=",")

TOTAL <- rbind(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)
dim(TOTAL)

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

dim(TOTAL)
TOTALBalanciertNEIN_Delete<-TOTAL[-sample(which(Bes.Erfolg=="NEIN"), diffErfolg)]
dim(TOTALBalanciertNEIN_Delete)
diffBalanicertNEIN_Delete <- length(which(TOTALBalanciertNEIN_Delete$Erfolg=="JA"))-length(which(TOTALBalanciertNEIN_Delete$Erfolg=="NEIN"))
diffBalanicertNEIN_Delete

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert
dim(TOTAL)
length(which(TOTAL$Erfolg=="JA"))-length(which(TOTAL$Erfolg=="NEIN"))
dim(TOTALBalanciertNEIN_Delete)
length(which(TOTALBalanciertNEIN_Delete$Erfolg=="JA"))-length(which(TOTALBalanciertNEIN_Delete$Erfolg=="NEIN"))
dim(TOTALBalanciert)
length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))

write.csv(TOTAL, "TOTAL_BESNichtBalanciert.csv", na = "0", row.names = FALSE)
write.csv(TOTALBalanciertNEIN_Delete, "TOTAL_BalanicertNEIN_Delete.csv", na = "0", row.names = FALSE)
write.csv(TOTALBalanciert, "TOTAL_Balanciert_Dupliziert.csv", na = "0", row.names = FALSE)

