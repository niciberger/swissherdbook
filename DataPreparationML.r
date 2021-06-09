ibrary(dplyr)
library(readr)
library(stringr)
library("xlsx")
library("ggplot2")  
library("openxlsx")
library("data.table")


#SET Working Directory
setwd("C:/Users/nbe/Documents/daten_MBA/")
MP <- fread("DatafileMBA_Ready14032021.csv",sep=",")
BES <- fread("FINAL1.csv",sep=",")
MP <- distinct(MP,MP$Tier.TVD,MP$Probe.Datum_ST, .keep_all = TRUE)
BES<- distinct(BES,BES$Tier.TVD,BES$Bes.Datum, .keep_all = TRUE)

dim(BES)
dim(MP)
MP<-data.table(MP)
BES<-data.table(BES)




dim(MP)
str(MP)
colnames(MP)
dim(BES)
str(BES)
colnames(BES)

ListeTierID<-data.table(Tiere=unique(BES$Tier.TVD, incomparables = FALSE))

nrow(ListeTierID)
#Add unique ID to each record
MP <- mutate(MP, idMP = rownames(MP))
BES <- mutate(BES, idBES = rownames(BES))
BES <- mutate(BES, idMPMin = NA)
BES_NEU<-BES
i<-1
p<-1
BES$Bes.Dat[1]
MP$Probe.Datum_ST[1]
difftime(BES$Bes.Dat[1],MP$Probe.Datum_ST[1])



start_time <- Sys.time()
BES_Temp<-BES[Tier.TVD == ListeTierID[23433,],]
MP_Temp<-MP[Tier.TVD == ListeTierID[32323,],]
end_time <- Sys.time()
LoopTime <- end_time-start_time
LoopTime

start_time <- Sys.time()
for(i in 1:nrow(ListeTierID)) {
#for(i in 1:10) {
  MP_Temp<-MP[Tier.TVD == ListeTierID[i,],] 
  BES_Temp<-BES[Tier.TVD == ListeTierID[i,],]
  print(ListeTierID[i,])
  print(nrow(BES_Temp))
  for(p in 1:nrow(BES_Temp)){
   #print(BES_Temp$Besamung[i])
    DayBESMP <- data.table(idBES=BES_Temp$idBES[p],idMP=MP_Temp$idMP, days=difftime(BES_Temp$Bes.Dat[p], MP_Temp$Probe.Datum_ST[], units = "days"))
    
    ID = DayBESMP[days == min(DayBESMP$days[DayBESMP$days>0]),]
    
    if(dim(ID[,1])[1]==0){
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    else if(as.numeric(ID[,3])>0 & as.numeric(ID[,3])<30) {
      print("if")
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-ID[,2]  
    } 
    else {
      print("else") 
      print(p)
      print(i)
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }

    print(paste("IDEBES=",BES_Temp$idBES[p],"------DIFF DAY=",ID[,3]))
    
  }
}





BES$idMPMin<-as.character(BES$idMPMin)
TOTAL<-merge(BES, MP, by.x = "idMPMin", by.y = "idMP", all=FALSE)
TOTAL$idMPMin<-as.character(TOTAL$idMPMin)
dim(TOTAL)
dim(MP)
dim(BES)
str(MP)
str(BES)
colnames(BES)
colnames(MP)
colnames(TOTAL)
sum(is.na(TOTAL))


TOTAL <- select(TOTAL,-c(idMPMin,"BES$Tier.TVD","BES$Bes.Datum",Tier.TVD.y,"MP$Tier.TVD","MP$Probe.Datum_ST"))

#TOTAL Ausbalanciere

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert

write.csv(TOTALBalanciert, "TOTALBalanciert1.csv", na = "0", row.names = FALSE)
write.csv(TOTAL, "TOTAL1.csv", na = "0", row.names = FALSE)
#write.csv2(TOTALBalanciert, file= "BESMP_MERGED_ALL1000.csv", row.names = FALSE)
#write.csv2(BetriebSophie, file = "BesamungenSophie.csv", row.names = FALSE)


#SET Working Directory
setwd("C:/Users/nbe/Documents/daten_MBA/")
MP <- fread("DatafileMBA_Ready14032021.csv",sep=",")
BES <- fread("FINAL2.csv",sep=",")
MP <- distinct(MP,MP$Tier.TVD,MP$Probe.Datum_ST, .keep_all = TRUE)
BES<- distinct(BES,BES$Tier.TVD,BES$Bes.Datum, .keep_all = TRUE)

dim(BES)
dim(MP)
MP<-data.table(MP)
BES<-data.table(BES)




dim(MP)
str(MP)
colnames(MP)
dim(BES)
str(BES)
colnames(BES)

ListeTierID<-data.table(Tiere=unique(BES$Tier.TVD, incomparables = FALSE))

nrow(ListeTierID)
#Add unique ID to each record
MP <- mutate(MP, idMP = rownames(MP))
BES <- mutate(BES, idBES = rownames(BES))
BES <- mutate(BES, idMPMin = NA)
BES_NEU<-BES
i<-1
p<-1
BES$Bes.Dat[1]
MP$Probe.Datum_ST[1]
difftime(BES$Bes.Dat[1],MP$Probe.Datum_ST[1])



start_time <- Sys.time()
BES_Temp<-BES[Tier.TVD == ListeTierID[23433,],]
MP_Temp<-MP[Tier.TVD == ListeTierID[32323,],]
end_time <- Sys.time()
LoopTime <- end_time-start_time
LoopTime

start_time <- Sys.time()
for(i in 1:nrow(ListeTierID)) {
  #for(i in 1:100) {
  MP_Temp<-MP[Tier.TVD == ListeTierID[i,],] 
  BES_Temp<-BES[Tier.TVD == ListeTierID[i,],]
  print(ListeTierID[i,])
  print(nrow(BES_Temp))
  for(p in 1:nrow(BES_Temp)){
    #print(BES_Temp$Besamung[i])
    DayBESMP <- data.table(idBES=BES_Temp$idBES[p],idMP=MP_Temp$idMP, days=difftime(BES_Temp$Bes.Dat[p], MP_Temp$Probe.Datum_ST[], units = "days"))
    
    ID = DayBESMP[days == min(DayBESMP$days[DayBESMP$days>0]),]
    
    if(dim(ID[,1])[1]==0){
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    else if(as.numeric(ID[,3])>0 & as.numeric(ID[,3])<30) {
      print("if")
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-ID[,2]  
    } 
    else {
      print("else") 
      print(p)
      print(i)
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    
    print(paste("IDEBES=",BES_Temp$idBES[p],"------DIFF DAY=",ID[,3]))
    
  }
}





BES$idMPMin<-as.character(BES$idMPMin)
TOTAL<-merge(BES, MP, by.x = "idMPMin", by.y = "idMP", all=FALSE)
TOTAL$idMPMin<-as.character(TOTAL$idMPMin)
dim(TOTAL)
dim(MP)
dim(BES)
str(MP)
str(BES)
colnames(BES)
colnames(MP)
colnames(TOTAL)
sum(is.na(TOTAL))


TOTAL <- select(TOTAL,-c(idMPMin,"BES$Tier.TVD","BES$Bes.Datum",Tier.TVD.y,"MP$Tier.TVD","MP$Probe.Datum_ST"))
TOTAL1 <- TOTAL
#TOTAL Ausbalanciere

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert

write.csv(TOTALBalanciert, "TOTALBalanciert2.csv", na = "0", row.names = FALSE)
write.csv(TOTAL, "TOTAL2.csv", na = "0", row.names = FALSE)
#write.csv2(TOTALBalanciert, file= "BESMP_MERGED_ALL1000.csv", row.names = FALSE)
#write.csv2(BetriebSophie, file = "BesamungenSophie.csv", row.names = FALSE)


#SET Working Directory
setwd("C:/Users/nbe/Documents/daten_MBA/")
MP <- fread("DatafileMBA_Ready14032021.csv",sep=",")
BES <- fread("FINAL3.csv",sep=",")
MP <- distinct(MP,MP$Tier.TVD,MP$Probe.Datum_ST, .keep_all = TRUE)
BES<- distinct(BES,BES$Tier.TVD,BES$Bes.Datum, .keep_all = TRUE)

dim(BES)
dim(MP)
MP<-data.table(MP)
BES<-data.table(BES)




dim(MP)
str(MP)
colnames(MP)
dim(BES)
str(BES)
colnames(BES)

ListeTierID<-data.table(Tiere=unique(BES$Tier.TVD, incomparables = FALSE))

nrow(ListeTierID)
#Add unique ID to each record
MP <- mutate(MP, idMP = rownames(MP))
BES <- mutate(BES, idBES = rownames(BES))
BES <- mutate(BES, idMPMin = NA)
BES_NEU<-BES
i<-1
p<-1
BES$Bes.Dat[1]
MP$Probe.Datum_ST[1]
difftime(BES$Bes.Dat[1],MP$Probe.Datum_ST[1])



start_time <- Sys.time()
BES_Temp<-BES[Tier.TVD == ListeTierID[23433,],]
MP_Temp<-MP[Tier.TVD == ListeTierID[32323,],]
end_time <- Sys.time()
LoopTime <- end_time-start_time
LoopTime

start_time <- Sys.time()
for(i in 1:nrow(ListeTierID)) {
  #for(i in 1:100) {
  MP_Temp<-MP[Tier.TVD == ListeTierID[i,],] 
  BES_Temp<-BES[Tier.TVD == ListeTierID[i,],]
  print(ListeTierID[i,])
  print(nrow(BES_Temp))
  for(p in 1:nrow(BES_Temp)){
    #print(BES_Temp$Besamung[i])
    DayBESMP <- data.table(idBES=BES_Temp$idBES[p],idMP=MP_Temp$idMP, days=difftime(BES_Temp$Bes.Dat[p], MP_Temp$Probe.Datum_ST[], units = "days"))
    
    ID = DayBESMP[days == min(DayBESMP$days[DayBESMP$days>0]),]
    
    if(dim(ID[,1])[1]==0){
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    else if(as.numeric(ID[,3])>0 & as.numeric(ID[,3])<30) {
      print("if")
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-ID[,2]  
    } 
    else {
      print("else") 
      print(p)
      print(i)
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    
    print(paste("IDEBES=",BES_Temp$idBES[p],"------DIFF DAY=",ID[,3]))
    
  }
}





BES$idMPMin<-as.character(BES$idMPMin)
TOTAL<-merge(BES, MP, by.x = "idMPMin", by.y = "idMP", all=FALSE)
TOTAL$idMPMin<-as.character(TOTAL$idMPMin)
dim(TOTAL)
dim(MP)
dim(BES)
str(MP)
str(BES)
colnames(BES)
colnames(MP)
colnames(TOTAL)
sum(is.na(TOTAL))


TOTAL <- select(TOTAL,-c(idMPMin,"BES$Tier.TVD","BES$Bes.Datum",Tier.TVD.y,"MP$Tier.TVD","MP$Probe.Datum_ST"))
TOTAL3 <- TOTAL
#TOTAL Ausbalanciere

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert

write.csv(TOTALBalanciert, "TOTALBalanciert3.csv", na = "0", row.names = FALSE)
write.csv(TOTAL, "TOTAL3.csv", na = "0", row.names = FALSE)
#write.csv2(TOTALBalanciert, file= "BESMP_MERGED_ALL1000.csv", row.names = FALSE)
#write.csv2(BetriebSophie, file = "BesamungenSophie.csv", row.names = FALSE)


#SET Working Directory
setwd("C:/Users/nbe/Documents/daten_MBA/")
MP <- fread("DatafileMBA_Ready14032021.csv",sep=",")
BES <- fread("FINAL4.csv",sep=",")
MP <- distinct(MP,MP$Tier.TVD,MP$Probe.Datum_ST, .keep_all = TRUE)
BES<- distinct(BES,BES$Tier.TVD,BES$Bes.Datum, .keep_all = TRUE)

dim(BES)
dim(MP)
MP<-data.table(MP)
BES<-data.table(BES)




dim(MP)
str(MP)
colnames(MP)
dim(BES)
str(BES)
colnames(BES)

ListeTierID<-data.table(Tiere=unique(BES$Tier.TVD, incomparables = FALSE))

nrow(ListeTierID)
#Add unique ID to each record
MP <- mutate(MP, idMP = rownames(MP))
BES <- mutate(BES, idBES = rownames(BES))
BES <- mutate(BES, idMPMin = NA)
BES_NEU<-BES
i<-1
p<-1
BES$Bes.Dat[1]
MP$Probe.Datum_ST[1]
difftime(BES$Bes.Dat[1],MP$Probe.Datum_ST[1])



start_time <- Sys.time()
BES_Temp<-BES[Tier.TVD == ListeTierID[23433,],]
MP_Temp<-MP[Tier.TVD == ListeTierID[32323,],]
end_time <- Sys.time()
LoopTime <- end_time-start_time
LoopTime

start_time <- Sys.time()
for(i in 1:nrow(ListeTierID)) {
  #for(i in 1:100) {
  MP_Temp<-MP[Tier.TVD == ListeTierID[i,],] 
  BES_Temp<-BES[Tier.TVD == ListeTierID[i,],]
  print(ListeTierID[i,])
  print(nrow(BES_Temp))
  for(p in 1:nrow(BES_Temp)){
    #print(BES_Temp$Besamung[i])
    DayBESMP <- data.table(idBES=BES_Temp$idBES[p],idMP=MP_Temp$idMP, days=difftime(BES_Temp$Bes.Dat[p], MP_Temp$Probe.Datum_ST[], units = "days"))
    
    ID = DayBESMP[days == min(DayBESMP$days[DayBESMP$days>0]),]
    
    if(dim(ID[,1])[1]==0){
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    else if(as.numeric(ID[,3])>0 & as.numeric(ID[,3])<30) {
      print("if")
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-ID[,2]  
    } 
    else {
      print("else") 
      print(p)
      print(i)
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    
    print(paste("IDEBES=",BES_Temp$idBES[p],"------DIFF DAY=",ID[,3]))
    
  }
}





BES$idMPMin<-as.character(BES$idMPMin)
TOTAL<-merge(BES, MP, by.x = "idMPMin", by.y = "idMP", all=FALSE)
TOTAL$idMPMin<-as.character(TOTAL$idMPMin)
dim(TOTAL)
dim(MP)
dim(BES)
str(MP)
str(BES)
colnames(BES)
colnames(MP)
colnames(TOTAL)
sum(is.na(TOTAL))


TOTAL <- select(TOTAL,-c(idMPMin,"BES$Tier.TVD","BES$Bes.Datum",Tier.TVD.y,"MP$Tier.TVD","MP$Probe.Datum_ST"))
TOTAL4 <- TOTAL
#TOTAL Ausbalanciere

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert

write.csv(TOTALBalanciert, "TOTALBalanciert4.csv", na = "0", row.names = FALSE)
write.csv(TOTAL, "TOTAL4.csv", na = "0", row.names = FALSE)
#write.csv2(TOTALBalanciert, file= "BESMP_MERGED_ALL1000.csv", row.names = FALSE)
#write.csv2(BetriebSophie, file = "BesamungenSophie.csv", row.names = FALSE)


#SET Working Directory
setwd("C:/Users/nbe/Documents/daten_MBA/")
MP <- fread("DatafileMBA_Ready14032021.csv",sep=",")
BES <- fread("FINAL5.csv",sep=",")
MP <- distinct(MP,MP$Tier.TVD,MP$Probe.Datum_ST, .keep_all = TRUE)
BES<- distinct(BES,BES$Tier.TVD,BES$Bes.Datum, .keep_all = TRUE)

dim(BES)
dim(MP)
MP<-data.table(MP)
BES<-data.table(BES)




dim(MP)
str(MP)
colnames(MP)
dim(BES)
str(BES)
colnames(BES)

ListeTierID<-data.table(Tiere=unique(BES$Tier.TVD, incomparables = FALSE))

nrow(ListeTierID)
#Add unique ID to each record
MP <- mutate(MP, idMP = rownames(MP))
BES <- mutate(BES, idBES = rownames(BES))
BES <- mutate(BES, idMPMin = NA)
BES_NEU<-BES
i<-1
p<-1
BES$Bes.Dat[1]
MP$Probe.Datum_ST[1]
difftime(BES$Bes.Dat[1],MP$Probe.Datum_ST[1])



start_time <- Sys.time()
BES_Temp<-BES[Tier.TVD == ListeTierID[23433,],]
MP_Temp<-MP[Tier.TVD == ListeTierID[32323,],]
end_time <- Sys.time()
LoopTime <- end_time-start_time
LoopTime

start_time <- Sys.time()
for(i in 1:nrow(ListeTierID)) {
  #for(i in 1:100) {
  MP_Temp<-MP[Tier.TVD == ListeTierID[i,],] 
  BES_Temp<-BES[Tier.TVD == ListeTierID[i,],]
  print(ListeTierID[i,])
  print(nrow(BES_Temp))
  for(p in 1:nrow(BES_Temp)){
    #print(BES_Temp$Besamung[i])
    DayBESMP <- data.table(idBES=BES_Temp$idBES[p],idMP=MP_Temp$idMP, days=difftime(BES_Temp$Bes.Dat[p], MP_Temp$Probe.Datum_ST[], units = "days"))
    
    ID = DayBESMP[days == min(DayBESMP$days[DayBESMP$days>0]),]
    
    if(dim(ID[,1])[1]==0){
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    else if(as.numeric(ID[,3])>0 & as.numeric(ID[,3])<30) {
      print("if")
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-ID[,2]  
    } 
    else {
      print("else") 
      print(p)
      print(i)
      BES$idMPMin[BES$idBES==BES_Temp$idBES[p]]<-"NA"
    }
    
    print(paste("IDEBES=",BES_Temp$idBES[p],"------DIFF DAY=",ID[,3]))
    
  }
}





BES$idMPMin<-as.character(BES$idMPMin)
TOTAL<-merge(BES, MP, by.x = "idMPMin", by.y = "idMP", all=FALSE)
TOTAL$idMPMin<-as.character(TOTAL$idMPMin)
dim(TOTAL)
dim(MP)
dim(BES)
str(MP)
str(BES)
colnames(BES)
colnames(MP)
colnames(TOTAL)
sum(is.na(TOTAL))


TOTAL <- select(TOTAL,-c(idMPMin,"BES$Tier.TVD","BES$Bes.Datum",Tier.TVD.y,"MP$Tier.TVD","MP$Probe.Datum_ST"))
TOTAL5 <- TOTAL
#TOTAL Ausbalanciere

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert

write.csv(TOTALBalanciert, "TOTALBalanciert5.csv", na = "0", row.names = FALSE)
write.csv(TOTAL, "TOTAL5.csv", na = "0", row.names = FALSE)
#write.csv2(TOTALBalanciert, file= "BESMP_MERGED_ALL1000.csv", row.names = FALSE)
#write.csv2(BetriebSophie, file = "BesamungenSophie.csv", row.names = FALSE)

TOTAL <- rbind(TOTAL,TOTAL2,TOTAL3,TOTAL4,TOTAL5)

nErfolg <- length(which(TOTAL$Bes.Erfolg=="JA"))
nNichtErfolg <- length(which(TOTAL$Bes.Erfolg=="NEIN"))
nErfolg
nNichtErfolg
diffErfolg=nNichtErfolg-nErfolg
set.seed(10)
TOTALSample <- sample_n(TOTAL[TOTAL$Bes.Erfolg=="JA",], diffErfolg,replace=TRUE)

TOTALBalanciert <- rbind(TOTALSample,TOTAL)
diffBalanicert <- length(which(TOTALBalanciert$Erfolg=="JA"))-length(which(TOTALBalanciert$Erfolg=="NEIN"))
diffBalanicert

write.csv(TOTALBalanciert, "TOTALBalanciert1to5.csv", na = "0", row.names = FALSE)
write.csv(TOTAL, "TOTAL1to5.csv", na = "0", row.names = FALSE)
