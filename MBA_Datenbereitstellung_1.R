# Wahl Working Dir
library(dplyr)
library(readr)
library(stringr)
library("xlsx")
library("ggplot2")
library("openxlsx")
library("data.table")

setwd("C:/Users/aba/Desktop/Masterarbeit MBA/Datenzusammenstellen")


#K33Datei wählen
myFile <- choose.files(default = "",
                       caption = "Wähle die Original-K33-Datei",
                       multi = FALSE)

K.33 <- read_delim(myFile, delim = ";", col_names = FALSE,
                   locale = locale(encoding = "ISO-8859-1"))

#K33Datei - Spalten auswählen
K.33.1 <- K.33 %>%
  mutate(Betrieb_aktuell.SHB = substring(X1,9,15),
         Betrieb.TVD = substring(X1,16,22),
         Tier.TVD = substring(X1, 23, 36),
		 Tier.TVD_ohneCH = substring(X1,25,36),
		 Tier.RasseCode = substring(X1, 37,39),
         Tier.Kalbedatum = substring(X1,69,76),
		 Tier.Kalbemonat = substring(X1,73,74),
         Tier.Laktationsnummer = substring(X1,77,78),
		 Tier.Probennummer = substring(X1,79,81),
         Datum.Probewaegung = substring(X1,82,89),
		 Monat.Probewaegung = substring(X1,86,87),
         Milchkg = substring(X1,90,93),
         Fett = substring(X1,94,97),
         Eiweiss = substring(X1,98,101),
         Probepersistenz = substring(X1,106,108),
         Zellzahl = substring(X1,109,112),
         Milchharnstoff = substring(X1,113,115),
         AlpHoehe = substring(X1,118,119),
         Pruefmethode = substring(X1,125,126),
         CodeWaegung = substring(X1,146,147))



#K33Datei - Spalten auswählen für Excel
K.33.2 <- K.33.1 %>%
  select(Betrieb_aktuell.SHB,
         Betrieb.TVD,
         Tier.TVD,
		 Tier.TVD_ohneCH,
		 Tier.RasseCode,
         Tier.Kalbedatum,
		 Tier.Kalbemonat,
         Tier.Laktationsnummer,
         Tier.Probennummer,
         Datum.Probewaegung,
		 Monat.Probewaegung,
         Milchkg,
         Fett,
         Eiweiss,
         Probepersistenz,
         Zellzahl,
         Milchharnstoff,
         AlpHoehe,
         Pruefmethode,
         CodeWaegung)

dim(K.33.2)

Tier.TVD_ohneCH=K.33.2$Tier.TVD_ohneCH
Tier.TVD_ohneCH <- as.numeric(Tier.TVD_ohneCH)
Tier.Laktationsnummer=K.33.2$Tier.Laktationsnummer
Tier.Laktationsnummer <- as.numeric(Tier.Laktationsnummer)
Tier.TVD.Laktationsnummer=Tier.TVD_ohneCH+(Tier.Laktationsnummer)/100
Tier.TVD.Laktationsnummer <- as.numeric(Tier.TVD.Laktationsnummer)
#Tier.TVD.Laktationsnummer=data.frame(Tier.TVD.Laktationsnummer)
dim(Tier.TVD.Laktationsnummer)
Tier.TVD.Laktationsnummer[1,]
Tier.TVD.Laktationsnummer=data.table(Tier.TVD.Laktationsnummer)


K.33.2 <- cbind(Tier.TVD.Laktationsnummer,K.33.2)
#K.33.2 <- cbind(K.33.2, Tier.TVD.Laktationsnummer)
dim(K.33.2)

#######K.33.2 <- K.33.2[1:5505286,]
#######dim(K.33.2)

#K.33.2 <- as.data.table(K.33.2)

#K33Datei nur erste Milchkontrolle
K.33.1K <- K.33.1 %>%
  select(Tier.TVD_ohneCH,
         Tier.Probennummer,
         Datum.Probewaegung,
		 Monat.Probewaegung,
         Milchkg,
         Fett,
         Eiweiss,
         Zellzahl,
         Milchharnstoff,
         AlpHoehe,
         Pruefmethode,
         CodeWaegung)

#K.33.1K = data.table(K.33.1K)
K.33.1K <- cbind(Tier.TVD.Laktationsnummer,K.33.1K)
#K.33.1K <- cbind(K.33.1K, Tier.TVD.Laktationsnummer)
dim(K.33.1K)		 
	
########K.33.1K <- K.33.1K[1:5505286,]
########dim(K.33.1K)

#K.33.1K <- as.data.table(K.33.1K)	
	
	
K.33.1K <- K.33.1K[!grepl("2",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("3",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("4",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("5",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("6",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("7",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("8",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("9",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("10",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("11",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("12",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("13",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("14",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("15",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("16",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("17",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("18",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("19",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("20",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("21",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("22",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("23",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("24",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("25",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("26",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("27",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("28",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("29",K.33.1K$Tier.Probennummer),]
K.33.1K <- K.33.1K[!grepl("30",K.33.1K$Tier.Probennummer),]
#K.33.1K <- as.data.table(K.33.1K)
K.33.1K[1,]
dim(K.33.1K)


K.33.Ready = merge(K.33.2,K.33.1K,by="Tier.TVD.Laktationsnummer", allow.cartesian=TRUE)
dim(K.33.Ready)

K.33.Ready <- as.data.table(K.33.Ready)

#Y01Datei wählen
myFile <- choose.files(default = "",
                       caption = "Wähle die Original-Y01-Datei",
                       multi = FALSE)

Y.01 <- read_delim(myFile, delim = ";", col_names = FALSE,
                   locale = locale(encoding = "ISO-8859-1"))

#Y01Datei - Spalten auswählen
Y.01.1 <- Y.01 %>%
  mutate(Tier.TVD = substring(X1, 23, 36),
         Tier.Geburtsdatum.Y01 = substring(X1,52,59),
         Tier.Abgangsdatum.Y01 = substring(X1,138,145))
		 
#Y01Datei - Spalten auswählen für Excel
Y.01.2 <- Y.01.1 %>%
  select(Tier.TVD,
         Tier.Geburtsdatum.Y01,
         Tier.Abgangsdatum.Y01)




#K09Datei wählen
myFile <- choose.files(default = "",
                       caption = "Wähle die Original-K09-Datei",
                       multi = FALSE)

K.09 <- read_delim(myFile, delim = ";", col_names = FALSE,
                   locale = locale(encoding = "ISO-8859-1"))

#K09Datei - Spalten auswählen
K.09.1 <- K.09 %>%
  mutate(Tier.TVD = substring(X1, 23, 36),
         Tier.ZW.Mi_K09 = substring(X1,63,67),
		 Tier.ZW.Fe_K09 = substring(X1,72,76),
		 Tier.ZW.Ei_K09 = substring(X1,81,85),
		 Tier.ZWBasis_K09 = substring(X1,86,91),
		 Tier.ZW.ZZ_K09 = substring(X1,94,98),
		 Tier.ZW.Per_K09 = substring(X1,108,110),
		 Tier.ZWLabel_K09 = substring(X1,115,117),
		 Tier.ZW.MaR_K09 = substring(X1,134,136))

#K09Datei - Spalten auswählen für Excel
K.09.2 <- K.09.1 %>%
  select(Tier.TVD,
         Tier.ZW.Mi_K09,
		 Tier.ZW.Fe_K09,
		 Tier.ZW.Ei_K09,
		 Tier.ZWBasis_K09,
		 Tier.ZW.ZZ_K09,
		 Tier.ZW.Per_K09,
		 Tier.ZWLabel_K09,
		 Tier.ZW.MaR_K09)




#K04Datei wählen
myFile <- choose.files(default = "",
                       caption = "Wähle die Original-K04-Datei",
                       multi = FALSE)

K.04 <- read_delim(myFile, delim = ";", col_names = FALSE,
                   locale = locale(encoding = "ISO-8859-1"))

#K04Datei - Spalten auswählen
K.04.1 <- K.04 %>%
  mutate(Tier.TVD_ohneCH = substring(X1, 25, 36),
		 Tier.Laktationsnummer = substring(X1, 69, 70),
		 Tier.Region.K04 = substring(X1,142,143))

#K04Datei - Spalten auswählen für Excel
K.04.2 <- K.04.1 %>%
  select(Tier.TVD_ohneCH,
		  Tier.Laktationsnummer,
          Tier.Region.K04)

K.04.2 <- K.04.2[!grepl("0",K.04.2$Tier.Laktationsnummer),]		
dim(K.04.2)
		
Tier.TVD_ohneCH=K.04.2$Tier.TVD_ohneCH
Tier.TVD_ohneCH <- as.numeric(Tier.TVD_ohneCH)
Tier.Laktationsnummer=K.04.2$Tier.Laktationsnummer
Tier.Laktationsnummer <- as.numeric(Tier.Laktationsnummer)
Tier.TVD.Laktationsnummer=Tier.TVD_ohneCH+(Tier.Laktationsnummer)/100
Tier.Region.K04=K.04.2$Tier.Region.K04
Tier.Region=data.frame(Tier.TVD.Laktationsnummer,Tier.Region.K04)
dim(Tier.Region)
Tier.Region[1,]

Tier.Reg.2 <- Tier.Region %>%
group_by(Tier.TVD.Laktationsnummer, Tier.Region.K04) %>%
#arrange(Tier.TVD.Laktationsnummer) %>% # in each group, arrange in ascending order by distance
filter(row_number() == 1)





#K45Datei wählen
myFile <- choose.files(default = "",
                       caption = "Wähle die Original-K45-Datei",
                       multi = FALSE)

K.45 <- read_delim(myFile, delim = ";", col_names = FALSE,
                   locale = locale(encoding = "ISO-8859-1"))

#K45Datei - Spalten auswählen
K.45.1 <- K.45 %>%
  mutate(Tier.TVD = substring(X1, 23, 36),
         Tier.Lebensleistung.K45 = substring(X1,77,82))

#K45Datei - Spalten auswählen für Excel
K.45.2 <- K.45.1 %>%
  select(Tier.TVD,
         Tier.Lebensleistung.K45)





#BetriebeMLPSHB - Datei wählen
BetriebeMLPSHB <-read.csv(file="BetriebeMLPSHB_20201231_ABA.csv",header=TRUE,sep=";",na.string="")
BetriebeMLPSHB[1,]

#BetriebeMLPSHB - Spalten auswählen
Betrieb_aktuell.BetriebeMLPSHB = BetriebeMLPSHB$VzgBtrNr
Betrieb_aktuell.BetriebeMLPSHB <- as.character(Betrieb_aktuell.BetriebeMLPSHB)
Betrieb_Lebensleistung.BetriebeMLPSHB = BetriebeMLPSHB$LLMkg
Betrieb_Lebenstagesleistung.BetriebeMLPSHB = BetriebeMLPSHB$MilchTag
Betrieb_ZZ.BetriebeMLPSHB = BetriebeMLPSHB$ZZ
Betrieb_RZ.BetriebeMLPSHB = BetriebeMLPSHB$RZ
Betrieb_SP.BetriebeMLPSHB = BetriebeMLPSHB$SP
Betrieb_Lebensdauer.BetriebeMLPSHB = BetriebeMLPSHB$Lebtage
Betrieb_Nutzungsdauer.BetriebeMLPSHB = BetriebeMLPSHB$ND

#BetriebeMLPSHB - Spalten auswählen für Excel
Betriebsdurchschnitt.BetriebeMLPSHB=data.table(Betrieb_aktuell.BetriebeMLPSHB,
Betrieb_Lebensleistung.BetriebeMLPSHB,
Betrieb_Lebenstagesleistung.BetriebeMLPSHB,
Betrieb_ZZ.BetriebeMLPSHB,
Betrieb_RZ.BetriebeMLPSHB,
Betrieb_SP.BetriebeMLPSHB,
Betrieb_Lebensdauer.BetriebeMLPSHB,
Betrieb_Nutzungsdauer.BetriebeMLPSHB)
dim(Betriebsdurchschnitt.BetriebeMLPSHB)
Betriebsdurchschnitt.BetriebeMLPSHB[1,]


#In Data Table Umwandeln
K.33.Ready <- as.data.table(K.33.Ready)
Y.01.2 <- as.data.table(Y.01.2)
K.09.2 <- as.data.table(K.09.2)
Tier.Reg.2 <- as.data.table(Tier.Reg.2)
K.45.2 <- as.data.table(K.45.2)
Betriebsdurchschnitt.BetriebeMLPSHB <- as.data.table(Betriebsdurchschnitt.BetriebeMLPSHB)



# K33.Y01.K09 = merge(K33.Y01, K.09.2, by="Tier.TVD", all.x=TRUE)
#Fehler in vecseq(f__, len__, if (allow.cartesian || notjoin || !anyDuplicated(f__,  : 
# Join results in 984620 rows; more than 926611 = nrow(x)+nrow(i). Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice.

# Zusammenführen von Daten mit SVERWEIS = merge
dim(K.33.Ready)
dim(Y.01.2)
K33.Y01 = merge(K.33.Ready, Y.01.2, by="Tier.TVD", allow.cartesian=TRUE)
K33.Y01 <- as.data.table(K33.Y01)
dim(K33.Y01)
K33.Y01.K09 = merge(K33.Y01, K.09.2, by="Tier.TVD", allow.cartesian=TRUE)
K33.Y01.K09 <- as.data.table(K33.Y01.K09)
dim(K33.Y01.K09)
K33.Y01.K09.K04 = merge(K33.Y01.K09, Tier.Reg.2, by="Tier.TVD.Laktationsnummer", allow.cartesian=TRUE)
K33.Y01.K09.K04 <- as.data.table(K33.Y01.K09.K04)
dim(K33.Y01.K09.K04)
K33.Y01.K09.K04.K45=merge(K33.Y01.K09.K04, K.45.2, by="Tier.TVD", allow.cartesian=TRUE)
K33.Y01.K09.K04.K45 <- as.data.table(K33.Y01.K09.K04.K45)
dim(K33.Y01.K09.K04.K45)

K.33 <- 0
K.33.1 <- 0
K.33.2 <- 0
K.33.1K <- 0
Y.01.2 <- 0
K.09.2 <- 0
Tier.Reg.2 <- 0
K.45.2 <- 0
K33.Y01 <- 0
K33.Y01.K09 <- 0
K33.Y01.K09.K04 <- 0



K33.Y01.K09.K04.K45.BD=merge(K33.Y01.K09.K04.K45, Betriebsdurchschnitt.BetriebeMLPSHB, by.x="Betrieb_aktuell.SHB", by.y ="Betrieb_aktuell.BetriebeMLPSHB", allow.cartesian=TRUE)
K33.Y01.K09.K04.K45.BD <- as.data.table(K33.Y01.K09.K04.K45.BD)
dim(K33.Y01.K09.K04.K45.BD)
K33.Y01.K09.K04.K45.BD[1,]


#<- K33.Y01.K09.K04.K45.BD$

Betrieb.SHB <- K33.Y01.K09.K04.K45.BD$Betrieb_aktuell.SHB
Tier.TVD <- K33.Y01.K09.K04.K45.BD$Tier.TVD
Tier.TVD.Lakt <- K33.Y01.K09.K04.K45.BD$Tier.TVD.Laktationsnummer
Betrieb.TVD <- K33.Y01.K09.K04.K45.BD$Betrieb.TVD
Tier.TVD_ohneCH <- K33.Y01.K09.K04.K45.BD$Tier.TVD_ohneCH.x
Tier.RC <- K33.Y01.K09.K04.K45.BD$Tier.RasseCode
Tier.Kalbedatum <- K33.Y01.K09.K04.K45.BD$Tier.Kalbedatum
		Tier.Kalbedatum <- strptime(Tier.Kalbedatum,format="%Y%m%d")
Tier.Kalbemonat <- K33.Y01.K09.K04.K45.BD$Tier.Kalbemonat
Tier.Lakt <- K33.Y01.K09.K04.K45.BD$Tier.Laktationsnummer
Probe.Nr <- K33.Y01.K09.K04.K45.BD$Tier.Probennummer.x
Probe.Datum <- K33.Y01.K09.K04.K45.BD$Datum.Probewaegung.x
		Probe.Datum <- strptime(Probe.Datum,format="%Y%m%d")
Probe.Monat <- K33.Y01.K09.K04.K45.BD$Monat.Probewaegung.x
Probe.Milch <- K33.Y01.K09.K04.K45.BD$Milchkg.x
		Probe.Milch <- as.numeric(Probe.Milch)
Probe.Fett <- K33.Y01.K09.K04.K45.BD$Fett.x
		Probe.Fett <- as.numeric(Probe.Fett)
Probe.Eiweiss <- K33.Y01.K09.K04.K45.BD$Eiweiss.x
		Probe.Eiweiss <- as.numeric(Probe.Eiweiss)
Probe.Persistenz <- K33.Y01.K09.K04.K45.BD$Probepersistenz
		Probe.Persistenz <- as.numeric(Probe.Persistenz)
		Probe.Persistenz <- as.data.frame(Probe.Persistenz)
		Probe.Persistenz <- Probe.Persistenz %>% mutate(Probe.Persistenz = if_else(is.na(Probe.Persistenz),as.numeric(100),Probe.Persistenz))
Probe.Zellzahl <- K33.Y01.K09.K04.K45.BD$Zellzahl.x
		Probe.Zellzahl <- as.numeric(Probe.Zellzahl)
Probe.Harnstoff <- K33.Y01.K09.K04.K45.BD$Milchharnstoff.x
		Probe.Harnstoff <- as.numeric(Probe.Harnstoff)
Probe.Alphoehe <- K33.Y01.K09.K04.K45.BD$AlpHoehe.x
		Probe.Alphoehe <- as.numeric(Probe.Alphoehe)
		Probe.Alphoehe <- as.data.frame(Probe.Alphoehe)
		Probe.Alphoehe <- Probe.Alphoehe %>% mutate(Probe.Alphoehe = if_else(is.na(Probe.Alphoehe),as.numeric(5),Probe.Alphoehe))
Probe.Pruefmethode <- K33.Y01.K09.K04.K45.BD$Pruefmethode.x
		Probe.Pruefmethode <- as.numeric(Probe.Pruefmethode)
		Probe.Pruefmethode <- as.data.frame(Probe.Pruefmethode)
		Probe.Pruefmethode<- Probe.Pruefmethode %>% mutate(Probe.Pruefmethode = if_else(is.na(Probe.Pruefmethode),as.numeric(8),Probe.Pruefmethode))
Probe.Code <- K33.Y01.K09.K04.K45.BD$CodeWaegung.x
		Probe.Code <- as.numeric(Probe.Code)
		Probe.Code <- as.data.frame(Probe.Code)
		Probe.Code<- Probe.Code %>% mutate(Probe.Code = if_else(is.na(Probe.Code),as.numeric(0),Probe.Code))
#Tier.TVD_ohneCH2 <- K33.Y01.K09.K04.K45.BD$Tier.TVD_ohneCH.y
Probe1 <- K33.Y01.K09.K04.K45.BD$Tier.Probennummer.y
Probe1.Datum <- K33.Y01.K09.K04.K45.BD$Datum.Probewaegung.y
		Probe1.Datum <- strptime(Probe1.Datum,format="%Y%m%d")
Probe1.Monat <- K33.Y01.K09.K04.K45.BD$Monat.Probewaegung.y
Probe1.Milch <- K33.Y01.K09.K04.K45.BD$Milchkg.y
		Probe1.Milch <- as.numeric(Probe1.Milch)
Probe1.Fett <- K33.Y01.K09.K04.K45.BD$Fett.y
		Probe1.Fett <- as.numeric(Probe1.Fett)
		sum(is.na(Probe1.Fett))
Probe1.Eiweiss <- K33.Y01.K09.K04.K45.BD$Eiweiss.y
		Probe1.Eiweiss <- as.numeric(Probe1.Eiweiss)
Probe1.Zellzahl <- K33.Y01.K09.K04.K45.BD$Zellzahl.y
		Probe1.Zellzahl <- as.numeric(Probe1.Zellzahl)
Probe1.Harnstoff <- K33.Y01.K09.K04.K45.BD$Milchharnstoff.y
		Probe1.Harnstoff <- as.numeric(Probe1.Harnstoff)
Probe1.Alphoehe <- K33.Y01.K09.K04.K45.BD$AlpHoehe.y
		Probe1.Alphoehe <- as.numeric(Probe1.Alphoehe)
		Probe1.Alphoehe <- as.data.frame(Probe1.Alphoehe)
		Probe1.Alphoehe <- Probe1.Alphoehe %>% mutate(Probe1.Alphoehe = if_else(is.na(Probe1.Alphoehe),as.numeric(5),Probe1.Alphoehe))
Probe1.Pruefmethode <- K33.Y01.K09.K04.K45.BD$Pruefmethode.y
		Probe1.Pruefmethode <- as.numeric(Probe1.Pruefmethode)
		Probe1.Pruefmethode <- as.data.frame(Probe1.Pruefmethode)
		Probe1.Pruefmethode<- Probe1.Pruefmethode %>% mutate(Probe1.Pruefmethode = if_else(is.na(Probe1.Pruefmethode),as.numeric(0),Probe1.Pruefmethode))
Probe1.Code <- K33.Y01.K09.K04.K45.BD$CodeWaegung.y
		Probe1.Code <- as.numeric(Probe1.Code)
		Probe1.Code <- as.data.frame(Probe1.Code)
		Probe1.Code<- Probe1.Code %>% mutate(Probe1.Code = if_else(is.na(Probe1.Code),as.numeric(8),Probe1.Code))
Tier.Geburtsdatum <- K33.Y01.K09.K04.K45.BD$Tier.Geburtsdatum.Y01
		Tier.Geburtsdatum <- strptime(Tier.Geburtsdatum,format="%Y%m%d")
Tier.Abgangsdatum <- K33.Y01.K09.K04.K45.BD$Tier.Abgangsdatum.Y01
		Tier.Abgangsdatum <- strptime(Tier.Abgangsdatum,format="%Y%m%d")
		Tier.Abgangsdatum <- as.data.frame(Tier.Abgangsdatum)
		Tier.Abgangsdatum <- Tier.Abgangsdatum %>% mutate(Tier.Abgangsdatum = if_else(is.na(Tier.Abgangsdatum), Sys.time(),Tier.Abgangsdatum))
		Tier.Abgangsdatum <- Tier.Abgangsdatum$Tier.Abgangsdatum
		Tier.Abgangsdatum <- as.numeric(Tier.Abgangsdatum)
Tier.ZW.Milch <- K33.Y01.K09.K04.K45.BD$Tier.ZW.Mi_K09
		Tier.ZW.Milch <- as.numeric(Tier.ZW.Milch)
		Tier.ZW.Milch <- as.data.frame(Tier.ZW.Milch)
		Tier.ZW.Milch <- Tier.ZW.Milch %>% mutate(Tier.ZW.Milch = if_else(is.na(Tier.ZW.Milch),as.numeric(0),Tier.ZW.Milch))
Tier.ZW.Fett <- K33.Y01.K09.K04.K45.BD$Tier.ZW.Fe_K09
		Tier.ZW.Fett <- as.numeric(Tier.ZW.Fett)
		Tier.ZW.Fett <- as.data.frame(Tier.ZW.Fett)
		Tier.ZW.Fett<- Tier.ZW.Fett %>% mutate(Tier.ZW.Fett = if_else(is.na(Tier.ZW.Fett),as.numeric(0),Tier.ZW.Fett))
Tier.ZW.Eiweiss <- K33.Y01.K09.K04.K45.BD$Tier.ZW.Ei_K09
		Tier.ZW.Eiweiss <- as.numeric(Tier.ZW.Eiweiss)
		Tier.ZW.Eiweiss <- as.data.frame(Tier.ZW.Eiweiss)
		Tier.ZW.Eiweiss<- Tier.ZW.Eiweiss %>% mutate(Tier.ZW.Eiweiss = if_else(is.na(Tier.ZW.Eiweiss),as.numeric(0),Tier.ZW.Eiweiss))
Tier.ZW.Basis <- K33.Y01.K09.K04.K45.BD$Tier.ZWBasis_K09
Tier.ZW.Zellzahl <- K33.Y01.K09.K04.K45.BD$Tier.ZW.ZZ_K09
		Tier.ZW.Zellzahl <- as.numeric(Tier.ZW.Zellzahl)
		Tier.ZW.Zellzahl <- as.data.frame(Tier.ZW.Zellzahl)
		Tier.ZW.Zellzahl <- Tier.ZW.Zellzahl %>% mutate(Tier.ZW.Zellzahl = if_else(is.na(Tier.ZW.Zellzahl),as.numeric(100),Tier.ZW.Zellzahl))
Tier.ZW.Persistenz <- K33.Y01.K09.K04.K45.BD$Tier.ZW.Per_K09
		Tier.ZW.Persistenz <- as.numeric(Tier.ZW.Persistenz)
		Tier.ZW.Persistenz <- as.data.frame(Tier.ZW.Persistenz)
		Tier.ZW.Persistenz <- Tier.ZW.Persistenz %>% mutate(Tier.ZW.Persistenz = if_else(is.na(Tier.ZW.Persistenz),as.numeric(100),Tier.ZW.Persistenz))
Tier.ZW.Label <- K33.Y01.K09.K04.K45.BD$Tier.ZWLabel_K09
Tier.ZW.Mar <- K33.Y01.K09.K04.K45.BD$Tier.ZW.MaR_K09
		Tier.ZW.Mar <- as.numeric(Tier.ZW.Mar)
		Tier.ZW.Mar <- as.data.frame(Tier.ZW.Mar)
		Tier.ZW.Mar <- Tier.ZW.Mar %>% mutate(Tier.ZW.Mar = if_else(is.na(Tier.ZW.Mar),as.numeric(100),Tier.ZW.Mar))
Tier.Lakt.Region <- K33.Y01.K09.K04.K45.BD$Tier.Region.K04
		Tier.Lakt.Region <- as.numeric(Tier.Lakt.Region)
		Tier.Lakt.Region <- as.data.frame(Tier.Lakt.Region)
		Tier.Lakt.Region <- Tier.Lakt.Region %>% mutate_all(na_if,"") 
		Tier.Lakt.Region <- Tier.Lakt.Region %>% mutate_all(na_if," ") 
		Tier.Lakt.Region <- Tier.Lakt.Region %>% mutate_all(na_if,"  ") 
		Tier.Lakt.Region <- Tier.Lakt.Region %>% mutate_all(na_if,"   ") 
		Tier.Lakt.Region <- Tier.Lakt.Region %>% mutate_all(na_if,"    ") 
		Tier.Lakt.Region <- Tier.Lakt.Region %>% mutate_all(na_if,"     ")
Tier.Lebensleistung <- K33.Y01.K09.K04.K45.BD$Tier.Lebensleistung.K45
		Tier.Lebensleistung <- as.numeric(Tier.Lebensleistung)
Betrieb.Lebensleistung <- K33.Y01.K09.K04.K45.BD$Betrieb_Lebensleistung.BetriebeMLPSHB
Betrieb.Lebensleistung.Tag <- K33.Y01.K09.K04.K45.BD$Betrieb_Lebenstagesleistung.BetriebeMLPSHB
Betrieb.Zellzahl <- K33.Y01.K09.K04.K45.BD$Betrieb_ZZ.BetriebeMLPSHB
Betrieb.Rastzeit <- K33.Y01.K09.K04.K45.BD$Betrieb_RZ.BetriebeMLPSHB
Betrieb.ServicePeriode <- K33.Y01.K09.K04.K45.BD$Betrieb_SP.BetriebeMLPSHB
Betrieb.Lebensdauer <- K33.Y01.K09.K04.K45.BD$Betrieb_Lebensdauer.BetriebeMLPSHB
Betrieb.Nutzungsdauer <- K33.Y01.K09.K04.K45.BD$Betrieb_Nutzungsdauer.BetriebeMLPSHB

Tier.Kalbealter.Monate <- round((Tier.Kalbedatum-Tier.Geburtsdatum)/(365.25/12),1)
Probe1.DIM <- round((Probe1.Datum-Tier.Kalbedatum)/(60*60*24),0)
Probe1.FeEw <- round(Probe1.Fett/Probe1.Eiweiss,2)
Probe.DIM <- round((Probe.Datum-Tier.Kalbedatum)/(60*60*24),0)
Probe.FeEw <- round(Probe.Fett/Probe.Eiweiss,2)
Tier.Abgangsdatum<-as.numeric(Tier.Abgangsdatum)
Tier.Geburtsdatum<-as.numeric(Tier.Geburtsdatum)
Tier.Alter <- round(Tier.Abgangsdatum-Tier.Geburtsdatum,0)
Tier.Alter <- as.numeric(Tier.Alter)
Tier.Alter <- round(Tier.Alter/(60*60*24),0)
Tier.Lebenstagesleistung <- round((Tier.Lebensleistung/Tier.Alter),1)
		Tier.Lebenstagesleistung <- as.numeric(Tier.Lebenstagesleistung)
		Tier.Lebenstagesleistung <- as.data.frame(Tier.Lebenstagesleistung)



Probe.Datum <- K33.Y01.K09.K04.K45.BD$Datum.Probewaegung.x
Probe.Datum_ST <- strptime(Probe.Datum,format="%Y%m%d")

DatenfileMBA=data.table(Tier.TVD, Tier.RC, Tier.Kalbealter.Monate, Tier.Lebensleistung, Tier.Lebenstagesleistung, Tier.Lakt, Tier.Kalbemonat, Tier.Lakt.Region, 
Probe1, Probe1.Monat, Probe1.DIM, Probe1.Milch, Probe1.Fett, Probe1.Eiweiss, Probe1.FeEw, Probe1.Zellzahl, Probe1.Harnstoff, Probe1.Alphoehe, Probe1.Pruefmethode, Probe1.Code, 
Probe.Nr, Probe.Datum_ST, Probe.Monat, Probe.DIM, Probe.Milch, Probe.Fett, Probe.Eiweiss, Probe.FeEw, Probe.Zellzahl, Probe.Harnstoff, Probe.Alphoehe, Probe.Pruefmethode, Probe.Code, Probe.Persistenz, 
Tier.ZW.Basis, Tier.ZW.Label, Tier.ZW.Milch, Tier.ZW.Fett, Tier.ZW.Eiweiss, Tier.ZW.Zellzahl, Tier.ZW.Mar, Tier.ZW.Persistenz, 
Betrieb.SHB, Betrieb.Lebensleistung, Betrieb.Lebensleistung.Tag, Betrieb.Zellzahl, Betrieb.Rastzeit, Betrieb.ServicePeriode, Betrieb.Lebensdauer, Betrieb.Nutzungsdauer)
dim(DatenfileMBA)			
DatenfileMBA <- distinct(DatenfileMBA,DatenfileMBA$Tier.TVD,DatenfileMBA$Probe.Datum_ST, .keep_all = TRUE)
dim(DatenfileMBA)
DatenfileMBA<-DatenfileMBA[,1:50]
DatenfileMBA<-as.data.table(DatenfileMBA)
dim(DatenfileMBA)
DatenfileMBA <- DatenfileMBA[DatenfileMBA$Probe.Datum_ST>"2015-01-01",] 
dim(DatenfileMBA)
DatenfileMBA <- DatenfileMBA[DatenfileMBA$Probe.Persistenz<300,] 
dim(DatenfileMBA)
DatenfileMBA <- DatenfileMBA[DatenfileMBA$Tier.Lebenstagesleistung<40,] 
dim(DatenfileMBA)

#is.na(DatenfileMBA)
#any(is.na(DatenfileMBA)
#sum(is.na(DatenfileMBA))
summary(DatenfileMBA)
DatenfileMBA2 <- na.omit(DatenfileMBA)
dim(DatenfileMBA2)


#CSV generieren:
write.csv(DatenfileMBA2, file = "DatafileMBA_Ready.csv", row.names = FALSE)		





