# 2. faza: Uvoz podatkov
library(rvest)
library(dplyr)
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(readr)
library(ggplot2)
library(ggvis)
library(ggmap)
library(munsell)
library(digest)

Tabela1 <- read_delim("podatki/Kmetijska gospodarstva.csv", delim=";",
                      na=c("", " ", "...", "-"),
                      locale=locale(encoding="Windows-1250", decimal_mark="."))
colnames(Tabela1)=c("Kmetijska gospodarstva", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela1 <- Tabela1 %>% reshape2::melt(id.vars="Kmetijska gospodarstva", variable.name="leto", value.name="stevilo")

Tabela2 <- read_delim("podatki/Rastlinski pridelki.csv", delim=";",
                      na=c("", " ", "...", "-"),
                      locale=locale(encoding="Windows-1250", decimal_mark="."))
colnames(Tabela2)=c("Rastilnski pridelki", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela2 <- Tabela2 %>% reshape2::melt(id.vars="Rastilnski pridelki", variable.name="leto", value.name="stevilo")

Tabela3 <- read_delim("podatki/Stevilo zivine.csv", delim=";",
                      na=c("", " ", "...", "-"),
                      locale=locale(encoding="Windows-1250", decimal_mark="."))
colnames(Tabela3)=c("Zivina", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela3 <- Tabela3 %>% reshape2::melt(id.vars="Zivina", variable.name="leto", value.name="stevilo")
Tabela3$Zivina <- gsub(" - SKUPAJ", "", Tabela3$Zivina)
Tabela3$Zivina <- tolower(Tabela3$Zivina)

Tabela4 <- read_delim("podatki/Pridelava ekoloskih rastlinskih pridelkov.csv", delim=";",
                      na=c("", " ", "...", "-"),
                      locale=locale(encoding="Windows-1250", decimal_mark="."), skip=1)
colnames(Tabela4)=c("Ekoloski rastlinski pridelki", "2012", "2013", "2014", "2015", "2016", "2017")
Tabela4 <- Tabela4 %>% reshape2::melt(id.vars="Ekoloski rastlinski pridelki", variable.name="leto", value.name="stevilo")

Tabela5 <- read_delim("podatki/Stevilo zivali v ekoloski reji.csv", delim=";",
                      na=c("", " ", "...", "-"),
                      locale=locale(encoding="Windows-1250", decimal_mark="."))
colnames(Tabela5)=c("Zivina v ekoloski reji", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
Tabela5 <- Tabela5 %>% reshape2::melt(id.vars="Zivina v ekoloski reji", variable.name="leto", value.name="stevilo")
Tabela5$`Zivina v ekoloski reji` <- tolower(Tabela5$`Zivina v ekoloski reji`)
Tabela5 <- filter(Tabela5, `Zivina v ekoloski reji` != "čebele (število panjev)")
Tabela5$`Zivina v ekoloski reji` <- gsub("ostalo (divjad)", "jelenjad", Tabela5$`Zivina v ekoloski reji`)

Tabela6 <- read_delim("podatki/Kmetijska gospodarstva - splosni pregled po statisticnih regijah.csv", delim=";",
                      na=c("", " ", "...", "-"),
                      locale=locale(encoding="Windows-1250", decimal_mark="."))
colnames(Tabela6)=c("Kmetijska gospodarstva po statisticnih regijah", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela6 <- Tabela6[2:13,]
Tabela6 <- Tabela6 %>% reshape2::melt(id.vars="Kmetijska gospodarstva po statisticnih regijah", variable.name="leto", value.name="stevilo kmetijskih gospodarstev")

Tabela7 <- read_csv2("podatki/cene.csv", na=c("", " ", "...", "-"), skip=1,
                     locale=locale(encoding="Windows-1250"))
colnames(Tabela7) <- c("IZDELEK", 2000:2018)
Tabela7 <- Tabela7[-1,]
Tabela7 <- Tabela7 %>% reshape2::melt(id.vars="IZDELEK", variable.name="leto", value.name="povprecna cena")
Tabela7 <- Tabela7[Tabela7$leto %in% c(2000, 2003, 2005, 2007, 2010, 2013, 2016), ]
Tabela7$IZDELEK <- gsub('(\\D*)(\\s\\(.*$)', '\\1',as.character(Tabela7$IZDELEK))
colnames(Tabela7)=c("Izdelek", "leto", "povprecna cena")
