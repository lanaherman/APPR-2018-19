# 2. faza: Uvoz podatkov
library(rvest)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  
  
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  
  tabela$regija[tabela$regija == "Spodnjeposavska"] <- "Posavska"
  tabela$regija[tabela$regija == "Notranjsko-kraška"] <- "Primorsko-notranjska"
  tabela$regija[tabela$regija == "Jugovzhodna"] <- "Jugovzhodna Slovenija"
  
  
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    #(tabela[[col]] != "-")
    #  tabela[[col]] <- 0
    #else 
    #  tabela[[col]] <- parse_number(tabela[[col]], na=c("", "NA", "-"), locale=sl, trim_ws = TRUE) */
    
  }
  
  
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% melt(id.vars="obcina", variable.name="velikost.druzine",
                        value.name="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

obcine2 <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.



library(dplyr)
library(rgdal)
library(rgeos)
library(mosaic)
library(maptools)
library(reshape2)
library(readr)

Tabela1 <- read.csv2("podatki/Kmetijska gospodarstva.csv", na=c("", " ", "..."))
colnames(Tabela1)=c("Kmetijska gospodarstva", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela1 <- Tabela1 %>% melt(id.vars="Kmetijska gospodarstva", variable.name="leto", value.name="stevilo")

Tabela2 <- read.csv2("podatki/Rastlinski pridelki.csv", na=c("", " ", "..."))
colnames(Tabela2)=c("Rastilnski pridelki", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela2 <- Tabela2 %>% melt(id.vars="Rastilnski pridelki", variable.name="leto", value.name="stevilo")

Tabela3 <- read.csv2("podatki/Stevilo zivine.csv", na=c("", " ", "..."))
colnames(Tabela3)=c("Zivina", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela3 <- Tabela3 %>% melt(id.vars="Zivina", variable.name="leto", value.name="stevilo")
Tabela3$Zivina <- gsub(" - SKUPAJ", "", Tabela3$Zivina)
Tabela3$Zivina <- tolower(Tabela3$Zivina)

Tabela4 <- read.csv2("podatki/Pridelava ekoloskih rastlinskih pridelkov.csv", na=c("", " ", "...", "-"), dec = ".")
colnames(Tabela4)=c("Ekoloski rastlinski pridelki", "2012", "2013", "2014", "2015", "2016", "2017")
Tabela4 <- Tabela4 %>% melt(id.vars="Ekoloski rastlinski pridelki", variable.name="leto", value.name="stevilo")
Tabela4$stevilo <- round(Tabela4$stevilo)

Tabela5 <- read.csv2("podatki/Stevilo zivali v ekoloski reji.csv", na=c("", " ", "...", "-"))
colnames(Tabela5)=c("Zivina v ekoloski reji", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
Tabela5 <- Tabela5 %>% melt(id.vars="Zivina v ekoloski reji", variable.name="leto", value.name="stevilo")
Tabela5$`Zivina v ekoloski reji` <- tolower(Tabela5$`Zivina v ekoloski reji`)
Tabela5 <- filter(Tabela5, `Zivina v ekoloski reji` != "čebele (število panjev)")
Tabela5$`Zivina v ekoloski reji` <- gsub("ostalo (divjad)", "jelenjad", Tabela5$`Zivina v ekoloski reji`)

Tabela6 <- read.csv2("podatki/Kmetijska gospodarstva - splosni pregled po statisticnih regijah.csv", na=c("", " ", "..."))
colnames(Tabela6)=c("Kmetijska gospodarstva po statisticnih regijah", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela6 <- Tabela6[2:13,]
Tabela6 <- Tabela6 %>% melt(id.vars="Kmetijska gospodarstva po statisticnih regijah", variable.name="leto", value.name="stevilo kmetijskih gospodarstev")

source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")


#kraji <- read.csv2("uvoz/SVN_adm2a.csv", na=c("", " ", "..."), sep=",",encoding="UTF-8")
obcine <- uvozi.obcine()


b <- Tabela6[Tabela6$leto==2010, ]
colnames(b) <- c("regija", "leto", "stevilo")

total <- merge(b, obcine,by=c("regija")) #Dobiš imena običn pod imenom regij
total$"obcina" <- toupper(total$"obcina")
names(total)[names(total) == "obcina"] <- "OB_IME"

zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                          pot.zemljevida="OB", encoding="Windows-1250") %>% fortify()

total$OB_IME <- gsub(" ", "", total$"OB_IME", fixed = TRUE)
zemljevid$OB_IME <- gsub(" ", "", zemljevid$"OB_IME", fixed = TRUE)
zemljevid$OB_IME[zemljevid$OB_IME == "SVETIANDRAŽVSLOV.GORICAH"] <- "SV.TROJICAVSLOV.GORICAH"
zemljevid$OB_IME[zemljevid$OB_IME == "SVETIJURIJVSLOV.GORICAH"] <- "SV.TROJICAVSLOV.GORICAH"

data <- merge(total, zemljevid,by=c("OB_IME")) #Dobiš koordinate od vseh obćin ter podatke za mapo
names(data)[names(data) == "stevilo"] <- "stevilo kmetijskih gospodarstev"

# distinct_df <- zemljevid %>% distinct(OB_IME)
# distinct_df <- sort(distinct_df$OB_IME)
# #h <- sort(distinct_df)
# distinct_df2 <- total %>% distinct(OB_IME)
# distinct_df2 <- sort(distinct_df2$OB_IME)
# g <- melt(data.frame(distinct_df,distinct_df2))
# X <- g[order(g$OB_IME, g$OB_IME1),]
# setdiff(distinct_df, distinct_df2)
# test <- merge(distinct_df, total,by=c("OB_IME")) #Dobiš imena običn pod imenom regij


#ggplot() + geom_polygon(data=data, aes(x=long, y=lat, group=group, fill="id")) +
#  guides(fill=FALSE)

 p <- ggplot(data=data, aes(x=long, y=lat)) +
   geom_polygon(aes(fill = `stevilo kmetijskih gospodarstev`, group = OB_IME))
 p
