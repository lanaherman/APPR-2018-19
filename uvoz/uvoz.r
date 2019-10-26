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
library(ggplot2)
library(ggvis)
library(ggmap)
library(mapproj)
library(munsell)

Tabela1 <- read.csv2("podatki/Kmetijska gospodarstva.csv", na=c("", " ", "..."))
colnames(Tabela1)=c("Kmetijska gospodarstva", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela1 <- Tabela1 %>% reshape2::melt(id.vars="Kmetijska gospodarstva", variable.name="leto", value.name="stevilo")

Tabela2 <- read.csv2("podatki/Rastlinski pridelki.csv", na=c("", " ", "..."))
colnames(Tabela2)=c("Rastilnski pridelki", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela2 <- Tabela2 %>% reshape2::melt(id.vars="Rastilnski pridelki", variable.name="leto", value.name="stevilo")

Tabela3 <- read.csv2("podatki/Stevilo zivine.csv", na=c("", " ", "..."))
colnames(Tabela3)=c("Zivina", "2000", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela3 <- Tabela3 %>% reshape2::melt(id.vars="Zivina", variable.name="leto", value.name="stevilo")
Tabela3$Zivina <- gsub(" - SKUPAJ", "", Tabela3$Zivina)
Tabela3$Zivina <- tolower(Tabela3$Zivina)

Tabela4 <- read.csv2("podatki/Pridelava ekoloskih rastlinskih pridelkov.csv", na=c("", " ", "...", "-"), dec = ".")
colnames(Tabela4)=c("Ekoloski rastlinski pridelki", "2012", "2013", "2014", "2015", "2016", "2017")
Tabela4 <- Tabela4 %>% reshape2::melt(id.vars="Ekoloski rastlinski pridelki", variable.name="leto", value.name="stevilo")
Tabela4$stevilo <- round(Tabela4$stevilo)

Tabela5 <- read.csv2("podatki/Stevilo zivali v ekoloski reji.csv", na=c("", " ", "...", "-"))
colnames(Tabela5)=c("Zivina v ekoloski reji", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
Tabela5 <- Tabela5 %>% reshape2::melt(id.vars="Zivina v ekoloski reji", variable.name="leto", value.name="stevilo")
Tabela5$`Zivina v ekoloski reji` <- tolower(Tabela5$`Zivina v ekoloski reji`)
Tabela5 <- filter(Tabela5, `Zivina v ekoloski reji` != "čebele (število panjev)")
Tabela5$`Zivina v ekoloski reji` <- gsub("ostalo (divjad)", "jelenjad", Tabela5$`Zivina v ekoloski reji`)

Tabela6 <- read.csv2("podatki/Kmetijska gospodarstva - splosni pregled po statisticnih regijah.csv", na=c("", " ", "..."))
colnames(Tabela6)=c("Kmetijska gospodarstva po statisticnih regijah", "2003", "2005", "2007", "2010", "2013", "2016")
Tabela6 <- Tabela6[2:13,]
Tabela6 <- Tabela6 %>% reshape2::melt(id.vars="Kmetijska gospodarstva po statisticnih regijah", variable.name="leto", value.name="stevilo kmetijskih gospodarstev")


########################## ZEMLJEVID ############################################################
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

p <- ggplot() + geom_polygon(data=left_join(zemljevid, total, by=c("OB_IME"="OB_IME")),
                        aes(x=long, y=lat, group=group, fill=stevilo))
p
p + labs(fill = "stevilo kmetijskih gospodarstev v letu 2010")

 #p <- ggplot(data=data, aes(x=long, y=lat, group=group)) +
  # geom_polygon(aes(fill = `stevilo kmetijskih gospodarstev`, group = OB_IME))
# p
###############################################################################################

########## HISTOGRAM RASTLINSKIH PRIDELKOV ####################################################
c <- Tabela2[Tabela2$leto %in% c(2013, 2016), ]
c[is.na(c)] <- 0
d <- Tabela4[Tabela4$leto %in% c(2013, 2016), ]
d[is.na(d)] <- 0

Rastline <- aggregate(c$stevilo, by=list(c$leto), FUN=sum)
Eko_rastline <- aggregate(d$stevilo, by=list(d$leto), FUN=sum)
Rastline$vrsta="Rastlinski pridelki"
Eko_rastline$vrsta="Ekoloski rastlinski pridelki"

data_rastline <- merge(Rastline, Eko_rastline, all=TRUE)
colnames(data_rastline)=c("leto", "stevilo", "vrsta")
razmerje_2013 <- 100000*(data_rastline$stevilo[1]/data_rastline$stevilo[2])
razmerje_2016 <- 100000*(data_rastline$stevilo[3]/data_rastline$stevilo[4])

nov_data <- data.frame(leto=c("2013", "2016"), stevilo=c(razmerje_2013, razmerje_2016),
                       vrsta=c("Razmerje pomnoženo z 100000"))
nov_data_rastline <- rbind(data_rastline, nov_data)

graf_rast <- ggplot(data = nov_data_rastline, aes(x=leto, y=stevilo, fill=vrsta)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual("Legenda", values = c("Ekoloski rastlinski pridelki" = "orange4", "Rastlinski pridelki" = "green4", "Razmerje pomnoženo z 100000" = "gray13"))
graf_rast + labs(title="Histogram rastlinskih pridelkov", 
                x="Leto", y = "Stevilo rastlinskih pridelkov (vsota)")
##############################################################################################

####################### ČRTNI GRAFIKON KMETIJSKIH GOSPODARSTEV ###############################
Tabela2[is.na(Tabela2)] <- 0
rast_ne_eko <- aggregate(Tabela2$stevilo, by=list(Tabela2$leto), FUN=sum)
rast_ne_eko$pridelek="Rastlinski pridelki (* 100 kg)"
rast_ne_eko$x <- rast_ne_eko$x * 10

Tabela3[is.na(Tabela3)] <- 0
ziv_ne_eko <- aggregate(Tabela3$stevilo, by=list(Tabela3$leto), FUN=sum)
ziv_ne_eko$pridelek="Zivina"

data_ne_eko <- merge(rast_ne_eko, ziv_ne_eko, all=TRUE)
colnames(data_ne_eko)=c("leto", "stevilo", "pridelek")
data_ne_eko$leto <- as.numeric(as.character(data_ne_eko$leto))

require(scales)

graf_prid <- ggplot(data = data_ne_eko, aes(x=leto, y=stevilo, col=pridelek)) +
  #geom_point() + 
  geom_line(size=1.5) +
  scale_color_manual("Vrsta pridelka", values=c("Rastlinski pridelki (* 100 kg)" = "green4", "Zivina" = "hotpink4"))
graf_prid + labs(x = "Leto", y = "Stevilo", title = "Primerjava stevila zivine in rastlinskih pridelkov") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))

#tukaj napiši, da je v sloveniji veliko več kmetij z živino. razlaga je tukaj
#https://www.stat.si/statweb/News/Index/6742

##############################################################################################

############################# ALI JE VEDNO VEČ EKOLOŠKIH? ####################################
library(plyr)
library(dplyr)
library(data.table)

tab <- Tabela1[Tabela1$`Kmetijska gospodarstva` %in% c("Kmetijska gospodarstva", "Kmetijska gospodarstva z ekološkim kmetovanjem", "Kmetijska gospodarstva v postopku preusmeritve v ekološko kmetovanje"), ]
tab$`Kmetijska gospodarstva`[tab$`Kmetijska gospodarstva` == "Kmetijska gospodarstva v postopku preusmeritve v ekološko kmetovanje"] <- "Kmetijska gospodarstva z ekološkim kmetovanjem"
tab[is.na(tab)] <- 0

DT <- data.table(tab)
dt <- DT[, sum(stevilo), by = c("Kmetijska gospodarstva", "leto")]
colnames(dt)=c("Kmetijska gospodarstva", "leto", "stevilo")


for (row in 1:nrow(dt)) {
  leto_x <- dt[row, "leto"]
  tip  <- dt[row, "Kmetijska gospodarstva"]
  stevilo <- dt[row, "stevilo"]
  
  if(tip == "Kmetijska gospodarstva"){
  
   stevilo_eko <- (dt[as.numeric(dt$leto) == as.numeric(leto_x) & dt$`Kmetijska gospodarstva` == "Kmetijska gospodarstva z ekološkim kmetovanjem", "stevilo"])
   print(stevilo)
   print(stevilo_eko)
   dt[row, "stevilo"] <- stevilo - stevilo_eko
   
   }  

}
ggplot(data=dt, aes(x=leto, y=`Kmetijska gospodarstva`, size=stevilo)) + geom_point() +
  theme(panel.background = element_rect(fill = 'wheat1', colour = "black"))
##############################################################################################

################# HISTOGRAM PRIMERJAVA VRSTE ZIVINE V EKO IN NE EKO ##########################
library(ggplot2)
library(scales)

Tabela5$`Zivina v ekoloski reji`[Tabela5$`Zivina v ekoloski reji` == "kopitarji"] <- "konji"
Tabela5$`Zivina v ekoloski reji`[Tabela5$`Zivina v ekoloski reji` == "ostalo (divjad)"] <- "jelenjad"
colnames(Tabela5) = c("Zivina", "leto", "stevilo")

p <- Tabela5[Tabela5$leto %in% c(2016), ]
p$vrsta = "Ekoloske zivali"
p$leto <- NULL
p$vrsta <- NULL
p <- p %>% mutate(label1 = paste0(round(stevilo / sum(stevilo) * 100, 1)))

r <- Tabela3[Tabela3$leto %in% c(2016), ]
r$vrsta = "Neekoloske zivali"
r$leto <- NULL
r$vrsta <- NULL
r$Zivina[r$Zivina == "perutina"] <- "perutnina"
r <- r %>% mutate(label1 = paste0(round(stevilo / sum(stevilo) * 100, 1)))

bp <- ggplot(p, aes(x="", y=stevilo, fill=Zivina)) + geom_bar(width = 1, stat = "identity", color = "black")
tortni_eko_ziv <- bp + coord_polar("y", start=0) + scale_fill_brewer("Vrsta zivine", palette="Dark2") +
  theme_void() +
  geom_text(aes(label = paste0(round(as.numeric(label1)), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Struktura zivine")
tortni_eko_ziv

# GRDO IZPISE
br <- ggplot(r, aes(x="", y=stevilo, fill=Zivina)) + geom_bar(width = 1, stat = "identity", color = "black")
tortni_ne_eko_ziv <- br + coord_polar("y", start=0) + scale_fill_brewer("Vrsta zivine", palette="Dark2") +
  theme_void() +
  geom_text(aes(label = paste0(round(as.numeric(label1)), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Struktura ekoloske zivine") 
tortni_ne_eko_ziv

# NEUSPELI POIZKUS
r <- Tabela3[Tabela3$leto %in% c(2016), ]
r$vrsta = "Neekoloske zivali"
r$leto <- NULL
r$vrsta <- NULL
r$Zivina[r$Zivina == "perutina"] <- "perutnina"
r <- r %>% mutate(label1 = paste0(round(stevilo / sum(stevilo) * 100, 1)))
r <- r %>%
  mutate(end = 2 * pi * cumsum(stevilo)/sum(stevilo),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
library(ggforce) # for 'geom_arc_bar'
ggplot(r) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = Zivina)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = label1,
                hjust = hjust, vjust = vjust)) +
  coord_fixed() +
  scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL)

#data_zivali <- merge(p, r, all=TRUE)
#data_zivali <- data_zivali[-c(13, 14),]

#graf_rast <- ggplot(data = data_zivali, aes(x=Zivina, y=stevilo, fill=vrsta)) +
  #geom_bar(stat="identity", position="dodge") +
  #scale_fill_manual("Legenda", values = c("Ekoloske zivali" = "orange4", "Neekoloske zivali" = "green4"))
#graf_rast + labs(title="Histogram zivine", 
                 #x="Zivina", y = "Stevilo")
#https://stackoverflow.com/questions/9531904/plot-multiple-columns-on-the-same-graph-in-r
############################################################################################

############################# KORELACIJA KOKOŠI-CENA JAJC ######################################
cene <- read.csv2("podatki/cene.csv", na=c("", " ", "...", "-"), header = T, check.names=FALSE)
cene <- cene[-1,]
cene <- cene %>% reshape2::melt(id.vars="IZDELEK", variable.name="leto", value.name="povprecna cena")
cene <- cene[cene$leto %in% c(2000, 2003, 2005, 2007, 2010, 2013, 2016), ]
cene$IZDELEK <- gsub('(\\D*)(\\s\\(.*$)', '\\1',as.character(cene$IZDELEK))

cene_jajca <- cene[cene$IZDELEK %in% "Jajca, konzumna", ]
colnames(cene_jajca) = c("izdelek", "leto", "stevilo")
cene_jajca$stevilo <- as.numeric(gsub(",", ".", gsub("\\.", "", cene_jajca$stevilo)))
cene_jajca$stevilo <- cene_jajca$stevilo * 50000000

st_kokosi <- Tabela3[Tabela3$Zivina %in% "perutina", ]
colnames(st_kokosi) = c("izdelek", "leto", "stevilo")

primerjava_kokosi_cene_jajc <- merge(st_kokosi, cene_jajca, all=TRUE)
primerjava_kokosi_cene_jajc <- primerjava_kokosi_cene_jajc[order(primerjava_kokosi_cene_jajc$leto),]

graf_kok <- ggplot(data = primerjava_kokosi_cene_jajc, aes(x=leto, y=stevilo, col=izdelek, group = izdelek)) +
  geom_line(size=1.2) +
  scale_color_manual("Legenda", values=c("Jajca, konzumna" = "khaki2", "perutina" = "palevioletred1"))

graf_kok + labs(x = "Leto", y = "Stevilo", title = "Korelacija kokosi - cena jajc") +
  theme(panel.background = element_rect(fill = 'white', colour = "black"))
############################################################################################
