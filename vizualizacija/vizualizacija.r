# 3. faza: Vizualizacija podatkov
########################## ZEMLJEVID ############################################################
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

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
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

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

zemljevid_slovenije <- ggplot() + geom_polygon(data=left_join(zemljevid, total, by=c("OB_IME"="OB_IME")),
                                               aes(x=long, y=lat, group=group, fill=stevilo))
zemljevid_slovenije <- zemljevid_slovenije + labs(fill = "stevilo kmetijskih gospodarstev v letu 2010")
zemljevid_slovenije
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
                       vrsta=c("Razmerje (* 100000)"))
nov_data_rastline <- rbind(data_rastline, nov_data)

graf_rast <- ggplot(data = nov_data_rastline, aes(x=leto, y=stevilo, fill=vrsta)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual("Legenda", values = c("Ekoloski rastlinski pridelki" = "orange4", "Rastlinski pridelki" = "green4", "Razmerje (* 100000)" = "gray13"))
graf_rast <- graf_rast + labs(title="Histogram rastlinskih pridelkov", 
                              x="Leto", y = "Stevilo rastlinskih pridelkov (vsota)")
graf_rast
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
  geom_line(size=1.5) +
  scale_color_manual("Vrsta pridelka", values=c("Rastlinski pridelki (* 100 kg)" = "green4", "Zivina" = "hotpink4"))
graf_prid <- graf_prid + labs(x = "Leto", y = "Stevilo", title = "Primerjava stevila zivine in rastlinskih pridelkov") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
graf_prid
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
tab$`Kmetijska gospodarstva`[tab$`Kmetijska gospodarstva` == "Kmetijska gospodarstva z ekološkim kmetovanjem"] <- "Kmetijska gospodarstva z ekoloskim kmetovanjem (* 10)"

DT <- data.table(tab)
dt <- DT[, sum(stevilo), by = c("Kmetijska gospodarstva", "leto")]
colnames(dt)=c("Kmetijska gospodarstva", "leto", "stevilo")
dt$stevilo[4] <- 1400

for (row in 1:nrow(dt)) {
  leto_x <- dt[row, "leto"]
  tip  <- dt[row, "Kmetijska gospodarstva"]
  stevilo <- dt[row, "stevilo"]
  if(tip == "Kmetijska gospodarstva"){
    stevilo_eko <- (dt[as.numeric(dt$leto) == as.numeric(leto_x) & dt$`Kmetijska gospodarstva` == "Kmetijska gospodarstva z ekoloskim kmetovanjem (* 10)", "stevilo"])
    dt[row, "stevilo"] <- stevilo - stevilo_eko}}

for (row in 1:nrow(dt)) {
  tip  <- dt[row, "Kmetijska gospodarstva"]
  stevilo <- dt[row, "stevilo"]
  if(tip == "Kmetijska gospodarstva z ekoloskim kmetovanjem (* 10)"){
    dt[row, "stevilo"] <- stevilo * 10}}

colnames(dt)=c("Kmetije", "leto", "stevilo")

graf_rast <- ggplot(data = dt, aes(x=leto, y=stevilo, fill=Kmetije)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual("Legenda", values = c("Kmetijska gospodarstva" = "darkgreen", "Kmetijska gospodarstva z ekoloskim kmetovanjem (* 10)" = "yellowgreen"))
graf_rast <- graf_rast + labs(title="Histogram kmetijskih gospodarstev", 
                              x="Leto", y = "Stevilo kmetijskih gospodarstev")
graf_rast
##############################################################################################

################# HISTOGRAM PRIMERJAVA VRSTE ZIVINE V EKO IN NE EKO ##########################
library(ggplot2)
library(scales)
library(ggforce)

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

br <- ggplot(r %>% arrange(desc(Zivina)) %>%
               mutate(end_angle=2*pi*cumsum(stevilo)/sum(r$stevilo),
                      start_angle=lag(end_angle, default = 0),
                      mid_angle=0.5*(start_angle + end_angle),
                      hjust=ifelse(mid_angle>pi, 1, 0),
                      vjust=ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))) +
  geom_arc_bar(aes(x0=0, y0=0, r0=0, r=1,
                   start=start_angle, end=end_angle, fill=Zivina))
tortni_ne_eko_ziv <- br + coord_fixed() +
  scale_fill_brewer("Vrsta zivine", palette="Dark2") + theme_void() +
  geom_text(aes(label=label1 %>% as.numeric() %>% round() %>% ifelse(paste0(., "%"), ""),
                x=1.03*sin(mid_angle), y=1.03*cos(mid_angle), hjust=hjust, vjust=vjust)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Struktura ekoloske zivine")
tortni_ne_eko_ziv
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
st_kokosi$Zivina[st_kokosi$Zivina == "perutina"] <- "Perutnina"
colnames(st_kokosi) = c("izdelek", "leto", "stevilo")

primerjava_kokosi_cene_jajc <- merge(st_kokosi, cene_jajca, all=TRUE)
primerjava_kokosi_cene_jajc <- primerjava_kokosi_cene_jajc[order(primerjava_kokosi_cene_jajc$leto),]

graf_kok <- ggplot(data = primerjava_kokosi_cene_jajc, aes(x=leto, y=stevilo, col=izdelek, group = izdelek)) +
  geom_line(size=1.2) +
  scale_color_manual("Legenda", values=c("Jajca, konzumna" = "khaki2", "Perutnina" = "palevioletred1"))

graf_kok <- graf_kok + labs(x = "Leto", y = "Stevilo", title = "Korelacija kokosi in cene jajc") +
  theme(panel.background = element_rect(fill = 'white', colour = "black")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
graf_kok
############################################################################################
