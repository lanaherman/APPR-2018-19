# 3. faza: Vizualizacija podatkov
########################## ZEMLJEVID ############################################################
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

b <- Tabela6[Tabela6$leto==2010, ]
colnames(b) <- c("NAME_1", "leto", "stevilo")
b$NAME_1[b$NAME_1 == "Posavska"] <- "Spodnjeposavska"
b$NAME_1[b$NAME_1 == "Primorsko-notranjska"] <- "Notranjsko-kraška"

zemljevid <- uvozi.zemljevid(url = "https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip",
                             ime.zemljevida = "gadm36_SVN_1",
                             encoding="UTF-8") %>% fortify()
zemljevid$NAME_1 <- as.character(zemljevid$NAME_1)

total1 <- merge(b, zemljevid, by=c("NAME_1"))

zemljevid_slovenije <- ggplot() + geom_polygon(data=left_join(zemljevid, total1, by=c("NAME_1"="NAME_1")),
                                               aes(x=long.x, y=lat.x, group=group.x, fill=stevilo))
zemljevid_slovenije <- zemljevid_slovenije + labs(fill = "Število kmetijskih gospodarstev v letu 2010") +
  xlab("") + ylab("")
zemljevid_slovenije
###############################################################################################

########## STOLPIČNI GRAF RASTLINSKIH PRIDELKOV ####################################################
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
graf_rast <- graf_rast + labs(title="Stolpični graf rastlinskih pridelkov", 
                              x="Leto", y = "Stevilo rastlinskih pridelkov (vsota)")
graf_rast
##############################################################################################

####################### ČRTNI GRAFIKON KMETIJSKIH GOSPODARSTEV ###############################
kk <- Tabela2
kk[is.na(kk)] <- 0
rast_ne_eko <- aggregate(kk$stevilo, by=list(kk$leto), FUN=sum)
rast_ne_eko$pridelek="Rastlinski pridelki (* 100 kg)"
rast_ne_eko$x <- rast_ne_eko$x * 10

Tabela3[is.na(Tabela3)] <- 0
ziv_ne_eko <- aggregate(Tabela3$stevilo, by=list(Tabela3$leto), FUN=sum)
ziv_ne_eko$pridelek="Živina"

data_ne_eko <- merge(rast_ne_eko, ziv_ne_eko, all=TRUE)
colnames(data_ne_eko)=c("leto", "stevilo", "pridelek")
data_ne_eko$leto <- as.numeric(as.character(data_ne_eko$leto))

require(scales)

graf_prid <- ggplot(data = data_ne_eko, aes(x=leto, y=stevilo, col=pridelek)) +
  geom_line(size=1.5) +
  scale_color_manual("Vrsta pridelka", values=c("Rastlinski pridelki (* 100 kg)" = "green4", "Živina" = "hotpink4"))
graf_prid <- graf_prid + labs(x = "Leto", y = "Stevilo", title = "Primerjava stevila zivine in rastlinskih pridelkov") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
graf_prid
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
tortni_eko_ziv <- bp + coord_polar("y", start=0) + scale_fill_brewer("Vrsta živine", palette="Dark2") +
  theme_void() +
  geom_text(aes(label = paste0(round(as.numeric(label1)), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Struktura živine")
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
  scale_fill_brewer("Vrsta živine", palette="Dark2") + theme_void() +
  geom_text(aes(label=label1 %>% as.numeric() %>% round() %>% ifelse(paste0(., "%"), ""),
                x=1.03*sin(mid_angle), y=1.03*cos(mid_angle), hjust=hjust, vjust=vjust)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Struktura ekološke živine")
tortni_ne_eko_ziv
############################################################################################

############################# KORELACIJA KOKOŠI-CENA JAJC #####################################
cene_jajca <- Tabela7[Tabela7$Izdelek %in% "Jajca, konzumna", ]
colnames(cene_jajca) = c("izdelek", "leto", "stevilo")
cene_jajca$stevilo <- as.numeric(cene_jajca$stevilo)
#cene_jajca$stevilo <- as.numeric(gsub(",", ".", gsub("\\.", "", cene_jajca$stevilo)))
cene_jajca$stevilo <- cene_jajca$stevilo

st_kokosi <- Tabela3[Tabela3$Zivina %in% "perutina", ]
st_kokosi$Zivina[st_kokosi$Zivina == "perutina"] <- "Perutnina"
colnames(st_kokosi) = c("izdelek", "leto", "stevilo")

primerjava_kokosi_cene_jajc <- merge(st_kokosi, cene_jajca, all=TRUE)
primerjava_kokosi_cene_jajc <- primerjava_kokosi_cene_jajc[order(primerjava_kokosi_cene_jajc$leto),]

graf_kok <- ggplot(data = primerjava_kokosi_cene_jajc, aes(x=leto, y=stevilo, col=izdelek, group = izdelek)) +
  geom_line(size=1.2) +
  labs(x = "Leto", y = "Število", title = "Gibanje števila kokoši in cene jajc skozi leta") +
  theme(panel.background = element_rect(fill = 'white', colour = "black")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  facet_grid(izdelek~., scales="free_y")
graf_kok <- graf_kok + theme(legend.position = "none")
graf_kok
############################################################################################
