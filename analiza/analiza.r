# 4. faza: Analiza podatkov

#Slovenija
library(plyr)
library(dplyr)
library(data.table)

tab <- Tabela1[Tabela1$`Kmetijska gospodarstva` %in% c("Kmetijska gospodarstva", "Kmetijska gospodarstva z ekološkim kmetovanjem", "Kmetijska gospodarstva v postopku preusmeritve v ekološko kmetovanje"), ]
tab$`Kmetijska gospodarstva`[tab$`Kmetijska gospodarstva` == "Kmetijska gospodarstva v postopku preusmeritve v ekološko kmetovanje"] <- "Kmetijska gospodarstva z ekološkim kmetovanjem"
tab[is.na(tab)] <- 0
tab$`Kmetijska gospodarstva`[tab$`Kmetijska gospodarstva` == "Kmetijska gospodarstva z ekološkim kmetovanjem"] <- "Kmetijska gospodarstva z ekoloskim kmetovanjem"

DT <- data.table(tab)
dt <- DT[, sum(stevilo), by = c("Kmetijska gospodarstva", "leto")]
colnames(dt)=c("Kmetijska gospodarstva", "leto", "stevilo")
dt$stevilo[4] <- 1400

for (row in 1:nrow(dt)) {
  leto_x <- dt[row, "leto"]
  tip  <- dt[row, "Kmetijska gospodarstva"]
  stevilo <- dt[row, "stevilo"]
  if(tip == "Kmetijska gospodarstva"){
    stevilo_eko <- (dt[as.numeric(dt$leto) == as.numeric(leto_x) & dt$`Kmetijska gospodarstva` == "Kmetijska gospodarstva z ekoloskim kmetovanjem", "stevilo"])
    dt[row, "stevilo"] <- stevilo - stevilo_eko}}

colnames(dt)=c("Kmetije", "leto", "stevilo")

tabela_kmet_gosp <- dt[Kmetije == "Kmetijska gospodarstva"]
tabela_kmet_gosp$Kmetije <- NULL
tabela_kmet_gosp <- as.data.frame(tabela_kmet_gosp)
tabela_kmet_gosp$leto <- as.numeric(as.character(tabela_kmet_gosp$leto))
tabela_kmet_gosp_lm <- lm(formula = stevilo ~ leto, data = tabela_kmet_gosp)

tabela_kmet_eko_gosp <- dt[Kmetije == "Kmetijska gospodarstva z ekoloskim kmetovanjem"]
tabela_kmet_eko_gosp$Kmetije <- NULL
tabela_kmet_eko_gosp <- as.data.frame(tabela_kmet_eko_gosp)
tabela_kmet_eko_gosp$leto <- as.numeric(as.character(tabela_kmet_eko_gosp$leto))
tabela_kmet_eko_gosp_lm <- lm(formula = stevilo ~ leto, data = tabela_kmet_eko_gosp)

nova_leta <- data.frame(leto=seq(2017,2022,1))

napoved <- mutate(nova_leta, n=predict(tabela_kmet_gosp_lm, nova_leta))
colnames(napoved) <- c("leto", "stevilo")
napoved$Kmetije <- "Kmetijska gospodarstva"

napoved_eko <- mutate(nova_leta, n=predict(tabela_kmet_eko_gosp_lm, nova_leta))
colnames(napoved_eko) <- c("leto", "stevilo")
napoved_eko$Kmetije <- "Kmetijska gospodarstva z ekoloskim kmetovanjem"

nov_data_napoved <- merge(napoved, napoved_eko, all = TRUE)
nov_data_napoved_cela <- merge(nov_data_napoved, dt, all = TRUE)

nov_data_napoved_cela <- nov_data_napoved_cela[seq_len(nrow(nov_data_napoved_cela)) + c(1,-1),]

#Svet
nova_leta_svet <- data.frame(Year=seq(2018,2023,1))

tabela_Svet <- Tabela_svet %>% filter(Area == "Svet")
tabela_Svet$Area <- NULL
tabela_Svet <- as.data.frame(tabela_Svet)
tabela_Svet_lm <- lm(formula = Value ~ Year, data = tabela_Svet)
napoved_Svet <- mutate(nova_leta_svet, n=predict(tabela_Svet_lm, nova_leta_svet))
colnames(napoved_Svet) <- c("Year", "Value")
napoved_Svet$Area <- "Svet"

tabela_Afrika <- Tabela_svet %>% filter(Area == "Afrika")
tabela_Afrika$Area <- NULL
tabela_Afrika <- as.data.frame(tabela_Afrika)
tabela_Afrika_lm <- lm(formula = Value ~ Year, data = tabela_Afrika)
napoved_Afrika <- mutate(nova_leta_svet, n=predict(tabela_Afrika_lm, nova_leta_svet))
colnames(napoved_Afrika) <- c("Year", "Value")
napoved_Afrika$Area <- "Afrika"

tabela_Amerika <- Tabela_svet %>% filter(Area == "Amerika")
tabela_Amerika$Area <- NULL
tabela_Amerika <- as.data.frame(tabela_Amerika)
tabela_Amerika_lm <- lm(formula = Value ~ Year, data = tabela_Amerika)
napoved_Amerika <- mutate(nova_leta_svet, n=predict(tabela_Amerika_lm, nova_leta_svet))
colnames(napoved_Amerika) <- c("Year", "Value")
napoved_Amerika$Area <- "Amerika"

tabela_Azija <- Tabela_svet %>% filter(Area == "Azija")
tabela_Azija$Area <- NULL
tabela_Azija <- as.data.frame(tabela_Afrika)
tabela_Azija_lm <- lm(formula = Value ~ Year, data = tabela_Afrika)
napoved_Azija <- mutate(nova_leta_svet, n=predict(tabela_Azija_lm, nova_leta_svet))
colnames(napoved_Azija) <- c("Year", "Value")
napoved_Azija$Area <- "Azija"

tabela_Evropa <- Tabela_svet %>% filter(Area == "Evropa")
tabela_Evropa$Area <- NULL
tabela_Evropa <- as.data.frame(tabela_Evropa)
tabela_Evropa_lm <- lm(formula = Value ~ Year, data = tabela_Evropa)
napoved_Evropa <- mutate(nova_leta_svet, n=predict(tabela_Evropa_lm, nova_leta_svet))
colnames(napoved_Evropa) <- c("Year", "Value")
napoved_Evropa$Area <- "Evropa"

tabela_Oceanija <- Tabela_svet %>% filter(Area == "Oceanija")
tabela_Oceanija$Area <- NULL
tabela_Oceanija <- as.data.frame(tabela_Oceanija)
tabela_Oceanija_lm <- lm(formula = Value ~ Year, data = tabela_Oceanija)
napoved_Oceanija <- mutate(nova_leta_svet, n=predict(tabela_Oceanija_lm, nova_leta_svet))
colnames(napoved_Oceanija) <- c("Year", "Value")
napoved_Oceanija$Area <- "Oceanija"

nova_tabela_svet <- Reduce(function(x, y) merge(x,y,all=TRUE),
                           list(napoved_Svet, napoved_Afrika, napoved_Amerika, napoved_Azija, napoved_Evropa, napoved_Oceanija))
Tabela_svet_napoved <- merge(nova_tabela_svet, Tabela_svet, all=TRUE)
