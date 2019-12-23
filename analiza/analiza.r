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

# for (row in 1:nrow(dt)) {
#   tip  <- dt[row, "Kmetijska gospodarstva"]
#   stevilo <- dt[row, "stevilo"]
#   if(tip == "Kmetijska gospodarstva z ekoloskim kmetovanjem (* 10)"){
#     dt[row, "stevilo"] <- stevilo * 10}}

colnames(dt)=c("Kmetije", "leto", "stevilo")

tabela_kmet_gosp <- dt[Kmetije == "Kmetijska gospodarstva z ekoloskim kmetovanjem"]
tabela_kmet_gosp$Kmetije <- NULL
tabela_kmet_gosp <- as.data.frame(tabela_kmet_gosp)
tabela_kmet_gosp$leto <- as.numeric(as.character(tabela_kmet_gosp$leto))
tabela_kmet_eko_gosp <- dt %>% filter(Kmetije == "Kmetijska gospodarstva")
tabela_kmet_eko_gosp$Kmetije <- NULL
tabela_kmet_eko_gosp <- as.data.frame(tabela_kmet_eko_gosp)
tabela_kmet_eko_gosp$leto <- as.numeric(as.character(tabela_kmet_eko_gosp$leto))
tabela_kmet_gosp_lm <- lm(data = tabela_kmet_gosp, formula = leto ~ stevilo)
tabela_kmet_eko_gosp_lm <- lm(data = tabela_kmet_eko_gosp, formula = leto ~ stevilo)
nova_leta <- data.frame(stevilo=seq(2017,2022,1))
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

