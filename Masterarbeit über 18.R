
#install.packages("readxl")
#install.packages("metafor")
#install.packages("dplyr")

library(readxl)
library(metafor)
library(dplyr)

excel <- read_excel("~/Uni Master/Masterarbeit/extraction/Extraktion.xlsx")

#Separieren nach Challenge und corrlelation
challenge <- subset (excel, analysis_type == "challenge")
correlation <- subset(excel, analysis_type == "correlation")

#Cohens'd
data_challenge <- escalc (measure="SMD",m1i = mean_hormone_cut_T1, m2i = mean_hormone_control_T1, sd1i = sd_hormone_cut_T1, sd2i = sd_hormon_control_T1, n1i = n_cut, n2i = n_control, data = challenge)

#Separieren nach CU-Gruppe und Kontrollgruppe 
data_CU <- subset(correlation, group == "CU")
data_HC <- subset(correlation, group == "HC")

#calculate r-to-z transformed correlations and corresponding sampling variances 
data_CU <- escalc(measure="ZCOR",ri = r_T1, ni=n_cut, data=data_CU)
data_HC <- escalc(measure="ZCOR",ri = r_T1, ni=n_control, data=data_HC)

#data_CU und data_HC zusammenführen 
gesamt<- rbind(data_CU, data_HC, data_challenge)

#Separieren nach Hormonen 
data_oxy <- subset(gesamt, hormone == "Oxytocin")
data_corti <- subset(gesamt, hormone == "Cortisol")
data_testo <- subset(gesamt, hormone == "Testosteron")

#doppelte löschen 
data_oxy_single <- data_oxy[-c(1,2,6),]
data_corti_single <- data_corti[-c(2,3,5,11,14,16,17),]
data_testo_single <- data_testo[-c(1,4,5,7,8),]

# Cohens'd in Fishers'z
data_corti_single[11, 68] <- -0.350
data_corti_single[11, 69] <- 0.043
data_testo_single[4, 69] <- 0.041

#Metanalysen berechnen 

######Oxytocin########
res_oxy <- rma(yi,vi, data=data_oxy_single, method ="REML", slab = paste (data_oxy_single$label))
res_oxy

#Rücktransformation
predict(res_oxy, transf=transf.ztor)

mod_oxy_age <- rma(yi,vi,mods =mean_age_total, data = data_oxy_single)
mod_oxy_age

mod_oxy_gender <- rma(yi,vi,mods =Prozent_männlich, data = data_oxy_single)
mod_oxy_gender

mod_oxy_ROB <- rma(yi,vi,mods =ROB, data = data_oxy_single)
mod_oxy_ROB

######Cortisol########
res_corti <- rma(yi,vi, data=data_corti_single, method ="REML", slab = paste(data_corti_single$label))
res_corti

#Rücktransformation
predict(res_corti, transf=transf.ztor)

#Moderatoranalyse Alter, Geschlecht und Phänotyp
data_corti_single$phenotype <- gsub("CS","1",data_corti_single$phenotype)
data_corti_single$phenotype <- gsub("HC","2",data_corti_single$phenotype)
data_corti_single$phenotype <- gsub("CU","3",data_corti_single$phenotype)
data_corti_single$phenotype <- as.numeric(data_corti_single$phenotype)

mod_corti_age <- rma(yi,vi,mods = mean_age_total, data = data_corti_single)
mod_corti_age

mod_corti_gender <- rma(yi,vi,mods = Prozent_männlich, data = data_corti_single)
mod_corti_gender

mod_corti_phenotype <- rma(yi,vi,mods = phenotype, data = data_corti_single)
mod_corti_phenotype

mod_corti_ROB <- rma(yi,vi,mods = ROB, data = data_corti_single)
mod_corti_ROB

#####Testosteron######
res_testo <- rma(yi,vi, data=data_testo_single, method ="REML", slab = paste (data_testo_single$label))
res_testo

#Rücktransformation
predict(res_testo, transf=transf.ztor)

mod_testo_age <- rma(yi,vi,mods = mean_age_total, data = data_testo_single)
mod_testo_age

mod_testo_gender <- rma(yi,vi,mods = Prozent_männlich, data = data_testo_single)
mod_testo_gender

mod_testo_ROB <- rma(yi,vi,mods = ROB, data = data_testo_single)
mod_testo_ROB

#Forest plot Cortisol 
forest(res_corti,
       addpred = TRUE,
       xlab="Effektgrößen",
       xlim=c(-3,2), at =seq(-1.5,1,by=0.5),
       digits=c(2,1),
       header = c("Autor","z [95% KI]"),
       cex = 1.0, #font size
       shade = TRUE, 
       width =5)

#Forest Plot Testosteron 

forest(res_testo,
       addpred = TRUE,
       xlab="Effektgrößen",
       xlim=c(-3,2), at =seq(-1.5,1,by=0.5),
       digits=c(2,1),
       header = c("Autor","z [95% KI]"),
       cex = 1.0, #font size
       shade = TRUE, 
       width=5)

#Forest Plot Oxytocin 

forest(res_oxy,
       addpred= TRUE,
       xlab="Effektgrößen",
       xlim=c(-3,2), at =seq(-1.5,1,by=0.5),
       digits=c(2,1),
       header = c("Autor","z [95% KI]"),
       cex = 1.0, #font size
       shade = TRUE,
       width=5)

#testing for influential studies (baujat) - cortisol
baujat(res_corti,
       xlab="Quadratische Pearson-Residuen",
       ylab="Einfluss auf das Gesamtergebnis",
       symbol="slab",
)
inf_corti <- influence(res_corti)
inf_corti

#testing for influential studies (baujat) - oxytocin
baujat(res_oxy,
       xlab="Quadratische Pearson-Residuen",
       ylab="Einfluss auf das Gesamtergebnis",
       symbol="slab",
)
inf_oxy <- influence(res_oxy)
inf_oxy

#testing for influential studies (baujat) - testosteron
baujat(res_testo,
       xlab="Quadratische Pearson-Residuen",
       ylab="Einfluss auf das Gesamtergebnis",
       symbol="slab",
)
inf_testo <- influence(res_testo)
inf_testo

#Publication bias - Cortisol
funnel(res_corti,
       xlab = "Standardisierte Korrelationen",
       ylab = "Standardfehler")
regtest(res_corti)
corti_tf <-trimfill(res_corti)
corti_tf

#Publication bias - Testosteron
funnel(res_testo,
       xlab = "Standardisierte Korrelationen",
       ylab = "Standardfehler")
regtest(res_testo)
testo_tf <-trimfill(res_testo)
testo_tf

#Publication bias - Oxytocin
funnel(res_oxy, 
       xlab = "Standardisierte Korrelationen",
       ylab = "Standardfehler")
regtest(res_oxy)
oxy_tf <-trimfill(res_oxy)
oxy_tf



