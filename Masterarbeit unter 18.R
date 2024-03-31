
#install.packages("readxl")
#install.packages("metafor")
#install.packages("dplyr")

library(readxl)
library(metafor)
library(dplyr)

excel <- read_excel("C:/Users/tamar/Desktop/Extraktion_U18.xlsx")

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
data_corti_single <- data_corti[-c(2,3,5,11,13,14),]

# Cohens'd in Fishers'z
data_corti_single[9, 68] <- -0.350
data_corti_single[9, 69] <- 0.043

#Metanalysen berechnen 

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

#testing for influential studies (baujat) - cortisol
baujat(res_corti,
       xlab="Quadratische Pearson-Residuen",
       ylab="Einfluss auf das Gesamtergebnis",
       symbol="slab",
)
inf_corti <- influence(res_corti)
inf_corti

#Publication bias - Cortisol
funnel(res_corti,
       xlab = "Standardisierte Korrelationen",
       ylab = "Standardfehler")
regtest(res_corti)
corti_tf <-trimfill(res_corti)
corti_tf





