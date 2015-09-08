# Main


############### Funktion för att testa vilken miljö vi sitter i ################
.is.inca <- function(){
  unname(!(Sys.info()["sysname"] == "Darwin"))
}

if (!.is.inca()){
  setwd("~/Documents/Github/HH_Survival")
  rm(list=ls())
  df <- read.csv2("HH.txt")
}


################################# Ladda paket ##################################
library(dplyr)
library(relsurv)
library(ggplot2)
library(gdata)
library(stringr)


############################### Ladda funktioner ###############################
files <- c("fun_surv_est.R", "fun_surv_plot.R", "fun_popmort_offline.R")
path <- if (!.is.inca()) "~/Documents/Github/HH_Survival/" else "D:/R-Scripts/Väst/Oc5hoer/funktioner/"
lapply(paste0(path, files), source, encoding = "UTF-8")


############################ Laddning av parametrar ############################
if (!.is.inca()) Från <- 2010
if (!.is.inca()) Till <- 2013
if (!.is.inca()) Diagnos <- "Samtliga diagnoser"
if (!.is.inca()) Stadie <- "Samtliga stadier"
if (!.is.inca()) Stratum <- "Aggregerat"
if (!.is.inca()) Relativ <- "Relativ överlevnad"
if (!.is.inca()) CI <- "Nej"
if (!.is.inca()) Minålder <- 18
if (!.is.inca()) Maxålder <- 110
# INCA
if (.is.inca()) Från <- as.numeric(param[["Från"]])
if (.is.inca()) Till <- as.numeric(param[["Till"]])
if (.is.inca()) Diagnos <- as.character(param[["Diagnos"]])
if (.is.inca()) Stadie <- as.character(param[["Stadie"]])
if (.is.inca()) Stratum <- as.character(param[["Stratum"]])
if (.is.inca()) Relativ <- as.character(param[["Relativ"]])
if (.is.inca()) CI <- as.character(param[["CI"]])
if (.is.inca()) Minålder <- as.numeric(param[["Minålder"]])
if (.is.inca()) Maxålder <- as.numeric(param[["Maxålder"]])
  

  
  
########################### Förberadande bearbetning ###########################
names(df) <- tolower(names(df))
df_HH <- df %>% 
  mutate(stadie_grupp = str_trim(substring(as.character(a_stadium_beskrivning),1,3)),
         diagnos_grupp = str_trim(gsub("[[:digit:]]","", a_icd10_gruppnamn))) %>% 
  filter(region_namn != "Region Demo",
         stadie_grupp %in% c("I", "II", "III", "IV", "-"),
         diagnos_grupp != "",
         vitalstatusdatum_estimat != "",
         a_diadat != "",
         !is.na(a_alder)  
         ) %>% 
  arrange(a_diadat) %>% 
  distinct(pat_id)


############################ Filtrering på stadie #############################
if (!("Samtliga stadier" %in% Stadie)) df_HH <- df_HH %>% 
  filter(stadie_grupp %in% Stadie)
Stadie <- if (!("Samtliga stadier" %in% Stadie)) {
  paste(Stadie, collapse=",")
} else {
  "Samtliga stadier"
}
  
  

############################ Filtrering på diagnos #############################
if (!("Samtliga diagnoser" %in% Diagnos)) df_HH <- df_HH %>%
  filter(diagnos_grupp %in% Diagnos)
Diagnos <- if (!("Samtliga diagnoser" %in% Diagnos)) {
  paste(Diagnos, collapse=",")
} else {
  "Samtliga diagnoser"
}


######################### Skapa text för valda urvalet #########################
Urval <- paste0("(Urval: Diagnosår: ",Från,"-",Till,", Diagnos: ",Diagnos,", Stadie: ", Stadie, ", Ålder: ", Minålder, "-",Maxålder, ")")








####################### Aggregerad presentation eller ej #######################
if (("Aggregerat" == Stratum)) {
  surv <- surv_est(df_HH, diagnosdatum_namn = "a_diadat", kön_namn = "kon_value", vitaldatum_namn = "vitalstatusdatum_estimat",
                   vitalstatus_namn = "vitalstatus", age_namn = "a_alder", relativ = (Relativ == "Relativ överlevnad")) 
  # Skapa titel
  Titel <- paste0(Relativ,"\n", Urval)  # Skapa plot
  surv_plot(surv, main = Titel, CI = (CI == "Ja"))
  
  
} else if ("Per stadie" == Stratum) {
  surv <- surv_est(df_HH, diagnosdatum_namn = "a_diadat", kön_namn = "kon_value", vitaldatum_namn = "vitalstatusdatum_estimat",
                   vitalstatus_namn = "vitalstatus", age_namn = "a_alder", stratum_var_namn = "stadie_grupp", relativ = (Relativ == "Relativ överlevnad"))  
  # Skapa titel
  Titel <- paste0(Relativ," per stadium \n", Urval)
  # Skapa plot
  surv_plot(surv, legend = TRUE, main = Titel, CI = (CI == "Ja")) 
} else {
  surv <- surv_est(df_HH, diagnosdatum_namn = "a_diadat", kön_namn = "kon_value", vitaldatum_namn = "vitalstatusdatum_estimat",
                   vitalstatus_namn = "vitalstatus", age_namn = "a_alder", stratum_var_namn = "diagnos_grupp", relativ = (Relativ == "Relativ överlevnad"))  
  # Skapa titel
  Titel <- paste0(Relativ," per diagnos \n", Urval)
  # Skapa plot
  surv_plot(surv, legend = TRUE, main = Titel, CI = (CI == "Ja"))  
} 
   
  
  