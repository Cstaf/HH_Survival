
############### Funktion för att testa vilken miljö vi sitter i ################
.is.inca <- function(){
  unname(!(Sys.info()["sysname"] == "Darwin"))
}



############# Identifiera working directory beroende på vem som kör skriptet #############

path <- if (.is.inca()) {
  "~/Documents/Github/HH_Survival/" 
} else if (Sys.info()["user"] == "erikbulow") {
  "~/Documents/huvud_hals/HH_Survival/"
}else {
  "D:/R-Scripts/Väst/Oc5hoer/funktioner/"
}


############################ Läs in data om vi jobbar lokalt #############################

if (!.is.inca()) {
  setwd(path)
  # rm(list = setdiff(ls(), "path"))
  if (!file.exists("HH.rda")) {
    df <- read.csv2("HH.txt")
    save(df, file = "HH.rda")
  } else {
    load("HH.rda")
  }
}




################################# Ladda paket ##################################
library(dplyr)
library(relsurv)
library(ggplot2)
library(gdata)
library(stringr)


############################### Ladda funktioner ###############################
files <- c("fun_surv_est.R", "fun_surv_plot.R", "fun_popmort.R")
lapply(paste0(path, files), source, encoding = "UTF-8")


############################ Laddning av parametrar ############################
if (!.is.inca()) {
  param <- list(
    Från     =  2010,
    Till     =  2013,
    Diagnos  =  "Samtliga diagnoser",
    Stadie   =  "Samtliga stadier",
    Stratum  =  "Aggregerat",
    Relativ  =  "Relativ överlevnad",
    CI       =  "Nej",
    Minålder =  18,
    Maxålder =  110
  )
}
  
  
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
Urval <- with(param, paste0("(Urval: Diagnosår: ", Från,"-", Till,", Diagnos: ", 
                           Diagnos,", Stadie: ", Stadie, ", Ålder: ", Minålder, 
                           "-",Maxålder, ")"
          ))








####################### Aggregerad presentation eller ej #######################
if (("Aggregerat" == Stratum)) {
  surv <- surv_est(df_HH, diagnosdatum_namn = "a_diadat", kön_namn = "kon_value", vitaldatum_namn = "vitalstatusdatum_estimat",
                   vitalstatus_namn = "vitalstatus", age_namn = "a_alder", relativ = param$Relativ == "Relativ överlevnad")
  # Skapa titel
  Titel <- paste0(Relativ,"\n", Urval)  # Skapa plot
  surv_plot(surv, main = Titel, CI = param$CI == "Ja")
  
  
} else if ("Per stadie" == Stratum) {
  surv <- surv_est(df_HH, diagnosdatum_namn = "a_diadat", kön_namn = "kon_value", vitaldatum_namn = "vitalstatusdatum_estimat",
                   vitalstatus_namn = "vitalstatus", age_namn = "a_alder", stratum_var_namn = "stadie_grupp", relativ = param$Relativ == "Relativ överlevnad")
  # Skapa titel
  Titel <- paste0(Relativ," per stadium \n", Urval)
  # Skapa plot
  surv_plot(surv, legend = TRUE, main = Titel, CI = param$CI == "Ja")
} else {
  surv <- surv_est(df_HH, diagnosdatum_namn = "a_diadat", kön_namn = "kon_value", vitaldatum_namn = "vitalstatusdatum_estimat",
                   vitalstatus_namn = "vitalstatus", age_namn = "a_alder", stratum_var_namn = "diagnos_grupp", relativ = param$Relativ == "Relativ överlevnad")
  # Skapa titel
  Titel <- paste0(Relativ," per diagnos \n", Urval)
  # Skapa plot
  surv_plot(surv, legend = TRUE, main = Titel, CI = param$CI == "Ja")
} 
   
  
  