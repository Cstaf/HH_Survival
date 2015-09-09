
############### Funktion för att testa vilken miljö vi sitter i ################
.is.inca <- function(){
  unname(!(Sys.info()["sysname"] == "Darwin"))
}


############# Identifiera working directory beroende på vem som kör skriptet #############

path <- if (.is.inca()) {
  "D:/R-Scripts/Väst/Oc5hoer/"
  } else if (Sys.info()["user"] == "erikbulow") {
  "~/Documents/huvud_hals/HH_Survival/"
}else {
  "~/Documents/Github/HH_Survival/" 
}
setwd(path)


############################ Läs in data om vi jobbar lokalt #############################

if (!.is.inca()) {
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

############################### Ladda funktioner ###############################
files <- c("fun_surv_est.R", "fun_surv_plot.R", "fun_popmort.R")
lapply(paste0(path, "funktioner/", files), source, encoding = "UTF-8")


############################ Laddning av parametrar ############################
if (!.is.inca()) {
  param <- list(
    Från     =  "2010",
    Till     =  "2013",
    Diagnos  =  "Samtliga diagnoser",
    Stadie   =  "Samtliga stadier",
    Stratum  =  "Per diagnos",
    Relativ  =  "Relativ överlevnad",
    CI       =  "Nej",
    Minålder =  "18",
    Maxålder =  "110"
  )
}
  
  
########################### Förberadande bearbetning ###########################
names(df) <- tolower(names(df))
df_HH <- df %>% 
  mutate(stadie_grupp             = stringr::str_trim(substring(as.character(a_stadium_beskrivning),1,3)),
         diagnos_grupp            = stringr::str_trim(gsub("[[:digit:]]","", a_icd10_gruppnamn))) %>% 
  filter(region_namn              != "Region Demo",
         stadie_grupp            %in% c("I", "II", "III", "IV", "-"),
         diagnos_grupp            != "",
         vitalstatusdatum_estimat != "",
         a_diadat         != "",
         !is.na(a_alder)  
         ) %>% 
  # Drop multiple tumours per patient
  arrange(a_diadat) %>% 
  distinct(pat_id)


############################ Filtrering på stadie #############################
if ("Samtliga stadier" %in% param$Stadie) {
  stadie_label <- "Samtliga stadier"
} else{
  stadie_label <- paste(param$Stadie, collapse = ", ")
  df_HH <- 
    df_HH %>% 
    filter(stadie_grupp %in% param$Stadie)
}

  
############################ Filtrering på diagnos #############################
if ("Samtliga diagnoser" %in% param$Diagnos) {
  diagnos_label <- "Samtliga diagnoser"
} else{
  diagnos_label <- paste(param$Diagnos, collapse = ", ")
  df_HH <- 
    df_HH %>% 
    filter(diagnos_grupp %in% param$Diagnos)
}



######################### Skapa text för valda urvalet #########################
urval_label <- paste0("(Urval: Diagnosår: ", param$Från,"-", param$Till, ", ",
                      "Diagnos: [", diagnos_label,"], ", 
                      "Stadie: [", stadie_label, "], ",
                      "Ålder: ", param$Minålder, "-", param$Maxålder, ")"
                )
huvudrubrik <- paste(param$Relativ, tolower(param$Stratum), "\n", urval_label)




####################### Skapa överlevnadsplot #######################

surv <- surv_est(df_HH, 
                 diagnosdatum_namn = "a_diadat", 
                 kön_namn = "kon_value", 
                 vitaldatum_namn = "vitalstatusdatum_estimat",
                 vitalstatus_namn = "vitalstatus", 
                 age_namn = "a_alder", 
                 relativ = param$Relativ == "Relativ överlevnad",
                 stratum_var_namn = switch(param$Stratum,
                                           "Aggregerat"  = "",
                                           "Per stadie"  = "stadie_grupp",
                                           "Per diagnos" = "diagnos_grupp"
                 )
        )

surv_plot(surv, main = huvudrubrik, CI = param$CI == "Ja")

 