
############### Funktion för att testa vilken miljö vi sitter i ################
.is.inca <- function(){
  unname(!(Sys.info()["sysname"] == "Darwin"))
}


############# Identifiera working directory beroende på vem som kör skriptet #############

path <- if (.is.inca()) {
  "D:/R-Scripts/Väst/oc5hoer/"
  } else if (Sys.info()["user"] == "erikbulow") {
  "~/Documents/huvud_hals/HH_Survival/"
}else {
  "~/Documents/Github/HH_Survival/"
}

if (!(.is.inca())){
  setwd(path)
}

############################ Läs in data om vi jobbar lokalt #############################

if (!.is.inca()) {
  # rm(list = setdiff(ls(), "path"))
  if (!file.exists("HH.rda")) {
    df <- read.csv("HH.txt")
    save(df, file = "HH.rda")
  } else {
    load("HH.rda")
  }
}


################################# Ladda paket ##################################
library(dplyr)
library(gdata)
library(stringr)
library(jsonlite)

############################### Ladda funktioner ###############################
files <- c("fun_surv_est.R",  "fun_popmort.R")
lapply(paste0(path, "funktioner/", files), source, encoding = "UTF-8")


############################ Laddning av parametrar ############################
if (!.is.inca()) {
  param <- list(
    Från     =  "2010",
    Till     =  "2013",
    Diagnos  =  "Samtliga diagnoser",
    Stadie   =  "Samtliga stadier",
    Stratum  =  "Per region",
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
         diagnos_grupp            = stringr::str_trim(gsub("[[:digit:]]","", a_icd10_gruppnamn)),
         stadie_grupp             = ifelse(stadie_grupp == "", "-", as.character(stadie_grupp))) %>% 
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
                                           "Per region"  = "region_namn",
                                           "Per stadie"  = "stadie_grupp",
                                           "Per diagnos" = "diagnos_grupp"
                 )
        )




########################### Aggregerad presentation ############################
if (("Aggregerat" == param$Stratum)) {
  x <- with(summary(surv), data.frame(x = time, y = surv, low = lower, up = upper, risk = n.risk))
  x <- x %>% 
    mutate(y = round(y,3)*100, 
           x = round(x,2),
           ci = paste0("(",round(low,3)*100, "% - ", round(up,3)*100, "%)" )) %>% 
    filter(!(x > 5.1))
  
  x <- paste0("{x:", x$x, ",y:", x$y, ",ci:'",x$ci, "',nrisk:", x$risk ,"}")
  ser <- paste("var","ser","= [{ data:",toJSON(x), "}];")
  ser <- gsub('\\"\\{', '{', ser  )
  ser <- gsub('\\}\\"', '}', ser  )
  # Skapa titel
  titel <- paste0(param$Relativ)
  # Legend
  legend <- paste0("var legend = ","false" ,";")
  # Header format
  headerFormat <- paste0("var headerFormat = '", "';")
  
  
  ############################# Stadie presentation ##############################
  
} else if ("Per stadie" == param$Stratum) {
  x <- with(summary(surv), data.frame(name = strata, x = time, y = surv, low = lower, up = upper, risk = n.risk))
  x <- x %>% 
    mutate(y = round(y,3)*100, 
           x = round(x,2),
           ci = paste0("(",round(low,3)*100, "% - ", round(up,3)*100, "%)" ),
           data = paste0("{x:", x, ",y:", y, ",ci:'",ci, "',nrisk:", risk ,"}")) %>%
    filter(!(x > 5.1)) %>% 
    select(name, data) %>%
    group_by(name) %>% 
    do(data = paste(.$data, collapse = ","))
  
  ser <- paste("var","ser","=",toJSON(x), ";")
  ser <- gsub('\\"\\{', '{', ser  )
  ser <- gsub('\\}\\"', '}', ser  )  
  # Skapa titel
  titel <- paste0(param$Relativ, " per stadie")
  # Legend
  legend <- paste0("var legend = ","true" ,";")
  # Header format
  headerFormat <- paste0("var headerFormat = '<span style=color:{series.color}><b>Stadie: {series.name}</b></span><br/>';")

  
  ## Region presentation (Kan eventuellt tages bort vid notifikation från Erik) ##
} else if ("Per region" == param$Stratum) {
  x <- with(summary(surv), data.frame(name = strata, x = time, y = surv, low = lower, up = upper, risk = n.risk))
  x <- x %>% 
    mutate(y = round(y,3)*100, 
           x = round(x,2),
           ci = paste0("(",round(low,3)*100, "% - ", round(up,3)*100, "%)" ),
           data = paste0("{x:", x, ",y:", y, ",ci:'",ci, "',nrisk:", risk ,"}")) %>%
    filter(!(x > 5.1)) %>% 
    select(name, data) %>%
    group_by(name) %>% 
    do(data = paste(.$data, collapse = ","))
  
  ser <- paste("var","ser","=",toJSON(x), ";")
  ser <- gsub('\\"\\{', '{', ser  )
  ser <- gsub('\\}\\"', '}', ser  )  
  # Skapa titel
  titel <- paste0(param$Relativ, " per region")
  # Legend
  legend <- paste0("var legend = ","true" ,";")
  # Header format
  headerFormat <- paste0("var headerFormat = '<span style=color:{series.color}><b>{series.name}</b></span><br/>';")
  
  
  
  ############################# Diagnos presentation #############################
  
} else {
  x <- with(summary(surv), data.frame(name = strata, x = time, y = surv, low = lower, up = upper, risk = n.risk))
  x <- x %>% 
    mutate(y = round(y,3)*100, 
           x = round(x,2),
           ci = paste0("(",round(low,3)*100, "% - ", round(up,3)*100, "%)" ),
           data = paste0("{x:", x, ",y:", y, ",ci:'",ci, "',nrisk:", risk ,"}")) %>%
    filter(!(x > 5.1)) %>%
    select(name, data) %>%
    group_by(name) %>% 
    do(data = paste(.$data, collapse = ","))
  
  ser <- paste("var","ser","=",toJSON(x), ";")
  ser <- gsub('\\"\\{', '{', ser  )
  ser <- gsub('\\}\\"', '}', ser  )  
  # Skapa titel
  titel <- paste0(param$Relativ, " per diagnos")
  # Legend
  legend <- paste0("var legend = ","true" ,";")
  # Header format
  headerFormat <- paste0("var headerFormat = '<span style=color:{series.color}><b>{series.name}</b></span><br/>';")
  
}


################################################################################
#                                                                              #
#               Här sätter vi globala parametrar för Highcharts                #
#                                                                              #
################################################################################

# Sätt width, height och marginbottom
height <- paste0("var height = ","700", ";")
width <- paste0("var width = ", "900" ,";")
marginBottom <- paste0("var marginBottom = ","120",";")

# Skapa titel
titel <- paste("var titel =", "'",titel,"'", ";")
subtitel <- paste("var subtitel =", "'",urval_label,"'", ";")






############################## Inladdning av del1 ##############################
if (!(.is.inca())) {
  # Ladda in del1
  del1 <- scan("del1_v2(offline).txt", what="", sep="\n", quiet=TRUE,encoding="UTF-8")
} else {
  del1 <- scan("D:/R-Scripts/Väst/oc5hoer/highcharts/överlevnad/del1_v2.txt", what="", sep="\n", quiet=TRUE,encoding="UTF-8")
}





outfile <- file("./output.html","w",encoding="UTF-8")
cat(paste("\n",del1) ,file=outfile,append=TRUE)
cat(paste("\n","<script>") ,file=outfile,append=TRUE)
cat(paste("\n",ser) ,file=outfile,append=TRUE)
cat(paste("\n",titel) ,file=outfile,append=TRUE)
cat(paste("\n",subtitel) ,file=outfile,append=TRUE)
cat(paste("\n",height) ,file=outfile,append=TRUE)
cat(paste("\n",width) ,file=outfile,append=TRUE)
cat(paste("\n",marginBottom) ,file=outfile,append=TRUE)
cat(paste("\n",legend) ,file=outfile,append=TRUE)
cat(paste("\n",headerFormat) ,file=outfile,append=TRUE)
cat(paste("\n","Survdiagram('container', titel, subtitel, height, width, marginBottom, legend ,ser);
          ") ,file=outfile,append=TRUE)
cat(paste("\n","</script> </body> </html>") ,file=outfile,append=TRUE)
close(outfile)




 