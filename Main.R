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

############################### Ladda funktioner ###############################
if (!.is.inca()) source('~/Documents/Github/HH_Survival/fun_surv_est.R')
if (!.is.inca()) source('~/Documents/Github/HH_Survival/fun_surv_plot.R')
if (!.is.inca()) source('~/Documents/Github/HH_Survival/fun_popmort.R')

#### Här skall sourcning från R-servern in!!!!!!!!!!!!! 

  

########################### Förberadande bearbetning ###########################
names(df) <- tolower(names(df))


table(df$a_stadium_beskrivning)

