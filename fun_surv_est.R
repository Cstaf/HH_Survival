########################### POPMORT måste vara laddad ###########################
###### Se längst ner på sidan för exempel på formatet den skall laddas in  ######
#################################################################################


##### Variabel #####                                    ##### Namn på parameter som måste sättas #####                      #### Nödvändig format på variabeln ####
# - df                                                                                          Data-framen
# - Kön                                                       kön_namn = ""                                                               1=Man, 2=Kvinna
# - Diagnosdatum                                              diagnosdatum_namn = ""                                                      YYYYmmdd
# - Vitaldatum                                                vitaldatum_namn = ""                                                        YYYYmmdd
# - Vitalstatus                                               vitalstatus_namn = ""                                                       1=död, 2=lever,censorerad
# - Stratum_var                                               stratum_var_namn = ""
# - Age                                                       age_namn = ""
# - Stratum_var                                               stratum_var_namn                                                            Frivilig




surv_est <- function(df,
                     diagnosdatum_namn,
                     kön_namn,
                     vitaldatum_namn,
                     vitalstatus_namn,
                     stratum_var_namn = "",
                     age_namn = "",
                     namecheck = FALSE,
                     relativ = TRUE
)
{

#################################### Laddning av packet #####################################
suppressMessages(library(relsurv))
suppressMessages(library(plyr)) 

#############################################################################################




  df$diagnosdatum    <- df[[diagnosdatum_namn]]
  df$kön <- df[[kön_namn]]
  df$vitaldatum    <- df[[vitaldatum_namn]]
  df$vitalstatus <- df[[vitalstatus_namn]]
  df$age <- df[[age_namn]]
  if(stratum_var_namn != ""){
  df$stratum_var <- df[[stratum_var_namn]]
  }  

  surv <- df
  
surv$diagnosdatum <- gsub("-","",surv$diagnosdatum) 
surv$vitaldatum <- gsub("-","",surv$vitaldatum) 



  surv$vitalstatus <- ifelse(surv$vitalstatus ==1,1,0)
  surv$sex <- ifelse(surv$kön==2,"female","male")
  
  surv$survtime <- as.numeric(with(surv,as.Date(as.character(vitaldatum),format="%Y%m%d") - 
                                     as.Date(as.character(diagnosdatum),format="%Y%m%d")))
  
  surv$yeardag <- as.Date(as.character(surv$diagnosdatum),format="%Y%m%d") - as.Date("1960-01-01")
  
  surv <- surv[with(surv,!is.na(survtime) & survtime>=0),]
  
  
  
  if(stratum_var_namn == ""){
      # Relativ överlevnad
    if (relativ) {
      fit <- rs.surv(Surv(survtime,vitalstatus) ~ ratetable(age= age*365.24,sex=sex,year=yeardag),
                        data = surv,
                        ratetable=popmort,
                        method="ederer2")
    } else {
      # Observerad överlevnad
      fit <- survfit(Surv(survtime,vitalstatus) ~ 1, 
                     data = surv)   
    }
  }
  
  
  if(stratum_var_namn != ""){
    surv$stratum_var <- as.factor(surv$stratum_var)

    if (relativ) {
      surv$stratum_var <- factor(surv$stratum_var, levels=c(levels(surv$stratum_var)[1],rev(levels(surv$stratum_var))[1:nlevels(surv$stratum_var)-1]))
         fit <- rs.surv(Surv(survtime,vitalstatus) ~ stratum_var + ratetable(age= age*365.24,sex=sex,year=yeardag),
                        data = surv,
                        ratetable=popmort,
                        method="ederer2")
    } else {
      fit <- survfit(Surv(survtime,vitalstatus) ~ stratum_var, 
                     data = surv)     
    }
  if(namecheck == FALSE){
         a <- strsplit(names(fit$strata), ", stratum_var")
         a <- unlist(lapply(a, function(surv){
           surv <- surv[substring(surv, nchar(surv)) == 1]
           if (identical(surv, character(0))) NA else surv
         }))
         a <- gsub("=1", "", a)
         a <- gsub("stratum_var", "",a)
         ## The first level of fit is coded as all equals 0 and has to be taken back from the opne unused level of x$strata!
         a[is.na(a)] <- levels(surv$stratum_var)[!(levels(surv$stratum_var) %in% a)]
         names(fit$strata) <- a
  }
  }
  fit$time <- fit$time / 365.24
  ################################################################################
  
  
  
  
  return(fit)
  }


####################################### POPMORT ###############################################

# ratedata     <-      read.delim2("popmort19582013.txt",header=T)
# 
# men    <- matrix(type.convert(as.character(ratedata[ratedata$sex==1,"prob"])),
#                  nrow=max(ratedata$X_age)-min(ratedata$X_age)+1,
#                  dimnames=list(min(ratedata$X_age):max(ratedata$X_age),
#                                min(ratedata$X_year):max(ratedata$X_year))
# ) + 10^-10
# women  <- matrix(type.convert(as.character(ratedata[ratedata$sex==2,"prob"])),
#                  nrow=max(ratedata$X_age)-min(ratedata$X_age)+1,
#                  dimnames=list(min(ratedata$X_age):max(ratedata$X_age),
#                                min(ratedata$X_year):max(ratedata$X_year))
# ) + 10^-10
# 
# popmort  <- transrate(men=men, 
#                       women=women,
#                       yearlim=c(min(ratedata$X_year),max(ratedata$X_year)),
#                       int.length=1)                 




